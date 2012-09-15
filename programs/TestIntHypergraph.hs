{-# LANGUAGE BangPatterns, RankNTypes #-}
module Main where

import Codec.Compression.GZip ( decompress )

import Control.DeepSeq ( rnf, deepseq, NFData(..) )
import Control.Seq ( using, rseq, r0, seqTuple2, seqList )
import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy.IO as T
import qualified Data.Tree as T
import qualified Data.Vector.Unboxed as VU
-- import Debug.Trace
import System.Environment ( getArgs )

import qualified Vanda.Algorithms.Earley.WSA as WSA
import Vanda.Algorithms.IntEarley ( earley, NTT (..) )
import Vanda.Functions ( toWSAmap )
import Vanda.Hypergraph.IntHypergraph
import Vanda.Token


makeItSo
  :: TokenArray -> TokenArray -> Candidate Token i -> String
makeItSo tok nm (Candidate w d)
  = show w ++ " -- " ++ makeItSo' tok nm d
makeItSo'
  :: TokenArray -> TokenArray -> Derivation Token i -> String
makeItSo' tok _ (T.Node e [])
  = getString tok (label e)
makeItSo' tok nm (T.Node e es)
  = "(" ++ getString tok (label e) ++ " "
    ++ unwords (map (makeItSo' tok nm) es)
    ++ ")"


{- printRule
  :: (Show i)
  => TokenArray
  -> TokenArray
  -> Hyperedge Token i
  -> String
printRule ta na e
  = getString na (to e)
    ++ " -> " ++ getString ta (label e)
    ++ "(" ++ unwords (map (getString na) (from e)) ++ ")"
    ++ " # " ++ (show (ident e)) -}


getTerminals :: Ord t => WSA.WSA Int t Double -> S.Set t
getTerminals = S.fromList . map WSA.transTerminal . WSA.transitions

memo :: TokenArray -> Int -> [NTT]
memo ta = l `seq` (a A.!)
  where
    b = getBounds ta
    a = A.array b l
    l = [ (i, c (getString ta i) i) | i <- A.range b ]
        `using` seqList (seqTuple2 r0 (seqList rseq))
    c s i = case reverse s of
              '2':'/':ch:_
                | ch /= '\\' -> bin
                | True -> [T i]
              '1':'/':ch:_
                | ch /= '\\' -> una
                | True -> [T i]
              _ -> [T i]
    bin :: [NTT]
    bin = [NT 0, NT 1]
    una :: [NTT]
    una = [NT 0]

instance (NFData l, NFData i) => NFData (Hyperedge l i) where
  rnf (Nullary _ l i) = rnf l `seq` rnf i
  rnf (Unary _ _ l i) = rnf l `seq` rnf i
  rnf (Binary _ _ _ l i) = rnf l `seq` rnf i
  rnf (Hyperedge _ f l i) = rnf (VU.toList f) `seq` rnf l `seq` rnf i

instance (NFData l, NFData i) => NFData (Candidate l i) where
  rnf (Candidate w d) = rnf w `seq` rnf d

instance (NFData l, NFData i, B.Binary l, B.Binary i)
  => B.Binary (Hyperedge l i) where
  put e = do
    B.put (to e)
    B.put (from e)
    B.put (label e)
    B.put (ident e)
  get = do
    x1 <- B.get
    x2 <- x1 `deepseq` B.get
    x3 <- x2 `deepseq` B.get
    x4 <- x3 `deepseq` B.get
    x4 `deepseq` return $! mkHyperedge x1 x2 x3 x4
     

instance (NFData l, NFData i, B.Binary l, B.Binary i)
  => B.Binary (Hypergraph l i) where
  put (Hypergraph vs es) = do
    B.put (S.fromList $ enumFromTo 0 $ vs - 1)
    B.put es -- myPut es
  get = do
    vs <- fmap ((+ 1) . snd . nodesL . S.toList) (B.get :: B.Get (S.Set Int))
    es <- B.get
    return (Hypergraph vs es)



main :: IO ()
main = do 
  args <- getArgs
  case args of
    ["-z", zhgFile, "-t", tokFile, "--intersect"] -> do
      el :: Hypergraph Int Int
        <- fmap (B.decode . decompress) (B.readFile zhgFile)
      tok :: TokenArray
        <- fmap fromText $ T.readFile tokFile
      let s2 = S.fromList [ label e | e@Binary{} <- edges el ]
      let s1 = S.fromList [ label e | e@Unary{} <- edges el ]
      let s0 = S.fromList [ label e | e@Nullary{} <- edges el ]
      putStr (unwords (map (getString tok) (S.toList (s0 `S.intersection` (s1 `S.union` s2)))))
    ["-z", zhgFile, "-t", tokFile] -> do
      el :: Hypergraph Int Int
        <- fmap (B.decode . decompress) (B.readFile zhgFile)
      weights :: VU.Vector Double
        <- fmap (VU.fromList . B.decode . decompress)
           $ B.readFile (zhgFile ++ ".weights.gz")
      tm :: TokenMap
        <- fmap fromText $ T.readFile tokFile
      ta :: TokenArray
        <- fmap fromText $ T.readFile tokFile
      {- na :: TokenArray
        <- fmap fromText $ T.readFile (zhgFile ++ ".nodes") -}
      let pN !_ !i xs
            = (weights VU.! fromIntegral (fst i)) * Prelude.product xs
          wsa = toWSAmap tm "those were the days"
          -- wsa = toWSAmap tm "days days days days days days days days" -- ""
          ts = getTerminals wsa
          el' = Hypergraph (nodes el) (map snd $ filter p $ zip [0..] $ edges el)
          p (i, e) = i `mod` 2 == 0 && weights VU.! (ident e) > 1.0e-10 {-&&
                case e of
                  Nullary{} -> S.member (label e) ts
                  _ -> True-}
          h = dropNonproducing ({- dropNonproducing' -} el')
          (s2n, h', _) = earley h (memo ta) wsa 1132
          init
            = ( fst . head . WSA.initialWeights $ wsa
              , 1132
              , fst . head . WSA.finalWeights $ wsa
              )
          dafuq
            = take 1
            $ (IM.! (s2n M.! init))
            $ knuth h' pN
      weights `seq` putStrLn "Gewichte geladen... "
      rnf (edges el) `seq` putStrLn "Hypergraph geladen... "
      rnf dafuq `seq` putStrLn "Nice."
      putStr
        $ unlines
        $ map (makeItSo ta undefined) -- nodes
        $ dafuq
      {- T.writeFile (zhgFile ++ ".reduce")
        (T.unlines (map (T.pack . printRule ta na) (edges h)))
      T.writeFile (zhgFile ++ ".intersect")
        (T.unlines (map (T.pack . printRule ta na) (edges h')))-}
    _ -> error "Usage: TestHypergraph -z zhgFile -t tokenFile"

