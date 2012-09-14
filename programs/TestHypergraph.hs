{-# LANGUAGE BangPatterns, RankNTypes #-}
module Main where

import Codec.Compression.GZip ( decompress )

import Control.Monad.ST
import qualified Data.Map as M
import Control.DeepSeq ( rnf, force, NFData(..) )
import Control.Seq ( using, rseq, r0, seqTuple2, seqList )
import qualified Data.Array as A
import qualified Data.Array.IArray as IA
import qualified Data.Array.MArray as MA
import qualified Data.Array.ST as STA
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import Data.Int ( Int32 )
import qualified Data.Set as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Debug.Trace
import System.Environment ( getArgs )

import qualified Vanda.Algorithms.Earley.WSA as WSA
import Vanda.Algorithms.EarleyMonadic ( earley )
import Vanda.Features
import Vanda.Functions ( GHKM, toWSAmap )
import Vanda.Hypergraph hiding ( knuth, dropNonproducing )
import Vanda.Hypergraph.Binary ()
import Vanda.Hypergraph.EdgeList ( knuth, dropNonproducing', dropNonproducing )
import Vanda.Hypergraph.NFData ()
import Vanda.Token

{-
instance (Show v, Show l, Show i) => Show (Hyperedge v l i) where
  show e
    = show (to e)
      ++ " -> "
      ++ show (label e)
      ++ " "
      ++ unwords (map show $ from e)
      ++ " # "
      ++ show (i e)

instance (Show v, Show l, Show i) => Show (Candidate v l i x) where
  show (Candidate w d _) = show w ++ " -- " ++ show d
-}

compute :: [Hyperedge v l i] -> [Int]
compute _es =
  let
    -- a :: [Hyperedge v l i] -> (forall s0. ST s0 (STA.STUArray s0 Int Int))
    -- a :: forall s0. ST s0 (STA.STUArray s0 Int Int)
    a es = do
      -- test <- newSTRef (0 :: Int)
      ar <- MA.newArray (0, 1000) 0 :: ST s (STA.STUArray s Int Int) 
      sequence_ $ map (ac ar . arity) es
      return ar
    -- ac :: STA.STUArray s Int Int -> Int -> ST s ()
    ac ar i = do
      n :: Int <- MA.readArray ar i
      let n' = n + 1
      n' `seq` MA.writeArray ar i n'
  -- in ((IA.elems . STA.runSTUArray) :: (forall s. ST s (STA.STUArray s Int Int)) -> [Int]) $ a
  in IA.elems $ STA.runSTUArray $ a _es

instance Ord l => Ord (T.Tree l) where
  T.Node l1 ts1 `compare` T.Node l2 ts2
    = case (l1 `compare` l2, ts1 `compare` ts2) of
        (LT, _) -> LT
        (EQ, LT) -> LT
        (EQ, EQ) -> EQ
        _ -> GT


makeItSo
  :: TokenArray -> TokenArray -> Candidate v Token i x -> String
makeItSo tok nodes (Candidate w d _)
  = show w ++ " -- " ++ makeItSo' tok nodes d
makeItSo'
  :: TokenArray -> TokenArray -> Derivation v Token i -> String
makeItSo' tok _ (T.Node e [])
  = getString tok (label e)
makeItSo' tok nodes (T.Node e es)
  = "(" ++ getString tok (label e) ++ " "
    ++ unwords (map (makeItSo' tok nodes) es)
    ++ ")"


class PrintState q where
  printState :: TokenArray -> q -> String


instance PrintState Token where
  printState na q = getString na q


instance PrintState q => PrintState (Int, q, Int) where
  printState na (p, q, p')
    = "(" ++ show p ++ ", " ++ printState na q ++ ", " ++ show p' ++ ")"


printRule
  :: (PrintState q, Show i)
  => TokenArray
  -> TokenArray
  -> Hyperedge q Token i
  -> String
printRule ta na e
  = printState na (to e)
    ++ " -> " ++ getString ta (label e)
    ++ "(" ++ unwords (map (printState na) (from e)) ++ ")"
    ++ " # " ++ (show (ident e))


getTerminals :: Ord t => WSA.WSA Int t Double -> S.Set t
getTerminals = S.fromList . map WSA.transTerminal . WSA.transitions

bin = [Left 0, Left 1]

una = [Left 0]

memo :: TokenArray -> Int -> [Either Int Int]
memo ta = l `seq` (a A.!)
  where
    b = getBounds ta
    a = A.array b l
    l = [ (i, c (getString ta i) i) | i <- A.range b ]
        `using` seqList (seqTuple2 r0 (seqList rseq))
    c s i = case reverse s of
              '2':'/':ch:_
                | ch /= '\\' -> bin
                | True -> [Right i]
              '1':'/':ch:_
                | ch /= '\\' -> una
                | True -> [Right i]
              _ -> [Right i]

instance (NFData v, NFData l, NFData i) => NFData (Candidate v l i x) where
  rnf (Candidate w d _) = rnf w `seq` rnf d

main :: IO ()
main = do 
  args <- getArgs
  case args of
    {- [] -> do
      let he = head (B.decode (B.encode (take 500000 (repeat (mkHyperedge (1::Int) [1] (1::Int) (1::Int)))))) :: Hyperedge Int Int Int
      print he -}
    ["-b", bhgFile] -> do
      el :: EdgeList Token (GHKM Token) Int
        <- fmap
           ( B.decode
           . decompress
           )
           $ B.readFile (bhgFile ++ ".bhg.gz")
      let s1 = S.fromList . map label . edges $ el
      let s2 = S.fromList . map (fst . label) . edges $ el
      let s3 = S.fromList . map (snd . label) . edges $ el
      putStr
        $ "Kanten: " ++ (show (length (edges el))) ++ "; "
          ++ "Labels: " ++ (show (S.size s1)) ++ "; "
          ++ "Bäume: " ++ (show (S.size s2)) ++ "; "
          ++ "Strings: " ++ (show (S.size s3)) ++ "\n"
    ["-b", bhgFile, "-s", statFile] -> do
      el :: EdgeList Token (GHKM Token) Int
        <- fmap
           ( B.decode
           . decompress
           )
           $ B.readFile (bhgFile ++ ".bhg.gz")
      -- stat :: [Int]
      let stat = compute (edges el)
      T.writeFile statFile $ T.unlines $ map (T.pack . show) $ stat
    ["-z", zhgFile, "-t", tokFile, "--intersect"] -> do
      el :: EdgeList Int Int Int
        <- fmap (B.decode . decompress) (B.readFile zhgFile)
      tok :: TokenArray
        <- fmap fromText $ T.readFile tokFile
      let s2 = S.fromList [ label e | e@Binary{} <- edges el ]
      let s1 = S.fromList [ label e | e@Unary{} <- edges el ]
      let s0 = S.fromList [ label e | e@Nullary{} <- edges el ]
      putStr (unwords (map (getString tok) (S.toList (s0 `S.intersection` (s1 `S.union` s2)))))
    ["-z", zhgFile, "-t", tokFile] -> do
      el :: EdgeList Int Int Int
        <- fmap (B.decode . decompress) (B.readFile zhgFile)
      weights :: VU.Vector Double
        <- fmap (VU.fromList . B.decode . decompress)
           $ B.readFile (zhgFile ++ ".weights.gz")
      tm :: TokenMap
        <- fmap fromText $ T.readFile tokFile
      ta :: TokenArray
        <- fmap fromText $ T.readFile tokFile
      na :: TokenArray
        <- fmap fromText $ T.readFile (zhgFile ++ ".nodes")
      let pN !_ !i xs
            = (weights VU.! fromIntegral (fst i)) * Prelude.product xs
          -- wsa = toWSAmap tm "those were the days"
          wsa = toWSAmap tm "days days days days days days" -- ""
          ts = getTerminals wsa
          el' = EdgeList (nodesEL el) (filter p $ edgesEL el)
          p e = weights VU.! (ident e) > 1.0e-10 &&
                case e of
                  Nullary{} -> S.member (label e) ts
                  _ -> True
          h = dropNonproducing ({- dropNonproducing' -} el')
          (h', _) = earley h (memo ta) wsa 1132
          init
            = ( fst . head . WSA.initialWeights $ wsa
              , 1132
              , fst . head . WSA.finalWeights $ wsa
              )
          dafuq
            = take 1
            $ (M.! init)
            $ knuth h' (Feature pN V.singleton) (V.singleton 1)
      weights `seq` rnf el `seq` rnf dafuq `seq` putStr "Nice."
      putStr
        $ unlines
        $ map (makeItSo ta undefined) -- nodes
        $ dafuq
      {- T.writeFile (zhgFile ++ ".reduce")
        (T.unlines (map (T.pack . printRule ta na) (edges h)))
      T.writeFile (zhgFile ++ ".intersect")
        (T.unlines (map (T.pack . printRule ta na) (edges h')))-}
    _ -> error "Usage: TestHypergraph -z zhgFile -t tokenFile"

