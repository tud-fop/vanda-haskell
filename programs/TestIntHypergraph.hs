{-# LANGUAGE BangPatterns, RankNTypes #-}
module Main where

import Codec.Compression.GZip ( decompress )

import Control.Arrow ( (&&&) )
import Control.DeepSeq ( rnf, deepseq, NFData(..) )
import Control.Seq ( using, rseq, r0, seqTuple2, seqList )
import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Tree as T
import qualified Data.Vector.Unboxed as VU
-- import Debug.Trace
import System.Environment ( getArgs )

import qualified Vanda.Algorithms.Earley.WSA as WSA
import Vanda.Algorithms.IntEarley ( earley, NTT (..) )
import Vanda.Functions ( toWSAmap )
import Vanda.Hypergraph.IntHypergraph
import Vanda.Hypergraph.IntBinary ()
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

getString2 :: IM.IntMap (Int, Token, Int) -> TokenArray -> Int -> String
getString2 s2n na i
  = case s2n IM.! i of
      (p, q, p') -> "(" ++ show p ++
                    ", " ++ getString na q ++
                    ", " ++ show p' ++
                    ")"

printRule
  :: (Show i)
  => TokenArray
  -> (Token -> String)
  -> Hyperedge Token i
  -> String
printRule ta na e
  = na (to e)
    ++ " -> " ++ getString ta (label e)
    ++ "(" ++ unwords (map na (from e)) ++ ")"
    ++ " # " ++ (show (ident e))


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

memo2 :: TokenMap -> TokenArray -> Int -> Int
memo2 nm na = (a A.!)
  where
    b = getBounds na
    a = A.array b
      $ [ (i, getToken nm (takeWhile (/= '_') (getString na i) ++ "_0"))
        | i <- A.range b
        ]

instance (NFData l, NFData i) => NFData (Hyperedge l i) where
  rnf (Nullary _ l i) = rnf l `seq` rnf i
  rnf (Unary _ _ l i) = rnf l `seq` rnf i
  rnf (Binary _ _ _ l i) = rnf l `seq` rnf i
  rnf (Hyperedge _ f l i) = rnf (VU.toList f) `seq` rnf l `seq` rnf i

instance (NFData l, NFData i) => NFData (Candidate l i) where
  rnf (Candidate w d) = rnf w `seq` rnf d

p0 <&> p1 = \ x -> p0 x && p1 x

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
      hg0 :: Hypergraph Int Int
        <- fmap (B.decode . decompress) (B.readFile zhgFile)
      weights :: VU.Vector Double
        <- fmap (VU.fromList . B.decode . decompress)
           $ B.readFile (zhgFile ++ ".weights.gz")
      tm :: TokenMap
        <- fmap fromText $ T.readFile tokFile
      ta :: TokenArray
        <- fmap fromText $ T.readFile tokFile
      nm :: TokenMap
        <- fmap fromText $ T.readFile (zhgFile ++ ".nodes")
      na :: TokenArray
        <- fmap fromText $ T.readFile (zhgFile ++ ".nodes")
      let wsa = toWSAmap tm "those were the days"
          -- wsa = toWSAmap tm "days days days days days days days days" -- ""
          {-
          els = L.group $ map (mapHE (memo2 nm na)) (edges hg0)
          avg :: [Double] -> Double
          avg xs = sum xs / fromIntegral (length xs)
          wt2 = IM.fromList
                  (map (ident . head &&& avg . map ((weights VU.!) . ident))
                    els)
          (hg, wt) = (mkHypergraph $ map head els, (wt2 IM.!))
          -}
          (hg, wt) = (hg0, (weights VU.!))
          p0 e = wt (ident e) > 1.0e-10
          p1 e = case e of
                   Nullary{} -> S.member (label e) ts
                   _ -> True
                 where ts = getTerminals wsa
          p = p0 <&> p1
          rhg0 = Hypergraph (nodes hg) (filter p $ edges hg)
          rhg1 = dropNonproducing ({- dropNonproducing' -} rhg0)
          rhg = rhg1
          (s2n, hg', _) = earley rhg (memo ta) wsa fst 1132
          n2s = IM.fromList $ map (snd &&& fst) $ M.toList s2n
          v0 = (0, 1132, fst . head . WSA.finalWeights $ wsa)
          dafuq
            = take 1
            $ (A.! (s2n M.! v0))
            $ knuth hg' (\ _ !i xs -> wt i * Prelude.product xs)
      weights `seq` putStrLn "Gewichte geladen... "
      edges hg0 `seq` putStrLn "Hypergraph geladen... "
      -- wt2 `seq` putStrLn "Nice."
      -- rnf dafuq `seq` putStrLn "Nice."
      putStrLn $ unlines $ map (makeItSo ta undefined) $ dafuq
      {-
      T.writeFile (zhgFile ++ ".reduce")
        (T.unlines (map (T.pack . printRule ta (getString na)) (edges rhg)))
      T.writeFile (zhgFile ++ ".intersect")
        (T.unlines (map (T.pack . printRule ta (getString2 n2s na)) (edges hg')))
      -}
    _ -> error "Usage: TestHypergraph -z zhgFile -t tokenFile"

