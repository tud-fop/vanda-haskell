{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CBSM.Main
-- Copyright   :  (c) Technische Universität Dresden 2014
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.CBSM.Main where


import qualified Data.RevMap as RM
import           Vanda.Algorithms.EarleyMonadic
import qualified Vanda.Algorithms.Earley.WSA as WSA
import           Vanda.CBSM.CountBasedStateMerging
import           Vanda.Corpus.Penn.Simple as Penn
import qualified Vanda.Features as F
import qualified Vanda.Hypergraph as H
import           Vanda.Util.Tree as T

import           Control.Applicative ((<$>))
import           Control.Arrow ((***))
import           Control.Monad
import qualified Data.Binary as B
import qualified Data.Map as M
import           Data.Ord
import qualified Data.Set as S
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import           Data.Tree
import qualified Data.Vector as V
import           System.Console.CmdArgs
import           Text.Parsec.String (parseFromFile)


data Args
  = CBSM
    { argIterations :: Int
    , argCorpus :: FilePath
    , argDefoliate :: Bool
    , argGrammar :: FilePath
    }
  | Parse
    { argCount :: Int
    , argGrammar :: FilePath
    }
  | Bests
    { argCount :: Int
    , argGrammar :: FilePath
    }
  deriving (Data, Show, Typeable)

argModes :: [Args]
argModes
  = [ CBSM
      { argIterations = 0 &= argPos 0 &= typ "ITERATIONS"
      , argCorpus = def &= argPos 1 &= typ "TREEBANK"
      , argDefoliate = False &= explicit &= name "defoliate"
          &= help "remove leafs from treebank trees, e.g. to just deal \
                  \with preterminals"
      , argGrammar = def &= explicit &= name "output" &= typ "GRAMMAR-FILE"
          &= help "write binary representation of grammar to this file \
                  \instead of pretty printing to stdout"
      } &= details
      [ "Read-off a grammar from TREEBANK and generalize it."
      ]
    , Parse
      { argCount = 1 &= argPos 2 &= typ "COUNT"
      , argGrammar = def &= argPos 3 &= typ "GRAMMAR-FILE"
      } &= details
      [ "Parse newline-separated sentences from standard input."
      ]
    , Bests
      { argCount = 1 &= argPos 4 &= typ "COUNT"
      , argGrammar = def &= argPos 5 &= typ "GRAMMAR-FILE"
      } &= details
      [ "View best trees of a grammar."
      ]
    ]
  &= helpArg [name "h"]
  &= versionArg [ignore]
  &= program "command"
  &= summary "Count-Based State Merging"


type BinaryCRTG = CRTG Int String


main :: IO ()
main = do
  arguments <- cmdArgs (modes argModes)
  print arguments
  case arguments of

    CBSM{..}
       -> ( if null argGrammar
            then print
            else B.encodeFile argGrammar :: BinaryCRTG -> IO ()
          )
        . (!! argIterations)
        . cbsm
        . forestToGrammar
        . map (if argDefoliate then T.defoliate else id)
        . map Penn.toTree
        . either (error . show) id
      =<< parseFromFile Penn.p_penn argCorpus

    Parse{..} -> do
      (hg, inis) <- toHypergraph <$> (B.decodeFile argGrammar :: IO BinaryCRTG)
      let comp e | a == 0    = [Right (H.label e)]
                 | otherwise = map Left [0 .. a - 1]
            where a = H.arity e
      let feature = F.Feature (\ _ (i, _) xs -> i * product xs) V.singleton
      sents <- map words . lines <$> getContents
      forM_ sents $ \ sent -> do
        let (hg', weights') = earley' (asBackwardStar hg) comp (WSA.fromList 1 sent) (M.keys inis)
        let inis' = M.mapKeys (\ k -> (0, k, length sent)) inis
        printWeightedDerivations
          $ take argCount
          $ bestsIni hg' feature (V.singleton 1) inis'

    Bests{..} -> do
      (hg, inis) <- toHypergraph <$> (B.decodeFile argGrammar :: IO BinaryCRTG)
      let feature = F.Feature (\ _ i xs -> i * product xs) V.singleton
      printWeightedDerivations
        $ take argCount
        $ bestsIni (asBackwardStar hg) feature (V.singleton 1) inis

  where
    printWeightedDerivations xs =
      forM_ (zip [1 :: Int ..] xs) $ \ (i, (w, t)) -> do
        let t' = fmap H.label t
        putStrLn $ show i ++ ": " ++ show w
        putStrLn $ unwords $ yield t'
        putStrLn
          $ drawTree' (drawstyleCompact2 0 "")
          $ mapLeafs (\ cs -> "\ESC[93m" ++ cs ++ "\ESC[m") t'  -- colorize


-- bests :: (Ord v, Eq l) => CRTG v l -> [(Double, H.Derivation v l Double)]
bestsIni hg feat wV inis
  = mergesBy (comparing (Down . fst))
  $ M.elems
  $ M.intersectionWith (\ w' -> map (\ (F.Candidate w d _) -> (w' * w, d))) inis
  $ H.bests hg feat wV
  where

    -- | Merge sorted lists to a single sorted list.
    mergesBy :: (a -> a -> Ordering) -> [[a]] -> [a]
    mergesBy cmp = foldl (mergeBy cmp) []


    -- | Merge two sorted lists to a single sorted list.
    mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
    mergeBy cmp xs@(x:xs') ys@(y:ys')
      = case x `cmp` y of
          GT ->  y : mergeBy cmp xs  ys'
          _  ->  x : mergeBy cmp xs' ys
    mergeBy _ [] ys = ys
    mergeBy _ xs [] = xs


test3 :: (Show l, Ord l) => Int -> [Tree l] -> IO ()
test3 n
  = putStr
  . unlines
  . concatMap (\ (w, d) -> [show w, drawTree' (drawstyleCompact2 0 "") $ fmap (show . H.label) d])
  . bests
  . (!! n)
  . iterate cbsmStep2
  . forestToGrammar


test2 :: (Show l, Ord l) => Int -> [Tree l] -> IO ()
test2 n
  = putStr
  . unlines
  . map (unlines . map show . H.edges . asEdgeList . fst . toHypergraph)
  . take n
  . iterate cbsmStep2
  . forestToGrammar


test1 :: (Show l, Ord l) => Int -> [Tree l] -> IO ()
test1 n
  = putStr
  . unlines
  . map (uncurry (++) . ((unlines . map show . H.edges . asEdgeList . fst . toHypergraph) *** (unlines . map showStep1)))
  . take n
  . tail
  . iterate step . (\ x -> (x, undefined))
  . forestToGrammar
  where
    step (g, _) = (cbsmStep2 g, refineRanking (mergeRanking g))

    showStep1 ((s, ((v1, n1), (v2, n2))), (mrg, delta))
      =  show s ++ "=" ++ show n1 ++ "+" ++ show n2 ++ ": "
      ++ show delta ++ ": "
      ++ show [v1, v2]
      ++ if M.size (RM.forward mrg) > 2
         then " -> " ++ show (map S.toList $ M.elems $ RM.backward mrg)
         else " (saturated)"


asEdgeList :: H.EdgeList v l i -> H.EdgeList v l i
asEdgeList = id

-- asBackwardStar :: H.BackwardStar v l i -> H.BackwardStar v l i
-- asBackwardStar = id
