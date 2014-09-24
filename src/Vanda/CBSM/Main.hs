{-# LANGUAGE LambdaCase, RecordWildCards, ScopedTypeVariables #-}

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
import           Data.RevMap (RevMap)
import           Vanda.CBSM.CountBasedStateMerging
import qualified Vanda.Hypergraph as H
import           Vanda.Util.Tree as T

import           Control.Arrow ((***), first, second)
import           Data.List (foldl', groupBy, sortBy)
import           Data.Function (on)
import qualified Data.Map as M
import           Data.Map (Map, (!))
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Tree

import Debug.Trace


test3 n
  = putStr
  . unlines
  . concatMap (\ (w, d) -> [show w, drawTree' (drawstyleCompact2 0 "") $ fmap (show . H.label) d])
  . bests
  . (!! n)
  . iterate cbsmStep2
  . forestToGrammar


test2 n
  = putStr
  . unlines
  . map (unlines . map show . H.edges . asEdgeList . fst . toHypergraph)
  . take n
  . iterate cbsmStep2
  . forestToGrammar


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

asBackwardStar :: H.BackwardStar v l i -> H.BackwardStar v l i
asBackwardStar = id
