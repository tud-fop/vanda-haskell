-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Algorithms.ExpectationMaximizationAcyclic
-- Copyright   :  (c) Technische Universität Dresden 2011-2014
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Algorithms.ExpectationMaximizationAcyclic (
  forestEM,
  forestEMlist,
  forestEMstep,
  forestEMstepList,
  normalize
) where

import Vanda.Hypergraph
import Vanda.Algorithms.InsideOutsideWeightsAcyclic

import Control.Arrow (second)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Tree as T


-- | Canonical iter function.
iter
  :: (a -> a) -- ^ function to be iterated
  -> (a -> a -> Int -> Bool) -- ^ stopping condition
  -> a        -- ^ initial value
  -> a        -- ^ value after iteration
iter = iter' 0 where
  iter' i f p a
    = let fa = f a
      in
        if p a fa i
        then fa
        else iter' (i+1) f p fa


-- | Execute the forest-EM algorithm, i.e., iterate the EM step.
forestEM
  :: (RealFloat w, Ord i, Ord v)
  => [[i]]                -- ^ partition of the ids for normalization
  -> [(M.Map v w, Tree [Hyperedge v l j], w)]
                          -- ^ a list of training example derivation forests
  -> (Hyperedge v l j -> i)
                          -- ^ function extracting the id from a 'Hyperedge'
  -> (w -> Int -> Bool)   -- ^ stopping cond. (delta-likelihood, no. of iter.)
  -> M.Map i w            -- ^ initial weight vector
  -> M.Map i w
forestEM part gs exId p i
  = snd $ iter (forestEMstep part gs exId) p' (0,i) where
    p' (l1,_) (l2,_) it = p (abs (l2-l1)) it


-- | Compute the list of EM estimates for a given corpus.
-- Use 'take' or '!!' to access a prefix or an element, respectively.
forestEMlist
  :: (RealFloat w, Ord i, Ord v)
  => [[i]]                -- ^ partition of the ids for normalization
  -> [(M.Map v w, Tree [Hyperedge v l j], w)]
                          -- ^ a list of training example derivation forests
  -> (Hyperedge v l j -> i)
                          -- ^ function extracting the id from a 'Hyperedge'
  -> M.Map i w            -- ^ initial weight vector
  -> [(w, M.Map i w)]     -- ^ list of (log-likelihood, estimate) pairs
forestEMlist part gs exId i
  = iterate (forestEMstep part gs exId) (0, i)


-- | Normalize a map according to a partition. Very similar to
-- relative-frequency estimation, only that the corpus is partitioned,
-- that is, it actually represents a bunch of corpora.
normalize
  :: (RealFloat w, Ord i)
  => [[i]]
  -> M.Map i w
  -> M.Map i w
normalize part m
  = M.fromList (concatMap handleClass part)
  where
    handleClass is
      | isInfinite factor = zip is ws
      | otherwise         = zip is (map (factor *) ws)
      where
        ws = map (\ i -> M.findWithDefault 0 i m) is
        factor = recip (sum ws)


-- | Do an EM-step. The arguments are as for 'forestEM', only without the
-- stopping condition.
forestEMstep
  :: (RealFloat w, Ord i, Ord v)
  => [[i]]                -- ^ partition of the ids for normalization
  -> [(M.Map v w, Tree [Hyperedge v l j], w)]
                          -- ^ a list of training-example derivation forests
  -> (Hyperedge v l j -> i)
                          -- ^ function extracting the id from a 'Hyperedge'
  -> (w, M.Map i w)       -- ^ log-likelihood and weight vector before...
  -> (w, M.Map i w)       -- ^ ... and after the step
forestEMstep part gs exId theta
  = second (normalize part)
  . foldl'Special
      (+)
      (L.foldl' (\ m (k, v) -> M.insertWith' (+) k v m))
      (0, M.empty)
  $ forestEMstepList gs exId theta
  where
    foldl'Special f g
      = L.foldl'
        (\ (x, y) (x', y') -> x `seq` y `seq` (f x x', g y y'))
    -- ( sum . fst . unzip $ list
    -- , normalize part $ M.fromListWith (+) (concat . snd . unzip $ list)
    -- )


-- | Compile a list of everything that is necessary to complete an EM step.
-- Each entry corresponds to a training example (a forest), and it consists of
-- the log-likelihood contribution of that forest and a list of id-weight
-- pairs for later id-specific summation.
forestEMstepList
  :: (Floating w, Ord i, Ord v)
  => [(M.Map v w, Tree [Hyperedge v l j], w)]
                          -- ^ a list of training-example derivation forests
  -> (Hyperedge v l j -> i)
                          -- ^ function extracting the id from a 'Hyperedge'
  -> (w, M.Map i w)       -- ^ log-likelihood and weight vector prior to step
  -> [(w, [(i, w)])]
forestEMstepList gs exId (_, theta)
  = [
      ( w * log inner0
      , go g inner outer
      )
    | let exweight e = M.findWithDefault 0 (exId e) theta
    , (w0, g, w) <- gs
    , let inner = inside exweight g
    , let outer = outside exweight inner g w0
    , let inner0  -- inner of target node(s)
            = sum
            $ map (\ (v, w') ->  w' * M.findWithDefault 0 v (rootLabel inner))
            $ M.toList w0
    , let factor = w / inner0  -- example-specific factor for id significance
    , let go (Node es ts) (Node _ is) (Node oM os)
            = map f es ++ concat (zipWith3 go ts is os)
            where f e = (exId e
                        ,   factor
                          * M.findWithDefault 0 (to e) oM
                          * exweight e
                          * product
                            ( zipWith
                                (\ v -> M.findWithDefault 0 v . rootLabel)
                                (from e)
                                is
                            )
                        )
    ]
