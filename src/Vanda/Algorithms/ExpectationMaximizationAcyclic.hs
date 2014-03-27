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

import qualified Data.List as L
import qualified Data.Map.Strict as M
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
  -> [(AcyclicHypergraph v l j, w)]
                          -- ^ a list of training example derivation forests
  -> (Hyperedge v l j -> i)
                          -- ^ function extracting the id from a 'Hyperedge'
  -> (w -> Int -> Bool)   -- ^ stopping cond. (delta-likelihood, no. of iter.)
  -> M.Map v w            -- ^ initial weights
  -> M.Map i w            -- ^ weight vector
  -> (M.Map v w, M.Map i w)
forestEM part gs exId p w0 ws
  = snd $ iter (forestEMstep part gs exId) p' (0, (w0, ws)) where
    p' (l1,_) (l2,_) it = p (abs (l2-l1)) it


-- | Compute the list of EM estimates for a given corpus.
-- Use 'take' or '!!' to access a prefix or an element, respectively.
forestEMlist
  :: (RealFloat w, Ord i, Ord v)
  => [[i]]                -- ^ partition of the ids for normalization
  -> [(AcyclicHypergraph v l j, w)]
                          -- ^ a list of training example derivation forests
  -> (Hyperedge v l j -> i)
                          -- ^ function extracting the id from a 'Hyperedge'
  -> M.Map v w            -- ^ initial weights
  -> M.Map i w            -- ^ weight vector
  -> [(w, (M.Map v w, M.Map i w))]
                        -- ^ list of (log-likelihood, initial, estimate) pairs
forestEMlist part gs exId w0 ws
  = iterate (forestEMstep part gs exId) (0, (w0, ws))


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
  -> [(AcyclicHypergraph v l j, w)]
                          -- ^ a list of training-example derivation forests
  -> (Hyperedge v l j -> i)
                          -- ^ function extracting the id from a 'Hyperedge'
  -> (w, (M.Map v w, M.Map i w))
              -- ^ log-likelihood, initial weights and weight vector before...
  -> (w, (M.Map v w, M.Map i w))                   -- ^ ... and after the step
forestEMstep part gs exId theta
  = (l, (normalize [M.keys w0] w0, normalize part ws))
  where
    (l, w0, ws)
      = foldl'Special
          (+)
          (M.unionWith (+))
          (L.foldl' (\ m (k, v) -> M.insertWith (+) k v m))
          (0, M.empty, M.empty)
      $ forestEMstepList gs exId theta
    foldl'Special f g h
      = L.foldl' $ \ (x, y, z) (x', y', z')
          -> x `seq` y `seq` z `seq` (f x x', g y y', h z z')
    -- ( sum . fst . unzip $ list
    -- , normalize part $ M.fromListWith (+) (concat . snd . unzip $ list)
    -- )


-- | Compile a list of everything that is necessary to complete an EM step.
-- Each entry corresponds to a training example (a forest), and it consists of
-- the log-likelihood contribution of that forest and a list of id-weight
-- pairs for later id-specific summation.
forestEMstepList
  :: (Floating w, Ord i, Ord v)
  => [(AcyclicHypergraph v l j, w)]
                          -- ^ a list of training-example derivation forests
  -> (Hyperedge v l j -> i)
                          -- ^ function extracting the id from a 'Hyperedge'
  -> (w, (M.Map v w, M.Map i w))
      -- ^ log-likelihood prior to step, initial weights and hyperedge weights
  -> [(w, M.Map v w, [(i, w)])]
forestEMstepList gs exId (_, (w0, theta))
  = [
      ( w * log inner0
      , M.mapWithKey
          (\ v w' -> factor * w' * M.findWithDefault 0 v (rootLabel inner))
          w0
      , go g inner outer
      )
    | let exweight e = M.findWithDefault 0 (exId e) theta
    , (g, w) <- gs
    , let inner = inside exweight g
    , let outer = outside exweight inner g w0
    , let inner0  -- inner of virtual single target node
            = sum
            $ map (\ (v, w') ->  w' * M.findWithDefault 0 v (rootLabel inner))
            $ M.toList w0
    , let factor = w / inner0  -- example-specific factor for id significance
    , let go (Node eM ts) (Node _ is) (Node oM os)
            = map f (concat $ M.elems eM) ++ concat (zipWith3 go ts is os)
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
