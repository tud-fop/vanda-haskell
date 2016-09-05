-- (c) 2011 Matthias Büchse <Matthias.Buechse@tu-dresden.de>
-- (c) 2011 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

module Vanda.Algorithms.ExpectationMaximization (
  forestEM,
  forestEMlist,
  forestEMstep,
  forestEMstepList,
  normalize
) where

import Control.Arrow ( second )

import Vanda.Algorithms.InsideOutsideWeights
import Vanda.Hypergraph

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V

-- import Debug.Trace

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
  :: (Converging w, RealFloat w, Integral i, Ord v, Hypergraph h, V.Unbox w)
  => [[i]]                -- ^ partition of the ids for normalization
  -> [(v, h v l j, w)]
                          -- ^ a list of training example derivation forests
  -> (Hyperedge v l j -> i)
                          -- ^ function extracting the id from a 'Hyperedge'
  -> (w -> Int -> Bool)   -- ^ stopping cond. (delta-likelihood, no. of iter.)
  -> V.Vector w            -- ^ initial weight vector
  -> V.Vector w
forestEM part gs exId p i
  = snd $ iter (forestEMstep part gs exId) p' (0,i) where
    p' (l1,_) (l2,_) it = p (abs (l2-l1)) it

forestEMlist
  :: (Converging w, RealFloat w, Integral i, Ord v, Hypergraph h, V.Unbox w)
  => [[i]]                -- ^ partition of the ids for normalization
  -> [(v, h v l j, w)]
                          -- ^ a list of training example derivation forests
  -> (Hyperedge v l j -> i)
                          -- ^ function extracting the id from a 'Hyperedge'
  -> V.Vector w            -- ^ initial weight vector
  -> [(w, V.Vector w)]
forestEMlist part gs exId theta
  = iterate (forestEMstep part gs exId) (-42, theta)
  {-where
    list
      = [ w * log (inside exweight g M.! v0)
        | (v0, g, w) <- gs 
        ]
    exweight = (theta V.!) . fromIntegral . exId-}

-- | Normalize a map according to a partition. Very similar to
-- relative-frequency estimation, only that the corpus is partitioned,
-- that is, it actually represents a bunch of corpora.
normalize
  :: (RealFloat w, Integral i, V.Unbox w)
  => [[i]]
  -> V.Vector w
  -> V.Vector w
normalize part m
  = V.accum (*) m (concatMap (handleClass . map fromIntegral) part)
  where
    handleClass is
      | isInfinite factor = zip is (repeat 1)
      | otherwise         = zip is (repeat factor)
      where
        ws = map (m V.!) is
        factor = recip (sum ws)

-- | Do an EM-step. The arguments are as for 'forestEM', only without the
-- stopping condition.
forestEMstep
  :: (Converging w, RealFloat w, Integral i, Ord v, Hypergraph h, V.Unbox w)
  => [[i]]                -- ^ partition of the ids for normalization
  -> [(v, h v l j, w)]
                          -- ^ a list of training-example derivation forests
  -> (Hyperedge v l j -> i)
                          -- ^ function extracting the id from a 'Hyperedge'
  -> (w, V.Vector w)       -- ^ log-likelihood and weight vector before...
  -> (w, V.Vector w)       -- ^ ... and after the step
forestEMstep part gs exId theta
  = second (normalize part)
  . foldl'Special
      (+)
      -- (L.foldl' (\ m (k, v) -> M.insertWith' (+) k v m))
      (\ vec list -> V.accum (+) vec list)
      (0, V.replicate (V.length (snd theta)) 0)
  $ forestEMstepList gs exId theta
  where
    foldl'Special f g
      = L.foldl'
        (\ (x, y) (x', y', _, _) -> x `seq` y `seq` (f x x', g y y'))
    -- ( sum . fst . unzip $ list
    -- , normalize part $ M.fromListWith (+) (concat . snd . unzip $ list)
    -- )

-- | Compile a list of everything that is necessary to complete an EM step.
-- Each entry corresponds to a training example (a forest), and it consists of
-- the log-likelihood contribution of that forest, a list of id-weight pairs
-- for later id-specific summation, and the inner and outer vectors for
-- that forest (not strictly necessary for further processing, but nice for
-- documentation).
forestEMstepList
  :: (Converging w, Floating w, Integral i, Ord v, Hypergraph h, V.Unbox w, Eq w)
  => [(v, h v l j, w)]
                          -- ^ a list of training-example derivation forests
  -> (Hyperedge v l j -> i)
                          -- ^ function extracting the id from a 'Hyperedge'
  -> (w, V.Vector w)      -- ^ log-likelihood and weight vector prior to step
  -> [(w, [(Int, w)], M.Map v w, M.Map v w)] -- ^ (see general info)
forestEMstepList gs exId (_,theta)
  = [
      ( w * log innerv0 -- contribution to log-likelihood
      , [ -- a list of id/weight pairs for upcoming id-specific summation
          ( fromIntegral $ exId e
          , factor
            * M.findWithDefault 0 (to e) outer
            * exweight e
            * (product [ M.findWithDefault 0 v inner | v <- from e ])
          )
        | e <- edges g -- for each hyperedge in the forest
        ]
      , inner -- for debug/documentation purposes (teaching)
      , outer -- for debug/documentation purposes (teaching)
      )
    | (v0, g, w) <- gs -- for each forest
    , let inner = inside exweight g -- compute inside weights
    , let outer = outside exweight inner v0 g -- compute outside weights
    , let innerv0 = M.findWithDefault 0 v0 inner -- inner of target node
    , let factor = w/innerv0 -- example-specific factor for id significance
    , innerv0 /= 0
    -- , traceShow outer True
    ]
    where
      exweight = (theta V.!) . fromIntegral . exId
