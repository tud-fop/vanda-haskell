module Algorithms.ExpectationMaximization (
  forestEM,
  forestEMstep
) where

import Algorithms.InsideOutsideWeights
import Data.Hypergraph
import qualified Data.Map as M

-- | Canonical iter function.
iter :: (a -> a) -- ^ function to be iterated
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

-- | Execute the forest EM algorithm, i.e., iterate the EM step.
forestEM
  :: (Converging w, Floating w, Ord i, Ord v)
  => [(v, Hypergraph v l w j, w)]
                          -- ^ a list of training example derivation forests
  -> (Hyperedge v l w j -> i)
                          -- ^ function extracting the id from a 'Hyperedge'
  -> [[i]]                -- ^ partition of the ids for normalization
  -> (w -> Int -> Bool)   -- ^ stopping cond. (delta-likelihood, no. of iter.)
  -> M.Map i w            -- ^ initial weight vector
  -> M.Map i w
forestEM gs exId part p i
  = snd $ iter (forestEMstep gs exId part) p' (0,i) where
    p' (l1,m1) (l2,m2) it = p (abs (l2-l1)) it

-- | Normalizes a map according to a partition
-- i.e., do relative-frequency estimation on the corpora described by
-- the map and the partition
normalize
  :: (Floating w, Ord i)
  => [[i]]
  -> M.Map i w
  -> M.Map i w
normalize part m = M.fromList (concatMap handleClass part) where
  handleClass c = zip c (map ((1/(sum c1))*) c1) where
    c1 = map (\i -> maybe 0 id $ M.lookup i m) c

-- | Do an EM-step. The arguments are as for 'forestEM', only without the
-- stopping condition.
forestEMstep
  :: (Converging w, Floating w, Ord i, Ord v)
  => [(v, Hypergraph v l w j, w)]
                          -- ^ a list of training-example derivation forests
  -> (Hyperedge v l w j -> i)
                          -- ^ function extracting the id from a 'Hyperedge'
  -> [[i]]                -- ^ partition of the ids for normalization
  -> (w, M.Map i w)       -- ^ log-likelihood and weight vector before...
  -> (w, M.Map i w)       -- ^ ... and after the step
forestEMstep gs exId part (l1,theta)
  = ( sum.fst.unzip $ list
    , normalize part $ M.fromListWith (+) (concat.snd.unzip $ list)
    ) where
    list =
      [
        ( w*(log innerv0) -- contribution to log-likelihood
        , [ -- a list of id/weight pairs for upcoming id-specific summation
            ( exId e
            , factor
              * (maybe 0 id $ M.lookup (eHead e) outer)
              * exweight e
              * (product [ maybe 0 id $ M.lookup v inner | v <- eTail e ])
            )
          | e <- edges g -- for each hyperedge in the forest
          ]
        )
      | (v0, g, w) <- gs -- for each forest
      , let inner = inside exweight g -- compute inside weights
      , let outer = outside exweight inner v0 g -- compute outside weights
      , let innerv0 = maybe 0 id $ M.lookup v0 inner -- inner of target node
      , let factor = w/innerv0 -- example-specific factor for id significance
      ]
    exweight e = maybe 0 id $ M.lookup (exId e) theta
