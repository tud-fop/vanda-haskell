-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Algorithms.InsideOutsideWeightsAcyclic
-- Copyright   :  (c) Technische Universität Dresden 2011-2014
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Functions to produce and process 'AcyclicHypergraph's.
--
-----------------------------------------------------------------------------

module Vanda.Algorithms.InsideOutsideWeightsAcyclic where

import Vanda.Hypergraph

import Control.Arrow (second)
import qualified Data.Map as IM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tree


-- | Represents a 'Hypergraph' with the following 'Hyperedge's:
--
-- @[hyperedge (q, p) [(q1, p ++ [1]), ..., (qn, p ++ [n])] l w i | @
-- the 'Tree' contains @(hyperedge q [q1, ..., qn] l w i)@ at position @p]@.
type AcyclicHypergraph v l i = Tree [Hyperedge v l i]


-- | Intersect a 'Hypergraph' with a 'Tree' (over 'Hypergraph' labels).
-- The result represents all executions of the given 'Hypergraph' beginning at
-- any of the given target nodes and resulting in the given 'Tree' (only
-- considering 'Hyperedge' labels).
--
-- A naive construction would result in a 'Hypergraph' with vertices which are
-- pairs of vertices of the given 'Hypergraph' and positions of the given
-- 'Tree'.
-- This construction omits the construction of many useless 'Hyperedge's.
-- Moreover, the resulting 'Hypergraph' is represented by an
-- 'AcyclicHypergraph', which reuses the given 'Hyperedge's.
parseTree
  ::  (Hypergraph h, Ord l, Ord v)
  => h v l i                    -- ^ 'Hypergraph' used for parsing
  -> [v]                        -- ^ target vertices
  -> Tree l                     -- ^ 'Tree' to be parsed
  -> AcyclicHypergraph v l i
parseTree g = parseTree' f
  where
    f len lab
      =  M.findWithDefault M.empty lab
      $ IM.findWithDefault M.empty len m
    m = IM.map (M.map (M.fromListWith (++)) . M.fromListWith (++))
      . IM.fromListWith (++)
      . map (\ e -> (length (from e), [(label e, [(to e, [e])])]))
      $ edges g


-- | The same as 'parseTree', but instead of passing the 'Hypergraph'
-- directly, a 'Hyperedge' lookup function is required. This function takes
-- a rank and a label, and returns all 'Hyperedge's with the given rank and
-- label.
parseTree'
  :: (Ord v, Eq l)
  => (Int -> l -> M.Map v [Hyperedge v l i])  -- ^ 'Hyperedge' lookup function
  -> [v]                                      -- ^ target vertices
  -> Tree l                                   -- ^ 'Tree' to be parsed
  -> AcyclicHypergraph v l i
parseTree' f targets tree = goT tree targets
  where
    goT (Node l ts) qs
      = let eM = f (length ts) l
            (es, ts') = goF ts
                      $ map (\ e -> (from e, e))
                      $ concatMap (\ q -> M.findWithDefault [] q eM) qs
        in Node es ts'
    goF [] xs = (map snd xs, [])
    goF (t : ts) xs
      = let t' = goT t $ L.nub $ map (head . fst) xs
            vS = S.fromList $ map to $ rootLabel t'
            fltr [] = []
            fltr ((v : vs, e) : ys)
              | S.member v vS = (vs, e) : fltr ys
              | otherwise     = fltr ys
            fltr _ = errorModule "parseTree'.goF.fltr"
        in second (t' :) $ goF ts $ fltr xs


-- | Drop unreachable 'Hyperedge's from an 'AcyclicHypergraph', which means
-- 'Hyperedge's which are unreachable from all target vertices (extended by
-- the position @[]@) in the 'AcyclicHypergraph'.
dropUnreach
  :: (Ord v)
  => [v]                        -- ^ target vertices
  -> AcyclicHypergraph v l i
  -> AcyclicHypergraph v l i
dropUnreach targets (Node es ts)
  = let vS  = S.fromList targets
        es' = filter (flip S.member vS . to) es
    in Node es'
    $ map (uncurry dropUnreach)
    $ flip zip ts
    $ L.transpose
    $ map from es'


-- | Compute the inside weights of an 'AcyclicHypergraph'. In the result the
-- lacking positions in the vertices are represented by the position in the
-- output 'Tree' (cf. 'AcyclicHypergraph').
inside
  :: (Num w, Ord v)
  => (Hyperedge v l i -> w)     -- ^ 'Hyperedge' weight access function
  -> Tree [Hyperedge v l i]
  -> Tree (M.Map v w)           -- ^ inside weights
inside eW (Node es ts)
  = Node
      ( M.fromListWith (+)
      $ map (\ e -> (to e, go ts' (from e) (eW e))) es
      )
      ts'
  where
    ts' = map (inside eW) ts
    go (x : xs) (v : vs) p
      = let p' = p * M.findWithDefault err v (rootLabel x)
              where err = errorModule "inside.go.p'"
        in p' `seq` go xs vs p'
    go [] [] p = p
    go _  _  _ = errorModule "inside.go"


-- | Compute the outside weights of an 'AcyclicHypergraph', based on the given
-- inside weights. In the result the lacking positions in the vertices are
-- represented by the position in the output 'Tree'.
outside
  :: (Num w, Ord v)
  => (Hyperedge v l i -> w)     -- ^ 'Hyperedge' weight access function
  -> Tree (M.Map v w)           -- ^ inside weights
  -> Tree [Hyperedge v l i]
  -> M.Map v w                  -- ^ outside weight(s) of the target node(s)
  -> Tree (M.Map v w)           -- ^ outside weights
outside eW (Node _ is) (Node es ts) om
  = Node om
  $ zipWith3 (outside eW) is ts
  $ map (M.fromListWith (+))
  $ L.transpose
  $ flip map es
    (\ e ->
      let w = M.findWithDefault 0 (to e) om * eW e  -- 0 if unreachable
          ws = zipWith (flip (M.findWithDefault err) . rootLabel) is (from e)
            where err = errorModule "outside.ws"
          ls = scanl (*) 1 ws
          rs = scanr (*) 1 ws
      in zipWith3 (\ v l r -> (v, w * l * r)) (from e) ls (tail rs)
    )


errorModule :: String -> a
errorModule = error . ("Vanda.Algorithms.InsideOutsideWeightsAcyclic." ++)
