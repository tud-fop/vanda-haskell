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

{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Vanda.Algorithms.InsideOutsideWeightsAcyclic
( AcyclicHypergraph
, parseTree
, dropUnreach
, inside
, outside
) where

import Vanda.Hypergraph

import qualified Data.List as L
import qualified Data.Map as M
import Data.Tree hiding(foldTree)


-- | Represents a 'Hypergraph' with the following 'Hyperedge's:
--
-- @['mkHyperedge' (q, p) [(q1, p ++ [1]), ..., (qn, p ++ [n])] l i | @
-- the 'Tree' contains @('mkHyperedge' q [q1, ..., qn] l i)@ at position @p]@.
type AcyclicHypergraph v l i = Tree (M.Map v [Hyperedge v l i])


-- | Intersect a 'Hypergraph' with a 'Tree' (over 'Hypergraph' labels).
-- The result represents all executions of the given 'Hypergraph' beginning at
-- any node and resulting in the given 'Tree' (only considering 'Hyperedge'
-- labels). The result is represented by an 'AcyclicHypergraph', which reuses
-- the given 'Hyperedge's.
--
-- A naive construction would result in a 'Hypergraph' with vertices which are
-- pairs of vertices of the given 'Hypergraph' and positions of the given
-- 'Tree'.
-- The construction used here is a bottom-up approach, which omits the
-- construction of many useless 'Hyperedge's. This implies, that you reach
-- nullary edges on every path from every node in the constucted
-- 'AcyclicHypergraph'.
--
-- When intersecting the same 'Hypergraph' with many 'Tree's, intermediate
-- helping data structures may be memoized
-- (e.g., @'map' ('parseTree' g) ts@).
parseTree
  :: (Hypergraph h, Ord v, Ord l)
  => h v l i -> Tree l -> AcyclicHypergraph v l i
parseTree g = parseTree' (toBackwardStarWithLabel g)


-- | The same as 'parseTree', but instead of passing the 'Hypergraph'
-- directly, a 'Hyperedge' lookup 'M.Map' is required.
parseTree'
  :: forall v l i
  .  (Ord v, Ord l)
  => M.Map (l, Int) (M.Map v [Hyperedge v l i])
                                               -- ^ 'Hyperedge' lookup 'M.Map'
  -> Tree l                                    -- ^ 'Tree' to be parsed
  -> AcyclicHypergraph v l i
parseTree' g = foldTree step
  where
    step :: l -> Forest (M.Map v [Hyperedge v l i])
              -> Tree   (M.Map v [Hyperedge v l i])
    step l ts
      = flip Node ts
      $ M.mapMaybe
          ( null2Nothing
          . filter (and . zipWith (flip M.member . rootLabel) ts . from)
          )
      $ M.findWithDefault M.empty (l, length ts) g

    null2Nothing :: [a] -> Maybe [a]
    null2Nothing [] = Nothing
    null2Nothing xs = Just xs

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where go (Node x ts) = f x (map go ts)


toBackwardStarWithLabel
  :: (Hypergraph h, Ord v, Ord l)
  => h v l i
  -> M.Map (l, Int) (M.Map v [Hyperedge v l i])
toBackwardStarWithLabel g
  = M.fromListWith (M.unionWith (++))
  . map (\ e -> ((label e, arity e), M.singleton (to e) [e]))
  . edges $ g


-- | Drop unreachable 'Hyperedge's from an 'AcyclicHypergraph', which means
-- 'Hyperedge's which are unreachable from all target vertices (extended by
-- the position @[]@) in the 'AcyclicHypergraph'.
dropUnreach
  :: forall v l i
  .  Ord v
  => [v]                        -- ^ target vertices
  -> AcyclicHypergraph v l i
  -> AcyclicHypergraph v l i
dropUnreach inis t = {-fmap (concat . M.elems) $-} go t (mkRechableM inis)
  where
    go (Node m ts) reachableM
      = Node m'
      $ zipWith go ts
      $ map mkRechableM
      $ L.transpose
      $ map from
      $ concat
      $ M.elems m'
      where
        m' = M.intersection m reachableM
    mkRechableM = M.fromList . map (flip (,) ())


-- | Compute the inside weights of an 'AcyclicHypergraph'. In the result the
-- lacking positions in the vertices are represented by the position in the
-- output 'Tree' (cf. 'AcyclicHypergraph').
inside
  :: forall v l i w
  .  (Num w, Ord v)
  => (Hyperedge v l i -> w)     -- ^ 'Hyperedge' weight access function
  -> AcyclicHypergraph v l i
  -> Tree (M.Map v w)           -- ^ inside weights
inside eW (Node eM ts)
  = Node (M.map (inH 0) eM) ts'
  where
    ts' :: [Tree (M.Map v w)]
    ts' = map (inside eW) ts

    ms :: [M.Map v w]
    ms  = map rootLabel ts'

    lkup :: Ord k => k -> M.Map k a -> a
    lkup = M.findWithDefault (errorModule "inside.lkup")

    inH :: w -> [Hyperedge v l i] -> w
    inH !s (e : es) = inH (s + inT (eW e) (from e) ms) es
    inH !s []       = s

    inT :: w -> [v] -> [M.Map v w] -> w
    inT !p (v : vs) (m : ms') = inT (p * lkup v m) vs ms'
    inT !p []       []        = p
    inT !_ _        _         = errorModule "inside.inT"


-- | Compute the outside weights of an 'AcyclicHypergraph', based on the given
-- inside weights. In the result the lacking positions in the vertices are
-- represented by the position in the output 'Tree'.
outside
  :: (Num w, Ord v)
  => (Hyperedge v l i -> w)     -- ^ 'Hyperedge' weight access function
  -> Tree (M.Map v w)           -- ^ inside weights
  -> AcyclicHypergraph v l i
  -> M.Map v w                  -- ^ outside weight(s) of the target node(s)
  -> Tree (M.Map v w)           -- ^ outside weights
outside eW (Node _ is) (Node eM ts) oM
  = Node oM
  $ zipWith3 (outside eW) is ts
  $ map (M.fromListWith (+))
  $ L.transpose
  $ flip map (concat $ M.elems eM)
    (\ e ->
      let w = M.findWithDefault 0 (to e) oM * eW e  -- 0 if unreachable
          ws = zipWith (flip (M.findWithDefault err) . rootLabel) is (from e)
            where err = errorModule "outside.ws"
          ls = scanl (*) 1 ws
          rs = scanr (*) 1 ws
      in zipWith3 (\ v l r -> (v, w * l * r)) (from e) ls (tail rs)
    )


errorModule :: String -> a
errorModule = error . ("Vanda.Algorithms.InsideOutsideWeightsAcyclic." ++)
