-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Hypergraph
-- Copyright   :  (c) Technische Universität Dresden 2012
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Matthias.Buechse@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- This module implements several representations of hypergraphs.
-- Currently, the following representations are available:
-- 'EdgeList', 'BackwardStar', 'ForwardStar', 'Simulation'.
-- All representations (but the last one) are lumped together in the class
-- 'Hypergraph', which enables conversion into any representation as well
-- as some generic functions.
--
-----------------------------------------------------------------------------

module Vanda.Hypergraph
  ( module Vanda.Hypergraph.Basic
  , Hypergraph (..)
--  , product
--  , product'
  ) where

import Prelude hiding ( product )

import Control.Arrow ( (&&&) )
import Control.DeepSeq ( NFData )
import qualified Data.Ix as Ix
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Set as S

import Vanda.Features hiding ( product )
import Vanda.Hypergraph.Basic
import qualified Vanda.Hypergraph.EdgeList as EL
import qualified Vanda.Hypergraph.BackwardStar as BS
import qualified Vanda.Hypergraph.ForwardStar as FS

{-- | Creates a Hypergraph from a list of 'Hyperedge's.
mkHypergraph :: Ix.Ix v => [Hyperedge v l i] -> EdgeList v l i
mkHypergraph = uncurry EdgeList . (nodesL &&& id)--}


-- | Hypergraph class
class Hypergraph h where
  -- | Computes the array of best derivations. Basically a composition
  -- of "bests'" and 'knuth'.
  bests
    :: (Integral i, NFData v, NFData l, NFData i, NFData x, Ix.Ix v, Show l, Show v, Show i)
    => h v l i
    -> Feature l i x
    -> V.Vector Double
    -> M.Map v [Candidate v l i x] -- BestArray v l i x
  bests hg feat wV = BS.bests (toBackwardStar hg) feat wV (knuth hg feat wV)
  
  -- | Computes the array of best derivations, given an array of one best
  -- derivations (e.g., obtained via Knuth's algorithm).
  bests'
    :: (Ord i, Ix.Ix v)
    => h v l i
    -> Feature l i x
    -> V.Vector Double
    -> M.Map v [Candidate v l i x] -- BestArray v l i x
    -> M.Map v [Candidate v l i x] -- BestArray v l i x
  bests' = BS.bests . toBackwardStar
  
  -- | Drops nonproducing nodes and corresponding edges.
  dropNonproducing
    :: Ord v
    => h v l i            -- ^ Input hypergraph
    -> BackwardStar v l i -- ^ Output hypergraph
  dropNonproducing = BS.dropNonproducing . toBackwardStar
  
  -- | Drops unreachable nodes and corresponding edges.
  dropUnreachables
    :: Ord v
    => v                  -- ^ Target node for reachability analysis
    -> h v l i            -- ^ Input hypergraph
    -> BackwardStar v l i -- ^ Output hypergraph
  dropUnreachables v0 = BS.dropUnreachables v0 . toBackwardStar
  
  -- | Returns the list of 'Hyperedge's.
  edges :: Ord v => h v l i -> [Hyperedge v l i]
  edges = edgesEL . toEdgeList
  
  -- | Applies a filter to all hyperedges. Need not be memoized.
  filterEdges :: Ord v => (Hyperedge v l i -> Bool) -> h v l i -> h v l i
  
  -- | Computes the best derivation for each node.
  knuth
    :: (NFData v, NFData l, NFData i, NFData x, Ord i, Ord v, Show l, Show v, Show i)
    => h v l i
    -> Feature l i x
    -> V.Vector Double
    -> BestArray v l i x
  knuth = EL.knuth . toEdgeList
  
  -- | Applies a mapping to all hyperedges. The mapping may not change the
  -- structure (i.e., head or tail), only label and id. Need not be memoized.
  mapLabels
    :: Ord v
    => (Hyperedge v l i -> Hyperedge v l' i')
    -> h v l i
    -> h v l' i'
  
  -- | Applies a renaming to all nodes. The renaming must be monotone!
  mapNodes
    :: (Ord v, Ord v')
    => (v -> v')
    -> h v l i
    -> h v' l i
  
  -- | Memoizes the hypergraph, if applicable. This function is idempotent.
  memoize :: Ord v => h v l i -> h v l i
  memoize = id
  
  -- | Creates a Hypergraph from a list of 'Hyperedge's.
  mkHypergraph :: (Ord v) => [Hyperedge v l i] -> h v l i
  
  -- | Returns the set of all nodes.
  nodes :: Ord v => h v l i -> S.Set v -- (v, v)
  
  -- | Returns the number of edges. This is included for its instructiveness.
  edgeCount :: Ord v => h v l i -> Int
  edgeCount = length . edges
  
  -- | Returns the EdgeList representation of a hypergraph.
  toEdgeList     :: Ord v => h v l i -> EdgeList v l i
  
  -- | Returns the BackwardStar representation of a hypergraph.
  toBackwardStar :: Ord v => h v l i -> BackwardStar v l i
  toBackwardStar = BS.fromEdgeList . toEdgeList
  
  -- | Returns the ForwardStar representation of a hypergraph.
  toForwardStar  :: Ord v => h v l i -> ForwardStar v l i
  toForwardStar = FS.fromEdgeList . toEdgeList
  
  -- | Returns the Simulation representation of a hypergraph.
  toSimulation   :: (Ord v, Ord l) => h v l i -> Simulation v l i
  toSimulation = toSimulation . toEdgeList


instance Hypergraph EdgeList where
  filterEdges = EL.filterEdges
  mapLabels = EL.mapLabels
  mapNodes = EL.mapNodes
  mkHypergraph = uncurry EdgeList . (nodesL &&& id)
  nodes = nodesEL
  toEdgeList = id
  toSimulation = EL.toSimulation

instance Hypergraph BackwardStar where
  edgeCount = BS.edgeCount
  filterEdges = BS.filterEdges
  mapLabels = BS.mapLabels
  mapNodes = BS.mapNodes
  memoize = BS.memoize
  mkHypergraph = BS.fromEdgeList . mkHypergraph
  nodes = nodesBS
  toEdgeList = BS.toEdgeList
  toBackwardStar = id
  toSimulation = BS.toSimulation

instance Hypergraph ForwardStar where
  edgeCount = FS.edgeCount
  filterEdges = FS.filterEdges
  mapLabels = FS.mapLabels
  mapNodes = FS.mapNodes
  memoize = FS.memoize
  mkHypergraph = FS.fromEdgeList . mkHypergraph
  nodes = nodesFS
  toEdgeList = FS.toEdgeList
  toForwardStar = id

{-
product
  :: (Hypergraph h1, Hypergraph h2)
  => (Hyperedge Int l i1 -> Hyperedge Int l i2 -> Bool)
  -> h1 Int l i1
  -> h2 Int l i2
  -> BackwardStar Int l (i1,i2)
product comp h1 h2 = BS.product comp (toBackwardStar h1) (toBackwardStar h2)

product'
  :: (Hypergraph h1, Hypergraph h2)
  => (Hyperedge Int l i1 -> Hyperedge Int l i2 -> Bool)
  -> h1 Int l i1
  -> h2 Int l i2
  -> BackwardStar Int l (i1,i2)
product' comp h1 h2 = BS.product' comp (toBackwardStar h1) (toBackwardStar h2)
-}
