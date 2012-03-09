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
  ) where

import Prelude hiding ( product )

import Control.Arrow ( (&&&) )
import qualified Data.Array as A
import qualified Data.Ix as Ix
import qualified Data.Vector as V

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
    :: (Ord i, Ix.Ix v, Eq i, Eq x)
    => h v l i
    -> Feature l i x
    -> V.Vector Double
    -> BestArray v l i x
  bests hg feat wV = BS.bests (toBackwardStar hg) feat wV (knuth hg feat wV)
  
  -- | Computes the array of best derivations, given an array of one best
  -- derivations (e.g., obtained via Knuth's algorithm).
  bests'
    :: (Ord i, Ix.Ix v)
    => h v l i
    -> Feature l i x
    -> V.Vector Double
    -> BestArray v l i x
    -> BestArray v l i x
  bests' = BS.bests . toBackwardStar
  
  -- | Drops unreachable nodes and corresponding edges.
  dropUnreachables
    :: Ix.Ix v
    => v                  -- ^ Target node for reachability analysis
    -> h v l i            -- ^ Input hypergraph
    -> BackwardStar v l i -- ^ Output hypergraph
  dropUnreachables v0 = BS.dropUnreachables v0 . toBackwardStar
  
  -- | Returns the list of 'Hyperedge's.
  edges :: Ix.Ix v => h v l i -> [Hyperedge v l i]
  edges = edgesEL . toEdgeList
  
  -- | Applies a filter to all hyperedges. Need not be memoized.
  filterEdges :: Ix.Ix v => (Hyperedge v l i -> Bool) -> h v l i -> h v l i
  
  -- | Computes the best derivation for each node.
  knuth
    :: (Ix.Ix v, Eq i, Eq x)
    => h v l i
    -> Feature l i x
    -> V.Vector Double
    -> BestArray v l i x
  knuth = EL.knuth . toEdgeList
  
  -- | Applies a mapping to all hyperedges. The mapping may not change the
  -- structure (i.e., head or tail), only label and id. Need not be memoized.
  mapLabels
    :: Ix.Ix v
    => (Hyperedge v l i -> Hyperedge v l' i')
    -> h v l i
    -> h v l' i'
  
  -- | Applies a renaming to all nodes. The renaming must be monotone!
  mapNodes
    :: (Ix.Ix v, Ix.Ix v')
    => (v -> v')
    -> h v l i
    -> h v' l i
  
  -- | Memoizes the hypergraph, if applicable. This function is idempotent.
  memoize :: Ix.Ix v => h v l i -> h v l i
  memoize = id
  
  -- | Creates a Hypergraph from a list of 'Hyperedge's.
  mkHypergraph :: Ix.Ix v => [Hyperedge v l i] -> h v l i
  
  -- | Returns the interval of all nodes.
  nodes :: Ix.Ix v => h v l i -> (v, v)
  
  -- | Returns the number of edges. This is included for its instructiveness.
  edgeCount :: Ix.Ix v => h v l i -> Int
  edgeCount = length . edges
  
  -- | Returns the EdgeList representation of a hypergraph.
  toEdgeList     :: Ix.Ix v => h v l i -> EdgeList v l i
  
  -- | Returns the BackwardStar representation of a hypergraph.
  toBackwardStar :: Ix.Ix v => h v l i -> BackwardStar v l i
  toBackwardStar = toBackwardStar . toEdgeList
  
  -- | Returns the ForwardStar representation of a hypergraph.
  toForwardStar  :: Ix.Ix v => h v l i -> ForwardStar v l i
  toForwardStar = toForwardStar . toEdgeList
  
  -- | Returns the Simulation representation of a hypergraph.
  toSimulation   :: (Ix.Ix v, Ord l) => h v l i -> Simulation v l i
  toSimulation = toSimulation . toEdgeList


instance Hypergraph EdgeList where
  filterEdges = EL.filterEdges
  mapLabels = EL.mapLabels
  mapNodes = EL.mapNodes
  mkHypergraph = uncurry EdgeList . (nodesL &&& id)
  nodes = nodesEL
  toEdgeList = id
  toBackwardStar = EL.toBackwardStar
  toForwardStar = EL.toForwardStar
  toSimulation = EL.toSimulation

instance Hypergraph BackwardStar where
  edgeCount = BS.edgeCount
  filterEdges = BS.filterEdges
  mapLabels = BS.mapLabels
  mapNodes = BS.mapNodes
  memoize = BS.memoize
  mkHypergraph = EL.toBackwardStar . mkHypergraph
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
  mkHypergraph = EL.toForwardStar . mkHypergraph
  nodes = nodesFS
  toEdgeList = FS.toEdgeList
  toForwardStar = id


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

-- OLD STUFF!

data Hypergraph' v l i x = Hypergraph' (A.Array v x) (x -> Hyperedge v l i)


data BinHyperedge l i
  = NullaryHyperedge
    { _to :: !Int
    , _label :: !l
    , _i :: !i
    }
  | UnaryHyperedge
    { _to :: !Int
    , _label :: !l
    , _i :: !i
    , _from :: !Int
    }
  | BinaryHyperedge
    { _to :: !Int
    , _label :: !l
    , _i :: !i
    , _from :: !Int
    , _from2 :: !Int
    }

convert :: BinHyperedge l i -> Hyperedge Int l i
convert (NullaryHyperedge t l i) = Hyperedge t V.empty l i
convert (UnaryHyperedge t l i f) = Hyperedge t (V.singleton f) l i
convert (BinaryHyperedge t l i f f2) = Hyperedge t (V.fromList [f,f2]) l i

