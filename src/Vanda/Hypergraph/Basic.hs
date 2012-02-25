-- (c) 2012 Matthias Büchse <Matthias.Buechse@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

-- |
-- Maintainer  :  Matthias Buechse
-- Stability   :  unknown
-- Portability :  portable
--
-- This module defines the Hypergraph class and the algebraic data types
-- for the several hypergraph representations.
module Vanda.Hypergraph.Basic
  ( Hyperedge (..)
  , Derivation
  , mkHyperedge
  , EdgeList (..)
  , BackwardStar (..)
  , ForwardStar (..)
  , Simulation (..)
  , nodesL
  , nodesLL
  ) where

import Control.Arrow ( (&&&) )
import qualified Data.Ix as Ix
import qualified Data.Tree as T
import qualified Data.Vector as V

-- | A Hyperedge, consisting of head node, tail nodes, label, and identifier.
-- The identifier can be used for interfacing with feature functions.
-- Hyperedge is made an instance of Eq and Ord solely via the identifier.
data Hyperedge v l i
  = Hyperedge
    { to :: !v
    , from :: V.Vector v
    , label :: !l
    , i :: !i
    }

instance Eq i => Eq (Hyperedge v l i) where
  Hyperedge _ _ _ i1 == Hyperedge _ _ _ i2 = i1 == i2

instance Ord i => Ord (Hyperedge v l i) where
  Hyperedge _ _ _ i1 `compare` Hyperedge _ _ _ i2 = i1 `compare` i2

-- | A derivation (tree), i.e., a tree over hyperedges.
type Derivation v l i = T.Tree (Hyperedge v l i)

-- | Creates a 'Hyperedge'.
mkHyperedge
  :: v   -- ^ Head node
  -> [v] -- ^ Tail nodes
  -> l   -- ^ Label
  -> i   -- ^ Identifier
  -> Hyperedge v l i
mkHyperedge t f l i
  = Hyperedge
    { to = t
    , from = V.fromList f
    , label = l
    , i = i
    }

-- * Hypergraph representations

-- | Edge list representation of a Hypergraph. It consists of the interval
-- of nodes present in the hypergraph and a list of its hyperedges.
data EdgeList v l i
  = EdgeList
    { nodesEL :: (v, v) -- ^ Interval of nodes
    , edgesEL :: [Hyperedge v l i] -- ^ List of 'Hyperedge's
    }

-- | Backward star representation of a hypergraph. The backward star of a
-- node is defined as the set of all ingoing edges. In other words,
-- Hyperedges are sorted according to the head node.
data BackwardStar v l i
  = BackwardStar
    { nodesBS :: (v, v) -- ^ Interval of nodes
    , backStar :: v -> [Hyperedge v l i] -- ^ Backward star
    , memoBS :: Bool  -- ^ Whether the backward star is memoized
    }

-- | Forward star representation of a hypergraph. The forward star of a
-- node is defined as the set of all outgoing edges. Note that the forward
-- star alone can not represent a hypergraph because it does not cover
-- nullary edges. Therefore nullary edges are explicitly included here.
data ForwardStar v l i
  = ForwardStar
    { nodesFS :: (v, v) -- ^ Interval of nodes
    , nullaryEdges :: [Hyperedge v l i] -- ^ List of nullary edges
    , forwStar :: v -> [Hyperedge v l i] -- ^ Forward star
    , memoFS :: Bool -- ^ Whether the forward star is memoized
    }

-- | Simulation representation of a hypergraph. Here hyperedges are sorted
-- according to head, label, and rank. This is useful for some parsing
-- procedures. This type is not a member of the Hypergraph class because
-- reconstructing all hyperedges from the sorted form is too tedious!
data Simulation v l i
  = Simulation
    { nodesSIM :: (v, v) -- ^ Interval of nodes
    , lookupSIM :: v -> l -> Int -> [Hyperedge v l i] -- ^ Lookup function
    }

-- | Extracts the nodes occurring in a list of edges. Does /not/ remove
-- duplicates.
nodesLL :: [Hyperedge v l i] -> [v]
nodesLL es = [ v | e <- es, v <- to e : V.toList (from e) ]

-- | Obtains the interval of nodes occurring in a list of edges.
nodesL :: Ix.Ix v => [Hyperedge v l i] -> (v,v)
nodesL = (minimum &&& maximum) . nodesLL

