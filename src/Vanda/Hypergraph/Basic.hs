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
  , deref
  , from
  , arity
  , mapHE
  , mapHEi
  , interlace
  , Derivation
  , mkHyperedge
  , EdgeList (..)
  , BackwardStar (..)
  , ForwardStar (..)
  , Simulation (..)
  , nodesL
  , nodesLL
  , nodesR
  ) where

import qualified Data.List as L
import qualified Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Set as S

-- | A Hyperedge, consisting of head node, tail nodes, label, and identifier.
-- The identifier can be used for interfacing with feature functions.
-- Hyperedge is made an instance of Eq and Ord solely via the identifier.
data Hyperedge v l i
  = Nullary
    { to :: !v
    , label :: !l
    , ident :: !i
    }
  | Unary
    { to :: !v
    , from1 :: !v
    , label :: !l
    , ident :: !i
    }
  | Binary
    { to :: !v
    , from1 :: !v
    , from2 :: !v
    , label :: !l
    , ident :: !i
    }
  | Hyperedge
    { to :: !v
    , _from :: !(V.Vector v)
    , label :: !l
    , ident :: !i
    }

    
instance (Show v, Show l, Show i) => Show (Hyperedge v l i) where
  show e
    = show (to e)
      ++ " -> "
      ++ show (label e)
      ++ " "
      ++ unwords (map show $ from e)
      ++ " # "
      ++ show (ident e)


from :: Hyperedge v l i -> [v]
from Nullary{} = []
from (Unary _ f1 _ _) = [f1]
from (Binary _ f1 f2 _ _) = [f1, f2]
from (Hyperedge _ f _ _) = V.toList f

deref (Unary _ f1 _ _) 0 = f1
deref (Binary _ f1 f2 _ _) 0 = f1
deref (Binary _ f1 f2 _ _) 1 = f2
deref (Hyperedge _ f _ _) i = f V.! i

arity :: Hyperedge v l i -> Int
arity Nullary{} = 0
arity Unary{} = 1
arity Binary{} = 2
arity (Hyperedge _ f _ _) = V.length f

mapHE :: (v -> v') -> Hyperedge v l i -> Hyperedge v' l i
mapHE g (Nullary t l i) = Nullary (g t) l i
mapHE g (Unary t f1 l i) = Unary (g t) (g f1) l i
mapHE g (Binary t f1 f2 l i) = Binary (g t) (g f1) (g f2) l i
mapHE g (Hyperedge t f l i) = Hyperedge (g t) (V.map g f) l i

mapHEi :: (i -> i') -> Hyperedge v l i -> Hyperedge v l i'
mapHEi g (Nullary t l i) = Nullary t l (g i)
mapHEi g (Unary t f1 l i) = Unary t f1 l (g i)
mapHEi g (Binary t f1 f2 l i) = Binary t f1 f2 l (g i)
mapHEi g (Hyperedge t f l i) = Hyperedge t f l (g i)

interlace
  :: ((Int, Int) -> Int)
  -> Hyperedge Int l i1
  -> Hyperedge Int l i2
  -> Hyperedge Int l (i1, i2)
interlace ix (Nullary t1 l i1) (Nullary t2 _ i2)
  = Nullary (ix (t1, t2)) l (i1, i2)
interlace ix (Unary t1 f1 l i1) (Unary t2 f2 _ i2)
  = Unary (ix (t1, t2)) (ix (f1, f2)) l (i1, i2)
interlace ix (Binary t1 f11 f12 l i1) (Binary t2 f21 f22 _ i2)
  = Binary (ix (t1, t2)) (ix (f11, f21)) (ix (f12, f22)) l (i1, i2)
interlace ix (Hyperedge t1 f1 l i1) (Hyperedge t2 f2 _ i2)
  = Hyperedge (ix (t1,t2)) (V.zipWith (curry ix) f1 f2) l (i1,i2)
interlace _ _ _ = error "cannot interlace different arities"
  

instance Eq i => Eq (Hyperedge v l i) where
  e1 == e2 = ident e1 == ident e2

instance Ord i => Ord (Hyperedge v l i) where
  e1 `compare` e2 = ident e1 `compare` ident e2

-- | A derivation (tree), i.e., a tree over hyperedges.
type Derivation v l i = T.Tree (Hyperedge v l i)

-- | Creates a 'Hyperedge'.
mkHyperedge
  :: v   -- ^ Head node
  -> [v] -- ^ Tail nodes
  -> l   -- ^ Label
  -> i   -- ^ Identifier
  -> Hyperedge v l i
mkHyperedge !t !f !l !i
  = case f of
      [] -> Nullary t l i
      [f1] -> Unary t f1 l i 
      [f1, f2] -> Binary t f1 f2 l i
      _ -> Hyperedge t (V.fromList f) l i

-- * Hypergraph representations

-- | Edge list representation of a Hypergraph. It consists of the interval
-- of nodes present in the hypergraph and a list of its hyperedges.
data EdgeList v l i
  = EdgeList
    { nodesEL :: S.Set v -- ^ Interval of nodes
    , edgesEL :: [Hyperedge v l i] -- ^ List of 'Hyperedge's
    }

-- | Backward star representation of a hypergraph. The backward star of a
-- node is defined as the set of all ingoing edges. In other words,
-- Hyperedges are sorted according to the head node.
data BackwardStar v l i
  = BackwardStar
    { nodesBS :: S.Set v -- ^ Interval of nodes
    , backStar :: v -> [Hyperedge v l i] -- ^ Backward star
    , memoBS :: Bool  -- ^ Whether the backward star is memoized
    }

-- | Forward star representation of a hypergraph. The forward star of a
-- node is defined as the set of all outgoing edges. Note that the forward
-- star alone can not represent a hypergraph because it does not cover
-- nullary edges. Therefore nullary edges are explicitly included here.
data ForwardStar v l i
  = ForwardStar
    { nodesFS :: S.Set v -- ^ Interval of nodes
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
    { nodesSIM :: S.Set v -- ^ Interval of nodes
    , lookupSIM :: v -> l -> Int -> [Hyperedge v l i] -- ^ Lookup function
    }

-- | Extracts the nodes occurring in a list of edges. Does /not/ remove
-- duplicates.
nodesLL :: [Hyperedge v l i] -> [v]
nodesLL es = [ v | e <- es, v <- to e : from e ]

-- | Obtains the interval of nodes occurring in a list of edges.
nodesL :: Ord v => [Hyperedge v l i] -> S.Set v
-- nodesL = (minimum &&& maximum) . nodesLL
nodesL = S.fromList . nodesLL


nodesR :: Ord v => [Hyperedge v l i] -> (v,v)
nodesR es = L.foldl' minimax (x,x) xs
  where
    minimax (min0, max0) a
      = let min1 = min min0 a
            max1 = max max0 a
        in min1 `seq` max1 `seq` (min1, max1)
    (x:xs) = nodesLL es
