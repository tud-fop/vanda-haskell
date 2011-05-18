-- (c) Johannes Osterholzer <oholzer@gmx.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Algorithms.KAStar 
  where

import Control.Monad.State
--import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.Tree as T
import qualified Data.Heap as H 
import qualified Data.Ord as O

import Data.Hypergraph

-- data Item v l w i =
--   I v (Hyperedge v l w i)   
--     -- ^ Inside item: 1-best derivation of a node
--   | O v (Hyperedge v l w i) 
--     -- ^ Outside item: 1-best g-context for a node
--   | K v (Hyperedge v l w i) Int [Int] 
--     -- ^ Ranked derivation item + backpointers
--   | D v (Hyperedge v l w i) [Int] 
--     -- ^ Modified inside derivation, with list of derivation item ranks


data Item v l w i = 
    I { iNode :: v
      , iEdge :: Hyperedge v l w i
      , iWeight :: w
      }
  | O { oNode :: v
      , oEdge :: Hyperedge v l w i
      , oWeight :: w
      }
  | K { kNode         :: v
      , kEdge         :: Hyperedge v l w i
      , kRank         :: Int
      , kBackpointers :: [Int]
      , kWeight :: w
      } 
  | D { dNode :: v
      , dEdge :: Hyperedge v l w i
      , dBackpointers :: [Int]
      , dWeight :: w
      } 
  deriving Show


weight :: Item v l w i -> w
weight (I _ _ w)     = w
weight (O _ _ w)     = w
weight (K _ _ _ _ w) = w
weight (D _ _ _ w)   = w


-- | Chart of already explored items with their weights.
--   Implemented as a map assigning nodes to their corresponding inside,
--   outside, etc., items. Lists should be sorted by /decreasing/ weights.
type Chart v l w i = M.Map v ( [Item v l w i] -- Inside items
                             , [Item v l w i] -- Outside items
                             , [Item v l w i] -- Ranked derivation items
                             , [Item v l w i] -- Modified derivation items
                             )

type Agenda v l w i = H.MinPrioHeap w (Item v l w i)


-- | @kbest k g h G@ computes a list of @k@ best derivations of the goal
--   node @g@ in the hypergraph @G@. It uses the supplied heuristic
--   @h@ for efficiency, applying the KA* algorithm.
kbest
  :: (Ord v, Ord w, Num w)
  => Int      -- ^ The number @k@ of derivations of @g@ to be searched for
  -> v        -- ^ The goal node @g@ of @G@
  -> (v -> w) -- ^ A heuristic function @h@, must be admissible and 
              -- consistent, this precondition is /not/ checked!
  -> Hypergraph v l w i 
              -- ^ The hypergraph @G@ in which the search is performed
  -> [(T.Tree (Hyperedge v l w i), w)] 
              -- ^ A List of @k@ best derivations, with their weights
kbest = error "not imp"
  