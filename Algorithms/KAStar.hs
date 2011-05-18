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

module Algorithms.KAStar 
  where

import Control.Monad.State
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Tree as T
import qualified Data.Heap as H 

import Data.Hypergraph

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
kbest = error "not implemented"
  