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
import Data.Map ((!))
import qualified Data.Tree as T
import qualified Data.Heap as H 
import qualified Data.Ord as O
import qualified Data.List as L
import Data.Maybe (listToMaybe)


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


data Item v l w i = Inside (I v l w i)
                  | Outside (O v l w i)
                  | Ranked (K v l w i)
                  | Derivation (D v l w i)
                  deriving Show

data I v l w i = I { iNode   :: v
                   , iEdge   :: Maybe (Hyperedge v l w i)
                   , iWeight :: Maybe w
                   } deriving Show

data O v l w i = O { oNode   :: v
                   , oEdge   :: Maybe (Hyperedge v l w i)
                   , oWeight :: Maybe w
                   } deriving Show

data K v l w i = K { kNode         :: v
                   , kEdge         :: Maybe (Hyperedge v l w i)
                   , kWeight       :: Maybe w
                   , kRank         :: Int
                   , kBackpointers :: [Int]
                   } deriving Show

data D v l w i = D { dNode         :: v
                   , dEdge         :: Maybe (Hyperedge v l w i)
                   , dWeight       :: Maybe w
                   , dBackpointers :: [Int]
                   } deriving Show

--mkI :: v -> I v l w i
--mkI v = I v 

weight :: Item v l w i -> Maybe w
weight (Inside (I _ _ w))       = w
weight (Outside (O _ _ w))      = w
weight (Ranked (K _ _ w _ _))   = w
weight (Derivation (D _ _ w _)) = w

edge :: Item v l w i -> Maybe (Hyperedge v l w i)
edge (Inside (I _ e _))       = e
edge (Outside (O _ e _))      = e
edge (Ranked (K _ e _ _ _))   = e
edge (Derivation (D _ e _ _)) = e

node :: Item v l w i -> v
node (Inside (I v _ _ ))      = v
node (Outside (O v _ _))      = v
node (Ranked (K v _ _ _ _))   = v
node (Derivation (D v _ _ _)) = v


-- | Chart of already explored items with their weights.
--   Implemented as a map assigning nodes to their corresponding inside,
--   outside, etc., items. Lists are sorted by /decreasing/ weights.
type Chart v l w i = M.Map v (ChartEntry v l w i)

-- | List of inside, outside, etc. items assigned to a node in the chart
data ChartEntry v l w i = CE 
  { ceInside     :: [I v l w i] -- Inside items
  , ceOutside    :: [O v l w i] -- Outside items
  , ceRanked     :: [K v l w i] -- Ranked derivation items
  , ceDerivation :: [D v l w i] -- Modified derivation items
  }

getItems :: Ord v => Chart v l w i -> Item v l w i -> [Item v l w i]
getItems c (Inside (I v _ _)) 
  = map Inside . filter ((v ==).iNode) . ceInside $ c ! v
getItems c (Outside (O v _ _)) 
  = map Outside  . filter ((v ==).oNode) . ceOutside $ c ! v
getItems c (Ranked (K v _ _ _ _)) 
  = map Ranked . filter ((v ==).kNode) . ceRanked $ c ! v
getItems c (Derivation (D v _ _ _)) 
  = map Derivation . filter ((v ==).dNode) . ceDerivation $ c ! v


data Rule v l w i = R
  { rRule :: (Item v l w i, [Item v l w i])
  , rWeight :: [w] -> w
  , rPriority :: [w] -> w
  }

rulesKAStar :: Hyperedge v l w i -> (v -> w) -> [Rule v l w i]
rulesKAStar e h = ins e 
                  ++ switch e
                  ++ concat [out e i | i <- [0 .. (pred . length . eTail $ e)]]
                  ++ build e
  where ins e = [R (I . eHead $ e, map I . eTail e)
                   (\ws -> sum ws + eWeight e)
                   (\ws -> sum ws + eWeight e + h . eHead $ e)]
                                                 
        switch e = error ""
        out e i = error ""
        build e = error ""


type Agenda v l w i = H.MinPrioHeap w (Item v l w i)


traceBackpointers 
  :: Ord v 
  => K v l w i
  -> Hypergraph v l w i
  -> Chart v l w i
  -> Maybe (T.Tree (Hyperedge v l w i))
-- traceBackpointers (K _ e _ bps _) graph chart 
--   = T.Node e $ map traceSubtree $ zip bps [0..]
--     where traceSubtree (rank, idx)  
--             = let precs = ceRanked $ chart ! (eTail e !! idx)
--               in traceBackpointers (precs !! (length precs - rank)) graph chart
traceBackpointers (K _ me _ _ bps) graph chart 
  = do
    e <- me
    T.Node e `fmap` mapM 
       (\(rank, idx) 
          -> let precs = ceRanked $ chart ! (eTail e !! idx)
             in traceBackpointers (precs !! (length precs - rank)) graph chart)
       (zip bps [0..])

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
  
-- | Helper function assigning to each node the list of edges with this
--   node in their tail, together with the according index.
--   Hopefully, this speeds things up.
edgesForward 
  :: Ord v
  => Hypergraph v l w i 
  -> M.Map v [(Hyperedge v l w i, Int)]
edgesForward graph 
  = L.foldl' (\m (a, b) -> M.insertWith' (++) a b m) M.empty . concatMap fwd $ edges
  where edges = concat . M.elems . edgesM $ graph
        fwd e = [(eTail e !! i, [(e, i)]) | i <- [0 .. pred . length . eTail $ e]]
        -- perhaps the strict versions of fold and insert make this more
        -- efficient... We will see.



-- Control.Exception.catch (fromJust Nothing) (\e -> print $ ("asdf" ++  show (e::Control.Exception.SomeException)))
--lol