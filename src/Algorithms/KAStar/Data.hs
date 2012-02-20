-- (c) Johannes Osterholzer <Johannes.Osterholzer@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

module Algorithms.KAStar.Data
  (
    Agenda
  , Assignment (..)
  , I (..)
  , O (..)
  , K (..)
  , isInside
  , isOutside
  , isRanked
  , weight
  , edge
  , node
  , rank
  , Chart (..)
  , EdgeMapEntry (..)
  , insideAssignments
  , outsideAssignments
  , rankedAssignments
  , numRanked
  , nthRankedAssignment
  , rankedWithBackpointer
  , eqHyperedges
  ) where

import qualified Data.Map as M
import qualified Data.Heap as H
import qualified Data.Sequence as S
import Data.Foldable (toList)

import Data.Hypergraph


-- | The agenda we store yet-to-be-processed assignments on
type Agenda p v l w i = H.Heap p (w, Assignment v l w i)


-- | Assignments associate an item with a weight. Used as data structure in
--   agenda and chart.
data Assignment v l w i = Inside (I v l w i) w
                        | Outside (O v l w i) w
                        | Ranked (K v l w i) w
                          deriving (Show, Eq)


-- | Inside items of a node
newtype I v l w i = I { iNode   :: v
                      } deriving (Show, Eq)


-- | Outside items of a node
newtype O v l w i = O { oNode   :: v
                      } deriving (Show, Eq)


-- | Ranked derivation items of a node. Possess edge and backpointers for
--   derivation reconstruction as well as a rank.
data K v l w i = K { kNode         :: v
                   , kEdge         :: Hyperedge v l w i
                   , kRank         :: Int
                   , kBackpointers :: [Int]
                   } deriving (Show)


-- When we check if the chart already contains a ranked item, we disregard its rank.
instance (Eq v, Eq l, Eq w, Eq i) => Eq (K v l w i) where
  (K v e _ bps) == (K v' e' _ bps') = v == v' && eqHyperedges e e' && bps == bps'


-- | @True@ iff inside assignment
isInside :: Assignment v l w i -> Bool
isInside (Inside _ _) = True
isInside _ = False


-- | @True@ iff outside assignment
isOutside :: Assignment v l w i -> Bool
isOutside (Outside _ _) = True
isOutside _ = False


-- | @True@ iff ranked assignment
isRanked :: Assignment v l w i -> Bool
isRanked (Ranked _ _) = True
isRanked _ = False


-- | Weight of an assignment
weight :: Assignment v l w i -> w
weight (Inside _ w)   = w
weight (Outside  _ w) = w
weight (Ranked _ w)   = w


-- | Returns edge for ranked assignments, @Nothing@ else
edge :: Assignment v l w i -> Maybe (Hyperedge v l w i)
edge (Ranked (K _ e _ _ ) _) = Just e
edge _                       = Nothing


-- | Returns the node contained in the assignment
node :: Assignment v l w i -> v
node (Inside (I v) _)       = v
node (Outside (O v) _)      = v
node (Ranked (K v _ _ _) _) = v


-- | Returns rank of an asssignment
--   /Nota bene:/ raises error if assignment doesn't contain a rank!
rank :: Assignment v l w i -> Int
rank (Ranked (K _ _ r _) _) = r
rank _ = error "Tried to compute rank of non-ranked assignment"
-- TODO: Or should I do this with maybe? I will have to signal error somewhere...

-- | Chart of already explored items with their weights.
--   'cEdgeMap' is a map assigning nodes to their corresponding inside,
--   outside, etc., items. Lists are sorted by /increasing/ weights.
--   'cBPMap' holds for each key @(id, i, v)@ those ranked assignments with
--   edge id @id@ whose @i@-th backpointer has rank @v@.
data Chart v l w i = C { cEdgeMap :: M.Map v (EdgeMapEntry v l w i)
                       , cBPMap   :: M.Map (i, Int, Int)
                                           [Assignment v l w i]
                       }


-- | Entry of the chart, with inside, outside and ranked assignments for the
--   node.
data EdgeMapEntry v l w i = EM { emInside  :: [Assignment v l w i]
                               , emOutside :: [Assignment v l w i]
                               , emRanked  :: S.Seq (Assignment v l w i)
                               } deriving Show


-- | @insideAssignments c v@ returns inside assignments for the node @v@
--   in chart @c@
insideAssignments :: Ord v => Chart v l w i ->  v -> [Assignment v l w i]
insideAssignments c v = maybe [] emInside . M.lookup v $ cEdgeMap c


-- | @outsideAssignments c v@ returns outside assignments for the node @v@
--   in chart @c@
outsideAssignments :: Ord v => Chart v l w i -> v -> [Assignment v l w i]
outsideAssignments c v = maybe [] emOutside . M.lookup v $ cEdgeMap c


-- | @rankedAssignments c v@ returns ranked assignments for the node @v@
--   in chart @c@
rankedAssignments :: Ord v => Chart v l w i -> v -> [Assignment v l w i]
rankedAssignments c v = maybe [] (toList . emRanked) . M.lookup v $ cEdgeMap c

-- | @numRanked c v@ returns the number of assignments for node @v@ in
--   chart @c@
numRanked :: Ord v => Chart v l w i -> v -> Int
numRanked c v = maybe 0 (S.length . emRanked) . M.lookup v $ cEdgeMap c


-- | @nthRankedAssignment c v n@ gets the @n@-ranked assignment for
--   the node @v@ from chart @c@, returned in a singleton list.
--   If there is no such assignment, the function returns @[]@.
--   This is useful for code in the list monad.
nthRankedAssignment :: Ord v
                    => Chart v l w i -> v
                    -> Int -> [Assignment v l w i]
nthRankedAssignment c v n = if n >= 1 && n <= S.length s
                            then [s `S.index` (S.length s - n)]
                            else []
  where s = maybe S.empty emRanked . M.lookup v $ cEdgeMap c


-- | @rankedWithBackpointer c e i v@ returns all ranked assignments
--   constructed from hyperedge @e@ whose @i$-th backpointer
--   points to the corresponding @v@-ranked assignment, with chart @c@.
rankedWithBackpointer :: Ord i
                      => Chart v l w i -> (Hyperedge v l w i)
                      -> Int -> Int -> [Assignment v l w i]
rankedWithBackpointer c e bp val = M.findWithDefault [] (eId e, bp, val) (cBPMap c)

-- | checks two `Hyperedge`s for equality. Note that we only compare IDs.
eqHyperedges :: Eq i => Hyperedge v l w i -> Hyperedge v l w i -> Bool
eqHyperedges e1 e2 = eId e1 == eId e2

