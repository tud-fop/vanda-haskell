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
--import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Tree as T
import qualified Data.Heap as H 
import qualified Data.Ord as O
import qualified Data.List as L
import Data.Maybe (fromJust)


import Data.Hypergraph

data Assignment v l w i = Inside (I v l w i) w
                        | Outside (O v l w i) w
                        | Ranked (K v l w i) w
                          deriving (Show, Eq)

data I v l w i = I { iNode   :: v
                   } deriving (Show, Eq)

data O v l w i = O { oNode   :: v
                   } deriving (Show, Eq)

data K v l w i = K { kNode         :: v
                   , kEdge         :: Hyperedge v l w i
                   , kRank         :: Int
                   , kBackpointers :: [Int]
                   } deriving (Show, Eq)

isInside :: Assignment v l w i -> Bool
isInside (Inside _ _) = True
isInside _ = False

isOutside :: Assignment v l w i -> Bool
isOutside (Outside _ _) = True
isOutside _ = False

isRanked :: Assignment v l w i -> Bool
isRanked (Ranked _ _) = True
isRanked _ = False


weight :: Assignment v l w i -> w
weight (Inside _ w)   = w
weight (Outside  _ w) = w
weight (Ranked _ w)   = w

edge :: Assignment v l w i -> Maybe (Hyperedge v l w i)
edge (Ranked (K _ e _ _ ) _) = Just e
edge _                       = Nothing

node :: Assignment v l w i -> v
node (Inside (I v) _)       = v
node (Outside (O v) _)      = v
node (Ranked (K v _ _ _) _) = v

rank :: Assignment v l w i -> Int
rank (Ranked (K _ _ r _) _) = r
rank a = error $ "Tried to compute rank of non-ranked assignment " -- ++ show a
-- Or should I do this with maybe? I will have to signal error somewhere...


-- | Chart of already explored items with their weights.
--   Implemented as a map assigning nodes to their corresponding inside,
--   outside, etc., items. Lists are sorted by /decreasing/ weights.
type Chart v l w i = M.Map v (ChartEntry v l w i)

data ChartEntry v l w i = CE { ceInside :: [Assignment v l w i]
                             , ceOutside :: [Assignment v l w i]
                             , ceRanked :: [Assignment v l w i]
                             }

insideAssignments :: Ord v => Chart v l w i -> v -> [Assignment v l w i]
insideAssignments chart v = ceInside (chart ! v)


outsideAssignments :: Ord v => Chart v l w i -> v -> [Assignment v l w i]
outsideAssignments chart v = ceOutside (chart ! v)


rankedAssignments :: Ord v => Chart v l w i -> v -> [Assignment v l w i]
rankedAssignments chart v = ceRanked (chart ! v)


-- | @contains c a@ checks whether the chart @c@ already contains an assignment that is equal to @a@ (with no respect paid to the rank of ranked items).
contains 
  :: (Eq v, Ord v, Eq l, Eq w, Eq i) 
  => Chart v l w i 
  -> Assignment v l w i 
  -> Bool
chart `contains` (Inside (I v) _) = not . null $ insideAssignments chart v
chart `contains` (Outside (O v) _) = not . null $ outsideAssignments chart v
chart `contains` (Ranked (K v e _ bps) _) = not . any f $ rankedAssignments chart v
  where f (Ranked (K v' e' _ bps') _) = v' == v && e' == e && bps' == bps
        f _                           = False



type Agenda v l w i = H.MinPrioHeap w (Assignment v l w i)

-- | @chartInsert a c@ inserts assignment @a@ into chart @c@. Note that already present
--   inside and outside items are overwritten at the moment instead of raising an error
--   or an exception. Ranked items are annotated with their corresponding rank upon insertion.
chartInsert :: Ord v => Assignment v l w i -> Chart v l w i -> Chart v l w i
chartInsert ass c = M.alter (Just . update ass) (node ass) c
  where update a@(Inside _ _)  Nothing   = CE [a] [] []
        update a@(Outside _ _) Nothing   = CE [] [a] []
        update a@(Ranked _ _)  Nothing   = CE [] [] [a]
        update a@(Inside _ _)  (Just ce) = ce{ceInside = [a]} 
        --maybe exception when already non-nil
        update a@(Outside _ _) (Just ce) = ce{ceOutside = [a]}
        update a@(Ranked _ _)  (Just ce) = ce{ceRanked = annotate a:ceRanked ce}
        -- the above is ugly, perhaps it can be simplified with some monadic stuff
        annotate (Ranked (K v e r bps) w) 
          = Ranked (K v e (succ . length $ (rankedAssignments c v)) bps) w
        annotate x = x


-- | creates the initial assignments together with their priorities to be put on the agenda,
initialAssignments
  :: (Num w, Ord v, Eq l, Eq i)
  => Hypergraph v l w i
  -> (v -> w)
  -> [(w, Assignment v l w i)]
initialAssignments graph h
  = map ins . filter ((==0) . length . eTail) 
      . concat . M.elems . edgesM $ graph
    where ins e = let w = eWeight e
                      p = w + (h . eHead $ e)
                  in (p, Inside (I . eHead $ e) w)

-- | creates those new prioritized assignments to be put on the agenda that are using
--   the last popped assignment. TODO: maybe prune edges which are not related to lastAss
--   via precomputation
newAssignments 
  :: (Num w, Ord v, Eq l, Eq i) 
  => Chart v l w i 
  -> Hypergraph v l w i 
  -> Assignment v l w i 
  -> v 
  -> (v -> w) 
  -> [(w, Assignment v l w i)]
newAssignments chart graph lastAss goal h 
  = switch ++ ins ++ outs ++ builds
  where
    switch = do
      guard $ isInside lastAss && node lastAss /= goal
      ig <- ceInside (chart ! goal)
      return (weight ig, Outside (O goal) 0)
    ins = apply inhelper
    outs = apply outhelper
    builds = apply buildhelper
    apply f = concatMap f . concat . M.elems . edgesM $ graph
    inhelper e = do
      ibs <- mapM (insideAssignments chart) (eTail e)
      guard (lastAss `elem` ibs)
      let w = eWeight e + (sum . map weight $ ibs)
      let p = h (eHead e) + w
      return (p, Inside (I (eHead e)) w)
    outhelper e = do
      ibs <- mapM (insideAssignments chart) (eTail e)
      oa <- outsideAssignments chart $ eHead e
      let assmts = oa : ibs -- we might express this with liftM2, but would lose the names
      guard $ lastAss `elem` assmts
      i <- [0 .. (length ibs - 1)]
      let w = eWeight e + weight oa
                + (sum . map weight $ take i ibs) -- leave out i-th element
                + (sum . map weight $ drop (i + 1) ibs)
      let p = w + weight (ibs !! i)
      return (p, Outside (O (eTail e !! i)) w)
    buildhelper e = do
      oa <- outsideAssignments chart $ eHead e
      ibs <- if null $ eTail e  -- case distinction because list monad treats x <- [] as "failure"
             then return []
             else mapM (rankedAssignments chart) (eTail e)
      let assmts = oa : ibs
      guard $ lastAss `elem` assmts
      let w = eWeight e + (sum . map weight $ ibs)
      let p = w + weight oa
      let bps = map rank ibs
      return (p, Ranked (K (eHead e) e 0 bps) w)

--maybe wrap this into state monad
kastar
  :: (Num w, Ord w, Ord v, Eq l, Eq i) 
  => Int
  -> Hypergraph v l w i
  -> v
  -> (v -> w)
  --  -> [(T.Tree (Hyperedge v l w i), w)]
  ->[Assignment v l w i]
kastar k graph g h
  = execute M.empty (agendaInsert (initialAssignments graph h) (H.empty::Agenda v l w i))
    where execute chart agenda 
            = if done chart agenda
              then rankedAssignments chart g
              else let ((p, popped), agenda') = fromJust $ H.view agenda
                       (chart', agenda'') 
                         = if chart `contains` popped
                           then (chart, agenda')
                           else ( chartInsert popped chart
                                , agendaInsert (newAssignments chart graph popped g h) agenda)
                   in execute chart' agenda''
          done chart agenda = length (rankedAssignments chart g) >= k
                              || H.isEmpty agenda
          agendaInsert as a = L.foldl' (flip H.insert)  a as
                                      


-- traceBackpointers 
--   :: Ord v 
--   => K v l w i
--   -> Hypergraph v l w i
--   -> Chart v l w i
--   -> Maybe (T.Tree (Hyperedge v l w i))
-- -- traceBackpointers (K _ e _ bps _) graph chart 
-- --   = T.Node e $ map traceSubtree $ zip bps [0..]
-- --     where traceSubtree (rank, idx)  
-- --             = let precs = ceRanked $ chart ! (eTail e !! idx)
-- --               in traceBackpointers (precs !! (length precs - rank)) graph chart
-- traceBackpointers (K _ me _ _ bps) graph chart 
--   = do
--     e <- me
--     T.Node e `fmap` mapM 
--        (\(rank, idx) 
--           -> let precs = ceRanked $ chart ! (eTail e !! idx)
--              in traceBackpointers (precs !! (length precs - rank)) graph chart)
--        (zip bps [0..])

traceBackpointers 
  :: Ord v 
  => Assignment v l w i 
  -> Chart v l w i
  -> Maybe (T.Tree (Hyperedge v l w i), w)
traceBackpointers (Ranked k  w) c = do
  t <- helper k c
  return (t, w)
  where helper (K _ e _ bps) c = T.Node e `fmap` mapM
          (\(rank, idx) 
             -> let precs = rankedAssignments c (eTail e !! idx)
                in fst `fmap`  traceBackpointers (precs !! (length precs - rank)) c)
          (zip bps [0..])
traceBackpointers _ _ = Nothing


-- -- | @kbest k g h G@ computes a list of @k@ best derivations of the goal
-- --   node @g@ in the hypergraph @G@. It uses the supplied heuristic
-- --   @h@ for efficiency, applying the KA* algorithm.
-- kbest
--   :: (Ord v, Ord w, Num w)
--   => Int      -- ^ The number @k@ of derivations of @g@ to be searched for
--   -> v        -- ^ The goal node @g@ of @G@
--   -> (v -> w) -- ^ A heuristic function @h@, must be admissible and 
--               -- consistent, this precondition is /not/ checked!
--   -> Hypergraph v l w i 
--               -- ^ The hypergraph @G@ in which the search is performed
--   -> [(T.Tree (Hyperedge v l w i), w)] 
--               -- ^ A List of @k@ best derivations, with their weights
-- kbest = error "not imp"
  
-- -- | Helper function assigning to each node the list of edges with this
-- --   node in their tail, together with the according index.
-- --   Hopefully, this speeds things up.
-- edgesForward 
--   :: Ord v
--   => Hypergraph v l w i 
--   -> M.Map v [(Hyperedge v l w i, Int)]
-- edgesForward graph 
--   = L.foldl' (\m (a, b) -> M.insertWith' (++) a b m) M.empty . concatMap fwd $ edges
--   where edges = concat . M.elems . edgesM $ graph
--         fwd e = [(eTail e !! i, [(e, i)]) | i <- [0 .. pred . length . eTail $ e]]
--         -- perhaps the strict versions of fold and insert make this more
--         -- efficient... We will see.



-- Control.Exception.catch (fromJust Nothing) (\e -> print $ ("asdf" ++  show (e::Control.Exception.SomeException)))
--lol



------------------------------------------------------------------------------

  