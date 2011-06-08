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
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Tree as T
import qualified Data.Heap as H 
import qualified Data.List as L
import Data.Maybe (fromJust, mapMaybe)
import Debug.Trace


import Data.Hypergraph


------------------------------------------------------------------------------
-- Data Structures -----------------------------------------------------------
------------------------------------------------------------------------------

-- | Assignments associate an item with a weight. Used as data structure in
--   agenda and chart.
data Assignment v l w i = Inside (I v l w i) w
                        | Outside (O v l w i) w
                        | Ranked (K v l w i) w
                          deriving (Show, Eq)

-- | Inside items of a node
data I v l w i = I { iNode   :: v
                   } deriving (Show, Eq)

-- | Outside items of a node
data O v l w i = O { oNode   :: v
                   } deriving (Show, Eq)

-- | Ranked derivation items of a node. Possess edge and backpointers for
--   derivation reconstruction as well as a rank.
data K v l w i = K { kNode         :: v
                   , kEdge         :: Hyperedge v l w i
                   , kRank         :: Int
                   , kBackpointers :: [Int]
                   } deriving (Show)

-- When we check if the chart contains a ranked item, we disregard its rank.
instance (Eq v, Eq l, Eq w, Eq i) => Eq (K v l w i) where
  (K v e _ bps) == (K v' e' _ bps') = v == v' && e == e' && bps == bps'

-- maybe we can do without the following three functions
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

-- | /Nota bene:/ raises error if assignment doesn't contain a rank!
rank :: Assignment v l w i -> Int
rank (Ranked (K _ _ r _) _) = r
rank a = error "Tried to compute rank of non-ranked assignment " -- ++ show a
-- Or should I do this with maybe? I will have to signal error somewhere...


-- | Chart of already explored items with their weights.
--   Implemented as a map assigning nodes to their corresponding inside,
--   outside, etc., items. Lists are sorted by /decreasing/ weights.
type Chart v l w i = M.Map v (ChartEntry v l w i)

data ChartEntry v l w i = CE { ceInside :: [Assignment v l w i]
                             , ceOutside :: [Assignment v l w i]
                             , ceRanked :: [Assignment v l w i]
                             } deriving Show

-- actual accessor functions for a chart
insideAssignments :: Ord v => Chart v l w i -> v -> [Assignment v l w i]
insideAssignments chart v = maybe [] ceInside $ M.lookup v chart 


outsideAssignments :: Ord v => Chart v l w i -> v -> [Assignment v l w i]
outsideAssignments chart v = maybe [] ceOutside $ M.lookup v chart 


rankedAssignments :: Ord v => Chart v l w i -> v -> [Assignment v l w i]
rankedAssignments chart v = maybe [] ceRanked $ M.lookup v chart

-- TODO: make abstract so that kbest and kworst possible
type Agenda v l w i = H.MaxPrioHeap w (Assignment v l w i)


-- | @c `contains` a@ checks whether the chart @c@ already contains an 
--   assignment that is equal to @a@ (with no respect paid to the rank 
--   of ranked items).
contains 
  :: (Eq v, Ord v, Eq l, Eq w, Eq i) 
  => Chart v l w i 
  -> Assignment v l w i 
  -> Bool
chart `contains` (Inside (I v) _) = not . null $ insideAssignments chart v
chart `contains` (Outside (O v) _) = not . null $ outsideAssignments chart v
chart `contains` r@(Ranked (K v _ _ _) _) = r `elem` rankedAssignments chart v


-- | @chartInsert a c@ inserts assignment @a@ into chart @c@. Note that already 
--   present inside and outside items are overwritten at the moment instead of 
--   raising an error or an exception. Ranked items are annotated with their 
--   corresponding rank upon insertion.
chartInsert :: Ord v => Assignment v l w i -> Chart v l w i -> (Chart v l w i, Assignment v l w i)
chartInsert ass c = (M.alter (Just . update ass) (node ass) c, rk ass)
  where update a@(Inside _ _)  Nothing   = CE [a] [] []
        update a@(Outside _ _) Nothing   = CE [] [a] []
        update a@(Ranked _ _)  Nothing   = CE [] [] [rk a]
        update a@(Inside _ _)  (Just ce) = ce{ceInside = [a]} 
        --maybe exception when already non-nil
        update a@(Outside _ _) (Just ce) = ce{ceOutside = [a]}
        update a@(Ranked _ _)  (Just ce) = ce{ceRanked = rk a:ceRanked ce}
        -- the above is ugly, perhaps it can be simplified with monadic mojo
        rk (Ranked (K v e r bps) w) 
          = Ranked (K v e (succ . length $ rankedAssignments c v) bps) w
        rk x = x

chartSize :: Chart v l w i -> Int
chartSize = M.fold ls 0
  where ls it l = (length $ ceInside it) + (length $ ceOutside it) + (length $ ceRanked it) + l

------------------------------------------------------------------------------
-- KA* Algorithm -------------------------------------------------------------
------------------------------------------------------------------------------

-- | creates the initial assignments together with their priorities to be put 
--   on the agenda,
initialAssignments
  :: (Num w, Ord v, Eq l, Eq i)
  => Hypergraph v l w i
  -> (v -> w)
  -> [(w, Assignment v l w i)]
initialAssignments graph h
  = map ins . filter ((==0) . length . eTail) 
      . concat . M.elems . edgesM $ graph
    where ins e = let w = eWeight e
                      p = w * (h . eHead $ e)
                  in (p, Inside (I . eHead $ e) w)


-- The specialize pragma makes GHC additionally compile instantiated, therefore faster,
-- versions of the supplied function
{-# SPECIALIZE newAssignments 
  :: Chart Char Char Double () 
  -> Hypergraph Char Char Double () 
  -> Assignment Char Char Double () 
  -> Char 
  -> (Char -> Double) 
  -> M.Map Char [(Hyperedge Char Char Double (), Int)]
  -> M.Map Char [(Hyperedge Char Char Double (), Int)]
  -> [(Double, Assignment Char Char Double ())]#-}
{-# SPECIALIZE newAssignments :: Chart Char String Double () 
 -> Hypergraph Char String Double () 
 -> Assignment Char String Double () 
 -> Char 
 -> (Char -> Double) 
 -> M.Map Char [(Hyperedge Char String Double (), Int)]
 -> M.Map Char [(Hyperedge Char String Double (), Int)]
 -> [(Double, Assignment Char String Double ())]#-}
-- | creates those new prioritized assignments to be put on the agenda that 
--   are using the last popped assignment. 
newAssignments 
  :: (Num w, Ord v, Eq l, Eq i)
  => Chart v l w i 
  -> Hypergraph v l w i 
  -> Assignment v l w i 
  -> v 
  -> (v -> w) 
  -> M.Map v [(Hyperedge v l w i, Int)]
  -> M.Map v [(Hyperedge v l w i, Int)]
  -> [(w, Assignment v l w i)]
newAssignments chart graph lastAss goal h inEdges otherEdges
  = case lastAss of 
      (Inside  _ _) -> switch ++ ins ++ outs
      (Outside _ _) -> outs ++ builds
      (Ranked  _ _) -> builds
    where
      switch = do
        guard $ isInside lastAss && node lastAss == goal
        ig <- insideAssignments chart goal
        return $! (weight ig, Outside (O goal) 1)
      ins = {-# SCC "ins" #-} concatMap inhelper (inEdges ! node lastAss)
      outs = {-# SCC "outs" #-} concatMap outhelper (otherEdges ! node lastAss)
      builds = {-# SCC "builds" #-} concatMap buildhelper (otherEdges ! node lastAss)
      inhelper (e, r) = do
        ibsl <- mapM (insideAssignments chart) . take r $ eTail e
        ibsr <- mapM (insideAssignments chart) . drop (r + 1) $ eTail e
        let ibs = ibsl ++ [lastAss] ++ ibsr
        let w = eWeight e * (product . map weight $ ibs)
        let p = h (eHead e) * w
        return $! (p, Inside (I (eHead e)) w)
      -- Put on your protective googles, the following code is weird.
      -- outhelper :: (Hyperedge v l w i, Int) -> [(w, Assignment v l w i)]
      outhelper (e, r) = do
        (oa, ibs) <- if r == 0 
                     then do
                       guard $ isOutside lastAss
                       liftM2 (,) [lastAss] (mapM (insideAssignments chart) (eTail e))
                     else do
                       guard $ isInside lastAss
                       ibsl <- mapM (insideAssignments chart) . take (r - 1) $ eTail e
                       ibsr <- mapM (insideAssignments chart) . drop r $ eTail e
                       liftM2 (,) (outsideAssignments chart $ eHead e) [ibsl ++ [lastAss] ++ ibsr]
        i <- [0 .. (length ibs - 1)]
        let w = eWeight e * weight oa
                  * (product . map weight $ take i ibs) -- drop i-th element
                  * (product . map weight $ drop (i + 1) ibs)
        let p = w * weight (ibs !! i)
        return $! (p, Outside (O (eTail e !! i)) w)
      buildhelper (e, r) = do
        (oa, ibs) <- if null $ eTail e
                     then liftM2 (,) [lastAss] (return [])
                     else if r == 0 
                          then do
                            guard $ isOutside lastAss
                            liftM2 (,) [lastAss] (mapM (rankedAssignments chart) (eTail e))
                          else do 
                            guard $ isRanked lastAss
                            ibsl <- mapM (rankedAssignments chart) . take (r - 1) $ eTail e
                            ibsr <- mapM (rankedAssignments chart) . drop r $ eTail e
                            liftM2 (,) (outsideAssignments chart $ eHead e) [ibsl ++ [lastAss] ++ ibsr]
        let w = eWeight e * (product . map weight $ ibs)
        let p = w * weight oa
        let bps = map rank ibs
        return $! (p, Ranked (K (eHead e) e 0 bps) w)

--maybe wrap this into state monad
kastar
  :: (Num w, Ord w, Ord v, Eq l, Eq i)
  => Int
  -> Hypergraph v l w i
  -> v
  -> (v -> w)
  -> [(T.Tree (Hyperedge v l w i), w)]
  -- -> Chart v l w i
kastar k graph g h 
  = reverse $ mapMaybe (traceBackpointers res) $ rankedAssignments res g
  --kastar k graph g h = res
  where res = execute M.empty $ agendaInsert (initialAssignments graph h) 
                                             (H.empty::Agenda v l w i)
        --execute chart agenda | trace ("c: " ++ show chart 
        --                       ++ "\na: " ++ show agenda ++"\n") False 
        --                                     = undefined
        execute chart agenda 
            = if done chart agenda
              then trace ("|chart| = " ++ show (chartSize chart) ++ ", |agenda| = " ++ show (H.size agenda) ++ "\n") chart
              else let ((p, popped), agenda') = fromJust $ H.view agenda
                       (chart', agenda'') 
                         = if chart `contains` popped
                           then (chart, agenda')
                           else let (chart'', lastAss) = chartInsert popped chart --ugly
                                in ( chart''
                                   , agendaInsert (newAssignments chart''
                                                                  graph
                                                                  lastAss
                                                                  g
                                                                  h
                                                                  (fst edgeMap)
                                                                  (snd edgeMap)) 
                                                   agenda' -- ugliER!! ^^
                                   )
                   in execute chart' agenda''
        done chart agenda = length (rankedAssignments chart g) >= k
                              || H.isEmpty agenda
        agendaInsert as a = L.foldl' (flip H.insert) a as
        edgeMap = edgesForward graph
                                      

traceBackpointers 
  :: Ord v 
  => Chart v l w i
  -> Assignment v l w i 
  -> Maybe (T.Tree (Hyperedge v l w i), w)
traceBackpointers c a@(Ranked _  w) = do
  t <- helper a
  return (t, w)
  where helper (Ranked (K _ e _ bps) _) = T.Node e `fmap` mapM
          (\(rank, idx) 
             -> let precs = rankedAssignments c (eTail e !! idx)
                in helper (precs !! (length precs - rank)))
          (zip bps [0..])
        helper _ = Nothing


------------------------------------------------------------------------------
-- Test cases ----------------------------------------------------------------
------------------------------------------------------------------------------

test1 = hypergraph [ hyperedge 'g' "ab" ' ' 1.0 ()
                   , hyperedge 'a' ""   ' ' 1.0 ()
                   , hyperedge 'b' ""   ' ' 1.0 ()
                   , hyperedge 'g' "g"  ' ' 0.9 ()
                   ]


heur1 :: Fractional w => Char -> w
heur1 _ = 1.0


test2 = hypergraph [ hyperedge 'a' ""   "alpha"   1.0 ()
                   , hyperedge 'b' ""   "beta"    1.0 ()
                   , hyperedge 'g' "ab" "sigma"   0.8 ()
                   , hyperedge 'a' "g"  "delta"   0.9 ()
                   , hyperedge 'b' "g"  "epsilon" 0.8 ()
                   ]



t graph goal h k = do
  putStrLn $ drawHypergraph graph
  mapM_ (putStrLn . uncurry str) $ kastar k graph goal h
    where str t w = "w = " ++ show w ++ "\n" 
                    ++ (T.drawTree . fmap drawHyperedge $ t)


t1 = t test1 'g' heur1 20


t2 = t test2 'g' heur1 400

t3 = kastar 400 test2 'g' heur1


test :: IO ()
--test = t2
test = t3 `seq` return ()


------------------------------------------------------------------------------
-- Stuff that might still be useful ------------------------------------------
------------------------------------------------------------------------------

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

edgesForward
  :: Ord v
  => Hypergraph v l w i
  -> ( M.Map v [(Hyperedge v l w i, Int)]
     , M.Map v [(Hyperedge v l w i, Int)])
edgesForward graph 
  = (compute ins, compute others)
    where compute f = L.foldl' (\m (a, b) -> M.insertWith' (++) a b m) M.empty . concatMap f $ edges
          ins e = [(eTail e !! i, [(e, i)]) | i <- [0 .. pred . length . eTail $ e]]
          others e = (eHead e, [(e, 0)]) : [(eTail e !! i, [(e, i + 1)]) | i <- [0 .. pred. length . eTail $ e]]
          edges = concat . M.elems . edgesM $ graph


--ghc -O2 -fexcess-precision -fvia-C -optc-O2 --make Main.hs
