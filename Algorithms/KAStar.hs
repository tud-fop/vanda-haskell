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

{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Algorithms.KAStar
  where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.List
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Tree as T
import qualified Data.Heap as H 
import qualified Data.List as L
import Data.Maybe (fromJust, mapMaybe)
import Debug.Trace

import Data.Hypergraph
import qualified TestData.TestHypergraph as Test

------------------------------------------------------------------------------
-- Deduction rules -----------------------------------------------------------
------------------------------------------------------------------------------

-- | Create the initial assignments together with their priorities to be put 
--   on the agenda
initialAssignments
  :: (Num w, Ord v, Eq l, Eq i)
  => KAStar p v l w i [(w, Assignment v l w i)]
initialAssignments = do
  g <- graph
  h <- heuristic
  return $ map (ins h) . filter ((==0) . length . eTail) 
      . concat . M.elems . edgesM $ g
    where ins h e = let w = eWeight e
                        p = w * (h . eHead $ e)
                    in (p, Inside (I . eHead $ e) w)


-- The specialize pragma makes GHC additionally compile instantiated, 
-- therefore maybe faster versions of the supplied function
-- {-# SPECIALIZE newAssignments 
--   :: Assignment Char Char Double () 
--   -> KAStar Char Char Double () 
--      [(Double, Assignment Char Char Double ())] #-}
-- {-# SPECIALIZE newAssignments
--  :: Assignment Char String Double () 
--  -> KAStar Char String Double () 
--     [(Double, Assignment Char String Double ())] #-}
-- | Create those new prioritized assignments to be put on the agenda that 
--   are using the last popped assignment, the /trigger/.
newAssignments 
  :: (Num w, Ord v, Eq l, Eq i)
  => Assignment v l w i 
  -> KAStar p v l w i [(w, Assignment v l w i)]
newAssignments trigger = do 
  h <- heuristic
  g <- goal
  c <- chart
  is <- inEdges $ node trigger
  os <- otherEdges $ node trigger
  inE <- cfgInEdges `liftM` ask
  return $ case trigger of 
             (Inside  _ _) -> switchRule c g trigger 
                              ++ ins c h trigger is 
                              ++ outs c trigger os
             (Outside _ _) -> outs c trigger os 
                              ++ builds c trigger inE os
             (Ranked  _ _) -> builds c trigger inE os
    where
      ins c h trigger        = concatMap (inRule c h trigger)
      outs c trigger         = concatMap (outRule c trigger)
      builds c trigger is os = concatMap (buildRuleO c trigger) os 
                               ++ buildRuleL c trigger is
                               ++ buildRuleR c trigger


-- | The Switch-rule generates the initial outside item for the goal node @g@,
--   triggered by an inside assignment for @g@.
switchRule
  :: (Num w, Ord v, Eq l, Eq i) 
  => Chart v l w i
  -> v
  -> Assignment v l w i
  -> [(w, Assignment v l w i)]
switchRule c g trigger = do
  guard $ isInside trigger && node trigger == g
  ig <- insideAssignments c g
  return $! (weight ig, Outside (O g) 1)


-- | The In-rule generates a new inside assignment from the inside assignments
--   for its tail nodes. Only triggered by an inside assignment.
inRule
  :: (Num w, Ord v, Eq l, Eq i)
  => Chart v l w i
  -> (v -> w)
  -> Assignment v l w i
  -> (Hyperedge v l w i, Int)
  -> [(w, Assignment v l w i)]
inRule c h trigger (e, r) = do
        ibsl <- mapM (insideAssignments c) . take r $ eTail e
        ibsr <- mapM (insideAssignments c) . drop (r + 1) $ eTail e
        let ibs = ibsl ++ [trigger] ++ ibsr
            w   = eWeight e * (product . map weight $ ibs)
            p   = h (eHead e) * w
        return $! (p, Inside (I (eHead e)) w)


-- | The Out-rule generates a new outside assignment from a list of inside
--   asssignments and an outside assignment. Triggered by an outside or inside
--   assignment.
outRule 
  :: (Num w, Ord v, Eq l, Eq i)
  => Chart v l w i 
  -> Assignment v l w i 
  -> (Hyperedge v l w i, Int) 
  -> [(w, Assignment v l w i)]
outRule c trigger (e, r) = do
  (oa, ibs) <- if r == 0 
               then do
                 guard $ isOutside trigger
                 liftM2 (,) [trigger] (mapM (insideAssignments c) (eTail e))
               else do
                 guard $ isInside trigger
                 ibsl <- mapM (insideAssignments c) . take (r - 1) $ eTail e
                 ibsr <- mapM (insideAssignments c) . drop r $ eTail e
                 liftM2 (,) (outsideAssignments c $ eHead e) 
                            [ibsl ++ [trigger] ++ ibsr]
  i <- [0 .. (length ibs - 1)]
  let w = eWeight e * weight oa
          * (product . map weight $ take i ibs) -- drop i-th element
          * (product . map weight $ drop (i + 1) ibs)
      p = w * weight (ibs !! i)
  return $! (p, Outside (O (eTail e !! i)) w)

-- | buildRuleO is triggered by an outside assignment for node @v@. We then 
--   find all edges with @v@ as head node and combine the 1-best derivations
--   of their tail nodes.
buildRuleO
  :: (Num w, Ord v, Eq l, Eq i)
  => Chart v l w i 
  -> Assignment v l w i 
  -> (Hyperedge v l w i, Int) 
  -> [(w, Assignment v l w i)]
buildRuleO c trigger@(Outside _ _) (e, 0) = do
  as <- mapM (flip (nthRankedAssignment c) 1) (eTail e)
  let w   = eWeight e * (product . map weight $ as)
      p   = w * weight trigger
      bps = map rank as
  return $! (p, Ranked (K (eHead e) e 0 bps) w)
buildRuleO c trigger@(Ranked _ _) (e, r) = do
  guard $ r /= 0 && rank trigger == 1
  oa <- outsideAssignments c $ eHead e
  asl <- zipWithM (nthRankedAssignment c) (take (r - 1) $ eTail e) (repeat 1)
  asr <- zipWithM (nthRankedAssignment c) (drop r $ eTail e) (repeat 1)
  let as  = asl ++ [trigger] ++ asr
      w   = eWeight e * (product . map weight $ as)
      p   = w * weight oa
      bps = map rank as
  return $! (p, Ranked (K (eHead e) e 0 bps) w)
buildRuleO _ _ _ = []


-- | buildRuleL is triggered by an @r@-ranked derivation assignment for node 
--   @v@. We find all assignments with @(r-1)@-backpointers to @v@ and
--   combine them with the trigger.
buildRuleL 
  :: (Num w, Ord v, Eq l, Eq i) 
  => Chart v l w i 
  -> Assignment v l w i 
  -> M.Map v [(Hyperedge v l w i, Int)]
  -> [(w, Assignment v l w i)]
buildRuleL c trigger@(Ranked _ _) inEdges 
  = concatMap rule (inEdges ! node trigger)
  where 
    rule (e, s) = do
      Ranked (K _ e' _ bps) _ <- rankedAssignments c (eHead e)
      guard $ e' == e && bps !! s == rank trigger - 1
      asl <- zipWithM (nthRankedAssignment c) (eTail e) (take s bps)
      asr <- zipWithM (nthRankedAssignment c) (eTail e) (drop (s + 1) bps)
      oa <- outsideAssignments c (eHead e)
      let as = asl ++ [trigger] ++ asr
          w  = eWeight e * (product . map weight $ as)
          p  = w * weight oa
          bps = map rank as
      return $! (p, Ranked (K (eHead e) e 0 bps) w)
buildRuleL _ _ _ = []

-- | buildRuleR is triggered by a ranked derivation assignment for node @v@
--   with backpointers @bps@. We decide for all of those backpointers if the
--   next-best ranked assignment was inserted and return the newly generated
--   assignments.
buildRuleR 
  :: (Num w, Ord v, Eq l, Eq i) 
  => Chart v l w i 
  -> Assignment v l w i 
  -> [(w, Assignment v l w i)]
buildRuleR c trigger@(Ranked (K _ e _ bps) _) = do
  r <- [1 .. length bps]
  let bps' = zipWith (+) bps (unit (length bps) r)
  as <- zipWithM (nthRankedAssignment c) (eTail e) bps'
  oa <- outsideAssignments c (eHead e)
  let w = eWeight e * (product . map weight $ as)
      p = w * weight oa
  return $! (p, Ranked (K (eHead e) e 0 bps') w)
  where
    -- r-th unit vector of dimension n
    unit n r = replicate (r - 1) 0 ++ [1] ++ replicate (n - r) 0
buildRuleR _ _ = []

------------------------------------------------------------------------------
-- Algorithm State -----------------------------------------------------------
------------------------------------------------------------------------------

-- | Monad stack for state and configuration of the KA* algorithm
newtype KAStar p v l w i a = KAStar {
      runK :: ReaderT (KAConfig v l w i) (State (KAState p v l w i)) a
    } deriving ( Monad
               , MonadReader (KAConfig v l w i)
               , MonadState (KAState p v l w i)
               )


-- | Holds configuration for KA*
data KAConfig v l w i = KAConfig { 
    cfgNumDeriv   :: Int 
    -- ^ number @k@ of derivations to be searched for
  , cfgGraph      :: Hypergraph v l w i 
    -- ^ underlying graph
  , cfgGoal       :: v 
    -- ^ goal node whose derivations are searched
  , cfgHeuristic  :: v -> w 
    -- ^ the supplied heuristic function
  , cfgInEdges    :: M.Map v [(Hyperedge v l w i, Int)] 
    -- ^ assigns tail-nodes to their hyperedge, together with their position
  , cfgOtherEdges :: M.Map v [(Hyperedge v l w i, Int)]
    -- ^ as 'cfgInEdges', but also for the edge's head node
  }


-- | Data structure holding run-time state of KA*
data KAState p v l w i = KAState { 
      stChart          :: Chart v l w i 
      -- ^ chart holds processed assignments
    , stAgenda         :: Agenda p v l w i 
      -- ^ pqueue with prioritized assignments waiting to be processed
    , stItemsInserted  :: Int
      -- ^ number of assignments inserted into chart
    , stItemsGenerated :: Int
      -- ^ number of assignments inserted into agenda
    }


-- | Runs the KAStar monad, similar to runState
runKAStar :: KAStar p v l w i a
          -> Agenda p v l w i
          -> Int 
          -> Hypergraph v l w i
          -> v
          -> (v -> w)
          -> M.Map v [(Hyperedge v l w i, Int)]
          -> M.Map v [(Hyperedge v l w i, Int)]
          -> (a, KAState p v l w i)
runKAStar kst agenda k graph goal heuristic ins others =
    let cfg   = KAConfig k graph goal heuristic ins others
        state = KAState M.empty agenda 0 0
    in runState (runReaderT (runK kst) cfg) state


-- | Returns number of derivations searched for
numDeriv :: KAStar p v l w i Int
numDeriv = cfgNumDeriv `liftM` ask


-- | Returns underlying graph
graph :: KAStar p v l w i (Hypergraph v l w i)
graph = cfgGraph `liftM` ask


-- | Returns goal node
goal :: KAStar p v l w i v
goal = cfgGoal `liftM` ask


-- | Returns supplied heuristic
heuristic :: KAStar p v l w i (v -> w)
heuristic = cfgHeuristic `liftM` ask


-- | Returns in-edges data structure, see 'cfgInEdges'
inEdges :: Ord v => v -> KAStar p v l w i [(Hyperedge v l w i, Int)]
inEdges v = ((!v) . cfgInEdges) `liftM` ask


-- | Returns other-edges, see 'cfgOtherEdges'
otherEdges :: Ord v => v -> KAStar p v l w i [(Hyperedge v l w i, Int)]
otherEdges v = ((!v) . cfgOtherEdges) `liftM` ask


-- | Returns momentary chart
chart :: KAStar p v l w i (Chart v l w i)
chart = stChart `liftM` get

-- | Returns the agenda
agenda :: KAStar p v l w i (Agenda p v l w i)
agenda = stAgenda `liftM` get


-- | Set the chart
putChart :: Chart v l w i -> KAStar p v l w i ()
putChart c = do
  st <- get
  put st{stChart = c}


-- | Set the agenda
putAgenda :: Agenda p v l w i -> KAStar p v l w i ()
putAgenda a = do
  st <- get
  put st{stAgenda = a}


-- | Increment number of inserted assignments
incItemsInserted :: KAStar p v l w i ()
incItemsInserted = do 
  st <- get
  put st{stItemsInserted = stItemsInserted st + 1}


-- | Increment number of generated assignments by @n@
incItemsGenerated :: Int -> KAStar p v l w i ()
incItemsGenerated n = do
  st <- get
  put st{stItemsGenerated = stItemsGenerated st + n}


-- | @chartInsert a@ inserts assignment @a@ into the chart, provided it does 
--   not already contain it (In this case, @Nothing@ is returned).
--   Ranked items are annotated with their corresponding rank upon insertion.
--   The number of inserted assignments is updated accordingly.
chartInsert 
  :: (Ord v, Eq l, Eq w, Eq i) 
  => Assignment v l w i 
  -> KAStar p v l w i (Maybe (Assignment v l w i))
chartInsert assgmt = do
  b <- chartContains assgmt
  if b then return Nothing
       else do
         incItemsInserted
         Just `liftM` insert assgmt
  where
    insert a = do
      c <- chart
      putChart $ M.alter (Just . update c a) (node a) c
      return $ rk c a
    update c a@(Inside _ _)  Nothing   = CE [a] [] []
    update c a@(Outside _ _) Nothing   = CE [] [a] []
    update c a@(Ranked _ _)  Nothing   = CE [] [] [rk c a]
    update c a@(Inside _ _)  (Just ce) = ce{ceInside = [a]} 
    --maybe exception when already non-nil
    update c a@(Outside _ _) (Just ce) = ce{ceOutside = [a]}
    update c a@(Ranked _ _)  (Just ce) = ce{ceRanked = rk c a:ceRanked ce}
    -- the above is ugly, perhaps it can be simplified with monadic mojo
    rk c (Ranked (K v e r bps) w) = 
      Ranked (K v e (succ . length $ rankedAssignments c v) bps) w
    rk _ x = x


-- | @chartContains a@ checks whether the chart already contains an 
--   assignment that is equal to @a@ (with no respect paid to the rank 
--   of ranked items).
chartContains 
  :: (Ord v, Eq l, Eq w, Eq i) 
  => Assignment v l w i -> KAStar p v l w i Bool
chartContains (Inside (I v) _) = (not . null) `liftM` insideM v
chartContains (Outside (O v) _) = (not . null) `liftM` outsideM v
chartContains r@(Ranked (K v _ _ _) _) = (r `elem`) `liftM` rankedM v


-- | @agendaInsert as@ inserts the list @as@ of prioritized assignments
--   into the current agenda, updating the number of generated assignments
--   accordingly.
agendaInsert 
  :: (Ord v, Ord w, H.HeapItem p (w, Assignment v l w i)) 
  => [(w, Assignment v l w i)] -> KAStar p v l w i ()
agendaInsert as = do 
  incItemsGenerated $ length as
  putAgenda =<< flip (L.foldl' (flip H.insert)) as `liftM` agenda
         

-- | @process@ pops assignments until
--
--   (1) there are none left or we have found the necessary number of
--       derivations of @g@, returning @Nothing@ OR
--
--   (2) the popped assignment is not contained in the chart. In this case,
--       it is inserted and returned with its according rank.
process 
  :: (Ord v, Ord w, Eq i, Eq l, H.HeapItem p (w, Assignment v l w i)) 
  => KAStar p v l w i (Maybe (Assignment v l w i))
process = do
  d <- done
  if d then return Nothing 
       else do -- agenda != empty
         ((p, popped), agenda') <- (fromJust . H.view) `liftM` agenda
         putAgenda agenda'
         trigger <- chartInsert popped
         case trigger of
           Nothing -> process
           _       -> return trigger
  where done = do
          e <- H.isEmpty `liftM` agenda
          l <- length `liftM` (rankedM =<< goal)
          k <- numDeriv
          return $ e || l >= k

-- | @kbest graph g h k@ finds the @k@ best derivations of the goal 
-- node @g@ in @graph@, applying the heuristic function @h@.
kbest 
  :: (Num w, Ord v, Ord w, Eq l, Eq i)
  => Hypergraph v l w i
  -> v
  -> (v -> w)
  -> Int
  -> [(T.Tree (Hyperedge v l w i), w)]
kbest = kastar (H.empty :: H.MaxPrioHeap w (Assignment v l w i))

-- | @kworst graph g h k@ finds the @k@ worst derivations of the goal 
-- node @g@ in @graph@, applying the heuristic function @h@.
kworst
  :: (Num w, Ord v, Ord w, Eq l, Eq i)
  => Hypergraph v l w i
  -> v
  -> (v -> w)
  -> Int
  -> [(T.Tree (Hyperedge v l w i), w)]
kworst = kastar (H.empty :: H.MinPrioHeap w (Assignment v l w i))

-- | @kastar agenda graph g h k@ finds the @k@ best derivations of the goal 
-- node @g@ in @graph@, applying the heuristic function @h@. "Best" thereby
-- means best with respect to the order induced by the @agenda@.
kastar
  :: (Num w, Ord v, Ord w, Eq l, Eq i, H.HeapItem p (w, Assignment v l w i))
  => Agenda p v l w i
  -> Hypergraph v l w i
  -> v
  -> (v -> w)
  -> Int
  -> [(T.Tree (Hyperedge v l w i), w)]
kastar agenda graph g h k 
  = --trace ("Inserted " ++ (show . stItemsInserted $ info) 
    --  ++ " assignments, generated " ++ (show . stItemsGenerated $ info) 
    --  ++ "assignments.\n")
    (reverse $ mapMaybe (traceBackpointers res) $ rankedAssignments res g)
  where (res, info) = runKAStar kst agenda k graph g h ins others
        (ins, others) = edgesForward graph
        kst = do
          agendaInsert =<< initialAssignments
          loop
        loop = do
          m <- process  -- try to pop new assignment
          case m of
            Nothing -> chart  -- we're done, return the chart
            (Just trigger) -> (agendaInsert =<< newAssignments trigger) 
                              >> loop -- generate new assignments and continue


-- | @traceBackpoiners chart a@ reconstructs a derivation from the backpointers
--   contained in the ranked derivation assignment @a@. If @a@ is not such an
--   assignment, it returns @Nothing@.
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
traceBackpointers _ _ = Nothing


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


-- When we check if the chart contains a ranked item, we disregard its rank.
instance (Eq v, Eq l, Eq w, Eq i) => Eq (K v l w i) where
  (K v e _ bps) == (K v' e' _ bps') = v == v' && e == e' && bps == bps'


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
rank a = error "Tried to compute rank of non-ranked assignment"
-- Or should I do this with maybe? I will have to signal error somewhere...

-- | Returns backpointers of an asssignment
--   /Nota bene:/ raises error if assignment doesn't contain a rank!
backpointers :: Assignment v l w i -> [Int]
backpointers (Ranked (K _ _ _ bps) _) = bps
backpointers _ = error "Tried to compute rank of non-ranked assignment "


-- | Chart of already explored items with their weights.
--   Implemented as a map assigning nodes to their corresponding inside,
--   outside, etc., items. Lists are sorted by /increasing/ weights.
type Chart v l w i = M.Map v (ChartEntry v l w i)


-- | Entry of the chart, with inside, outside and ranked assignments for the
--   node.
data ChartEntry v l w i = CE { ceInside  :: [Assignment v l w i]
                             , ceOutside :: [Assignment v l w i]
                             , ceRanked  :: [Assignment v l w i]
                             } deriving Show


-- | @insideAssignments c v@ returns inside assignments for the node @v@ 
--   in chart @c@
insideAssignments :: Ord v => Chart v l w i ->  v -> [Assignment v l w i]
insideAssignments c v = maybe [] ceInside $ M.lookup v c


-- | @outsideAssignments c v@ returns outside assignments for the node @v@ 
--   in chart @c@
outsideAssignments :: Ord v => Chart v l w i -> v -> [Assignment v l w i]
outsideAssignments c v = maybe [] ceOutside $ M.lookup v c


-- | @rankedAssignments c v@ returns ranked assignments for the node @v@ 
--   in chart @c@
rankedAssignments :: Ord v => Chart v l w i -> v -> [Assignment v l w i]
rankedAssignments c v = maybe [] ceRanked $ M.lookup v c


-- | @nthRankedAssignment c v n@ gets the @n@-ranked assignment for 
--   the node @v@ from chart @c@, returned in a singleton list.
--   If there is no such assignment, the function returns @[]@.
--   Useful for code in the list monad.
nthRankedAssignment 
  :: Ord v => Chart v l w i -> v -> Int -> [Assignment v l w i]
nthRankedAssignment c v n = as !!! (l - n)
  where 
    as           = rankedAssignments c v
    l            = length as
    []     !!! _ = []
    (x:_)  !!! 0 = [x]
    (_:xs) !!! n = xs !!! (n-1)

-- | @insideAssignments@ lifted into KAStar monad
insideM :: Ord v => v -> KAStar p v l w i [Assignment v l w i]
insideM v = flip insideAssignments v `liftM` chart


-- | @outsideAssignments@ lifted into KAStar monad
outsideM :: Ord v => v -> KAStar p v l w i [Assignment v l w i]
outsideM v = flip outsideAssignments v `liftM` chart


-- | @rankedAssignments@ lifted into KAStar monad
rankedM :: Ord v => v -> KAStar p v l w i [Assignment v l w i]
rankedM v = flip rankedAssignments v `liftM` chart


-- TODO: make abstract so that kbest and kworst possible
type Agenda p v l w i = H.Heap p (w, Assignment v l w i)


-- TODO: maybe delete this
-- -- | Helper function computing the size of the chart
-- chartSize :: KAStar p v l w i Int
-- chartSize = M.fold ls 0 `liftM` chart
--   where ls it l = length (ceInside it) + length (ceOutside it) 
--                   + length (ceRanked it) + l


------------------------------------------------------------------------------
-- Helper functions ----------------------------------------------------------
------------------------------------------------------------------------------

-- | Computes data structures for efficient association of tail nodes with
--   their respective hyperedges.
edgesForward
  :: Ord v
  => Hypergraph v l w i
  -> ( M.Map v [(Hyperedge v l w i, Int)]
     , M.Map v [(Hyperedge v l w i, Int)])
edgesForward graph = (compute ins, compute others)
  where 
    compute f = L.foldl' (\m (a, b) -> M.insertWith' (++) a b m) M.empty 
                . concatMap f $ edges
    ins e = [(eTail e !! i, [(e, i)]) | i <- [0 .. pred . length . eTail $ e]]
    others e = (eHead e, [(e, 0)]) 
               : [(eTail e !! i, [(e, i + 1)]) 
                    | i <- [0 .. pred . length . eTail $ e]]
    edges = concat . M.elems . edgesM $ graph


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
  mapM_ (putStrLn . uncurry str) $ kbest graph goal h k
    where str t w = "w = " ++ show w ++ "\n" 
                    ++ (T.drawTree . fmap drawHyperedge $ t)


-- Let's check the reference implementation
t' graph goal k = do
  putStrLn $ drawHypergraph graph
  mapM_ (putStrLn . uncurry str) $ nBest' k goal graph
    where str t w = "w = " ++ show w ++ "\n" 
                    ++ (T.drawTree . fmap drawHyperedge $ t)


t1 = t test1 'g' heur1 20


t2 = t test2 'g' heur1 400


t3 = kbest test2 'g' heur1 400

t4 = t (Test.testHypergraphs !! 1) 'S' heur1 10

comparison 
  :: (Fractional w, Ord v, Ord w, Eq i, Eq l, Show i, Show l, Show v) 
  => Hypergraph v l w i -> v -> (v -> w) -> Int -> IO Bool
comparison graph goal heur k = zipWithM put mine others >>= return . and
  where put w1 w2 = do 
          putStrLn $ show w1 ++ "\t" ++ show w2
          return $ w1 == w2
        mine = map snd $ kbest graph goal heur k
        others = nBest k goal graph

diff
  :: (Fractional w, Ord v, Ord w, Eq i, Eq l, Show i, Show l, Show v) 
  => Hypergraph v l w i 
  -> v 
  -> (v -> w) 
  -> Int 
  -> [((T.Tree (Hyperedge v l w i), w), (T.Tree (Hyperedge v l w i), w))]
diff graph goal heur k = filter neq $ zip mine others
  where neq ((_, w1), (_, w2)) = w1 /= w2
        mine = kbest graph goal heur k
        others = nBest' k goal graph

test :: IO ()
--test = comparison (Test.testHypergraphs !! 1) 'S' heur1 10 >>= putStrLn . show
--test = t3 `seq` return ()
--test = t' (Test.testHypergraphs !! 1) 'S' 10
test = mapM_ (uncurry pr) $ diff (Test.testHypergraphs !! 1) 'S' heur1 20
  where 
    pr l r = do
      putStrLn "===MINE==="
      putStrLn . uncurry str $ l
      putStrLn "===OTHER==="
      putStrLn . uncurry str $ r
    str t w = "w = " ++ show w ++ "\n" 
              ++ (T.drawTree . fmap drawHyperedge $ t)