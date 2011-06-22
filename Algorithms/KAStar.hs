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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

------------------------------------------------------------------------------
-- Deduction rules -----------------------------------------------------------
------------------------------------------------------------------------------

-- | creates the initial assignments together with their priorities to be put 
--   on the agenda,
initialAssignments
  :: (Num w, Ord v, Eq l, Eq i)
  => KAStar v l w i [(w, Assignment v l w i)]
initialAssignments = do
  g <- graph
  h <- heuristic
  return $ map (ins h) . filter ((==0) . length . eTail) 
      . concat . M.elems . edgesM $ g
    where ins h e = let w = eWeight e
                        p = w * (h . eHead $ e)
                    in (p, Inside (I . eHead $ e) w)


-- The specialize pragma makes GHC additionally compile instantiated, therefore faster,
-- versions of the supplied function
{-# SPECIALIZE newAssignments 
  :: Assignment Char Char Double () 
  -> KAStar Char Char Double () [(Double, Assignment Char Char Double ())]#-}
{-# SPECIALIZE newAssignments
 :: Assignment Char String Double () 
 -> KAStar Char String Double () [(Double, Assignment Char String Double ())]#-}
-- | creates those new prioritized assignments to be put on the agenda that 
--   are using the last popped assignment. 
newAssignments 
  :: (Num w, Ord v, Eq l, Eq i)
  => Assignment v l w i 
  -> KAStar v l w i [(w, Assignment v l w i)]
newAssignments trigger = do 
  h <- heuristic
  g <- goal
  c <- chart
  is <- inEdges $ node trigger
  os <- otherEdges $ node trigger
  return $ case trigger of 
             (Inside  _ _) -> switchRule c g trigger ++ ins c h trigger is ++ outs c trigger os
             (Outside _ _) -> outs c trigger os ++ builds c trigger os
             (Ranked  _ _) -> builds c trigger os
    where
      ins c h trigger = concatMap (inRule c h trigger)
      outs c trigger = concatMap (outRule c trigger)
      builds c trigger = concatMap (buildRule c trigger)


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
        let w = eWeight e * (product . map weight $ ibs)
        let p = h (eHead e) * w
        return $! (p, Inside (I (eHead e)) w)


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
                       liftM2 (,) (outsideAssignments c $ eHead e) [ibsl ++ [trigger] ++ ibsr]
        i <- [0 .. (length ibs - 1)]
        let w = eWeight e * weight oa
                  * (product . map weight $ take i ibs) -- drop i-th element
                  * (product . map weight $ drop (i + 1) ibs)
        let p = w * weight (ibs !! i)
        return $! (p, Outside (O (eTail e !! i)) w)


buildRule
  :: (Num w, Ord v, Eq l, Eq i)
  => Chart v l w i 
  -> Assignment v l w i 
  -> (Hyperedge v l w i, Int) 
  -> [(w, Assignment v l w i)]
buildRule c trigger (e, r) = do
        (oa, ibs) <- if null $ eTail e
                     then liftM2 (,) [trigger] (return [])
                     else if r == 0 
                          then do
                            guard $ isOutside trigger
                            liftM2 (,) [trigger] (mapM (rankedAssignments c) (eTail e))
                          else do 
                            guard $ isRanked trigger
                            ibsl <- mapM (rankedAssignments c) . take (r - 1) $ eTail e
                            ibsr <- mapM (rankedAssignments c) . drop r $ eTail e
                            liftM2 (,) (outsideAssignments c $ eHead e) [ibsl ++ [trigger] ++ ibsr]
        let w = eWeight e * (product . map weight $ ibs)
        let p = w * weight oa
        let bps = map rank ibs
        return $! (p, Ranked (K (eHead e) e 0 bps) w)


-----------------------------------------------------------------------------
-- Algorithm State -----------------------------------------------------------
------------------------------------------------------------------------------

newtype KAStar v l w i a = KAStar {
      runK :: ReaderT (KAConfig v l w i) (State (KAState v l w i)) a
    } deriving (Monad, MonadReader (KAConfig v l w i), MonadState (KAState v l w i))


data KAConfig v l w i = KAConfig { 
    cfgNumDeriv   :: Int
  , cfgGraph      :: Hypergraph v l w i
  , cfgGoal       :: v
  , cfgHeuristic  :: v -> w
  , cfgInEdges    :: M.Map v [(Hyperedge v l w i, Int)]
  , cfgOtherEdges :: M.Map v [(Hyperedge v l w i, Int)]
  }


data KAState v l w i = KAState { 
      stChart          :: Chart v l w i
    , stAgenda         :: Agenda v l w i
    , stItemsInserted  :: Int
    , stItemsGenerated :: Int
    }


runKAStar :: KAStar v l w i a
          -> Int 
          -> Hypergraph v l w i
          -> v
          -> (v -> w)
          -> M.Map v [(Hyperedge v l w i, Int)]
          -> M.Map v [(Hyperedge v l w i, Int)]
          -> (a, KAState v l w i)
runKAStar kst k graph goal heuristic ins others =
    let cfg   = KAConfig k graph goal heuristic ins others
        state = KAState M.empty (H.empty::Agenda v l w i) 0 0
    in runState (runReaderT (runK kst) cfg) state


numDeriv :: KAStar v l w i Int
numDeriv = cfgNumDeriv `liftM` ask


graph :: KAStar v l w i (Hypergraph v l w i)
graph = cfgGraph `liftM` ask


goal :: KAStar v l w i v
goal = cfgGoal `liftM` ask


heuristic :: KAStar v l w i (v -> w)
heuristic = cfgHeuristic `liftM` ask


inEdges :: Ord v => v -> KAStar v l w i [(Hyperedge v l w i, Int)]
inEdges v = ((!v) . cfgInEdges) `liftM` ask


otherEdges :: Ord v => v -> KAStar v l w i [(Hyperedge v l w i, Int)]
otherEdges v = ((!v) . cfgOtherEdges) `liftM` ask


chart :: KAStar v l w i (Chart v l w i)
chart = stChart `liftM` get


putChart :: Chart v l w i -> KAStar v l w i ()
putChart c = do
  st <- get
  put st{stChart = c}


putAgenda :: Agenda v l w i -> KAStar v l w i ()
putAgenda a = do
  st <- get
  put st{stAgenda = a}


agenda :: KAStar v l w i (Agenda v l w i)
agenda = stAgenda `liftM` get


incItemsInserted :: KAStar v l w i ()
incItemsInserted = do 
  st <- get
  put st{stItemsInserted = stItemsInserted st + 1}


incItemsGenerated :: Int -> KAStar v l w i ()
incItemsGenerated n = do
  st <- get
  put st{stItemsGenerated = stItemsGenerated st + n}


process :: (Ord v, Ord w, Eq i, Eq l) => KAStar v l w i (Maybe (Assignment v l w i))
process = do
  d <- done
  if d then return Nothing 
       else do -- agenda != empty
         ((p, popped), agenda') <- (fromJust . H.view) `liftM` agenda
         putAgenda agenda'
         trigger <- chartInsert' popped
         case trigger of
           Nothing -> process
           _       -> return trigger
  where done = do
          e <- H.isEmpty `liftM` agenda
          l <- length `liftM` (rankedM =<< goal)
          k <- numDeriv
          return $ e || l >= k


chartInsert' :: (Ord v, Eq i, Eq w, Eq l) => Assignment v l w i -> KAStar v l w i (Maybe (Assignment v l w i))
chartInsert' assgmt = do
  b <- chartContains assgmt
  if b then return Nothing
       else do
         incItemsInserted
         Just `liftM` chartInsert assgmt


agendaInsert :: (Ord v, Ord w) => [(w, Assignment v l w i)] -> KAStar v l w i ()
agendaInsert as = do 
  incItemsGenerated $ length as
  putAgenda =<< flip (L.foldl' (flip H.insert)) as `liftM` agenda
         

kastar graph g h k = reverse $ mapMaybe (traceBackpointers res) $ rankedAssignments res g
  where res = fst $ runKAStar kst k graph g h ins others
        (ins, others) = edgesForward graph
        kst = do
          agendaInsert =<< initialAssignments
          loop
        loop = do
          m <- process
          case m of
            Nothing -> chart
            (Just trigger) -> (agendaInsert =<< newAssignments trigger) >> loop

                 
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
insideAssignments :: Ord v => Chart v l w i ->  v -> [Assignment v l w i]
insideAssignments c v = maybe [] ceInside $ M.lookup v c


outsideAssignments :: Ord v => Chart v l w i -> v -> [Assignment v l w i]
outsideAssignments c v = maybe [] ceOutside $ M.lookup v c


rankedAssignments :: Ord v => Chart v l w i -> v -> [Assignment v l w i]
rankedAssignments c v = maybe [] ceRanked $ M.lookup v c


insideM :: Ord v => v -> KAStar v l w i [Assignment v l w i]
insideM v = flip insideAssignments v `liftM` chart


outsideM :: Ord v => v -> KAStar v l w i [Assignment v l w i]
outsideM v = flip outsideAssignments v `liftM` chart


rankedM :: Ord v => v -> KAStar v l w i [Assignment v l w i]
rankedM v = flip rankedAssignments v `liftM` chart


-- TODO: make abstract so that kbest and kworst possible
type Agenda v l w i = H.MaxPrioHeap w (Assignment v l w i)


-- | @c `contains` a@ checks whether the chart @c@ already contains an 
--   assignment that is equal to @a@ (with no respect paid to the rank 
--   of ranked items).
-- contains 
--   :: (Eq v, Ord v, Eq l, Eq w, Eq i) 
--   => Chart v l w i 
--   -> Assignment v l w i 
--   -> Bool
-- chart `contains` (Inside (I v) _) = not . null $ insideAssignments chart v
-- chart `contains` (Outside (O v) _) = not . null $ outsideAssignments chart v
-- chart `contains` r@(Ranked (K v _ _ _) _) = r `elem` rankedAssignments chart v
chartContains :: (Eq v, Ord v, Eq l, Eq w, Eq i) => Assignment v l w i -> KAStar v l w i Bool
chartContains (Inside (I v) _) = (not . null) `liftM` insideM v
chartContains (Outside (O v) _) = (not . null) `liftM` outsideM v
chartContains r@(Ranked (K v _ _ _) _) = (r `elem`) `liftM` rankedM v


-- | @chartInsert a c@ inserts assignment @a@ into chart @c@. Note that already 
--   present inside and outside items are overwritten at the moment instead of 
--   raising an error or an exception. Ranked items are annotated with their 
--   corresponding rank upon insertion.
chartInsert :: Ord v => Assignment v l w i -> KAStar v l w i (Assignment v l w i)
chartInsert ass = do
  c <- chart
  putChart $ M.alter (Just . update c ass) (node ass) c
  return $ rk c ass
    where update c a@(Inside _ _)  Nothing   = CE [a] [] []
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


chartSize :: KAStar v l w i Int
chartSize = M.fold ls 0 `liftM` chart
  where ls it l = length (ceInside it) + length (ceOutside it) + length (ceRanked it) + l


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
  mapM_ (putStrLn . uncurry str) $ kastar graph goal h k
    where str t w = "w = " ++ show w ++ "\n" 
                    ++ (T.drawTree . fmap drawHyperedge $ t)

t1 = t test1 'g' heur1 20


t2 = t test2 'g' heur1 400

t3 = kastar test2 'g' heur1 400


test :: IO ()
test = t2
--test = t3 `seq` return ()

