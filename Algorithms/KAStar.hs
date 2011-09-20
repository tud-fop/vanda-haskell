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

{-# LANGUAGE FlexibleContexts #-}

module Algorithms.KAStar
  where

import Control.Monad
import Control.Monad.Reader (ask)
import qualified Data.Map as M
import qualified Data.Tree as T
import qualified Data.Heap as H 
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Debug.Trace

import Data.Hypergraph
import qualified TestData.TestHypergraph as Test

import Control.DeepSeq

import Algorithms.KAStar.State
import Algorithms.KAStar.Data

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


-- | Create those new prioritized assignments to be put on the agenda that 
--   are using the last popped assignment, the /trigger/.
newAssignments 
  :: (Num w, Ord v, Ord l, Ord w, Ord i)
  => Assignment v l w i 
  -> KAStar p v l w i [(w, Assignment v l w i)]
newAssignments trigger = do 
  h   <- heuristic
  g   <- goal
  c   <- chart
  is  <- inEdges $ node trigger
  os  <- otherEdges $ node trigger
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

-- | Checks whether the nodes of the supplied assignments are compatible
--   with the tail nodes of the supplied hyperedge.
--   May be removed later.
assert :: Eq v => [Assignment v l w i] -> Hyperedge v l w i -> Bool
assert as e = and (zipWith f as $ eTail e) && length as == length (eTail e)
  where f a v = node a == v

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
  unless (assert as e) (error "error in buildRuleO") --TODO:remove
  let w   = eWeight e * (product . map weight $ as)
      p   = w * weight trigger
      bps = map rank as
  return $! (p, Ranked (K (eHead e) e 0 bps) w)
buildRuleO c trigger@(Ranked _ _) (e, r) = do
  guard $ r /= 0 && rank trigger == 1
  oa  <- outsideAssignments c $ eHead e
  asl <- zipWithM (nthRankedAssignment c) (take (r - 1) $ eTail e) (repeat 1)
  asr <- zipWithM (nthRankedAssignment c) (drop r $ eTail e) (repeat 1)
  let as  = asl ++ [trigger] ++ asr
      w   = eWeight e * (product . map weight $ as)
      p   = w * weight oa
      bps = map rank as
  unless (assert as e) (error "error in buildRuleO") --TODO:remove
  return $! (p, Ranked (K (eHead e) e 0 bps) w)
buildRuleO _ _ _ = []


-- | buildRuleL is triggered by an @r@-ranked derivation assignment for node 
--   @v@. We find all assignments with @(r-1)@-backpointers to @v@ and
--   combine them with the trigger.
buildRuleL 
  :: (Num w, Ord v, Ord l, Ord w, Ord i)
  => Chart v l w i 
  -> Assignment v l w i 
  -> M.Map v [(Hyperedge v l w i, Int)]
  -> [(w, Assignment v l w i)]
buildRuleL c trigger@(Ranked _ _) inEdges 
  = concatMap rule (M.findWithDefault [] (node trigger) inEdges)
  where 
    rule (e, s) = do
      Ranked (K _ e' _ bps) _ <- rankedWithBackpointer c e (s+1) (rank trigger - 1)
      unless (e' == e && bps !! s == rank trigger - 1) $
        error "error with rankedWithBackpointer in buildRuleL" --TODO: remove
      asl <- take s `liftM` zipWithM (nthRankedAssignment c) (eTail e) bps
      asr <- drop (s+1) `liftM` zipWithM (nthRankedAssignment c) (eTail e) bps
      oa  <- outsideAssignments c (eHead e)
      let as  = asl ++ [trigger] ++ asr
          w   = eWeight e * (product . map weight $ as)
          p   = w * weight oa
          bps = map rank as
      unless (assert as e) (error "error in buildRuleL") --TODO: remove
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
  unless (assert as e) (error "error in buildRuleO") --TODO: remove
  let w = eWeight e * (product . map weight $ as)
      p = w * weight oa
  return $! (p, Ranked (K (eHead e) e 0 bps') w)
  where
    -- r-th unit vector of dimension n
    unit n r = replicate (r - 1) 0 ++ [1] ++ replicate (n - r) 0
buildRuleR _ _ = []



-- | @kbest graph g h k@ finds the @k@ best derivations of the goal 
-- node @g@ in @graph@, applying the heuristic function @h@.
kbest 
  :: (Num w, Ord v, Ord w, Ord l, Ord i)
  => Hypergraph v l w i
  -> v
  -> (v -> w)
  -> Int
  -> [(T.Tree (Hyperedge v l w i), w)]
kbest = kastar (H.empty :: H.MaxPrioHeap w (Assignment v l w i))


-- | @kworst graph g h k@ finds the @k@ worst derivations of the goal 
-- node @g@ in @graph@, applying the heuristic function @h@.
kworst
  :: (Num w, Ord v, Ord w, Ord l, Ord i)
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
  :: (Num w, Ord v, Ord w, Ord l, Ord i, H.HeapItem p (w, Assignment v l w i))
  => Agenda p v l w i
  -> Hypergraph v l w i
  -> v
  -> (v -> w)
  -> Int
  -> [(T.Tree (Hyperedge v l w i), w)]
kastar agenda graph g h k 
  = --trace ("Inserted " ++ (show . stItemsInserted $ info) 
    --  ++ " assignments, generated " ++ (show . stItemsGenerated $ info) 
    --  ++ "assignments.\n") $
    reverse $ mapMaybe (traceBackpointers res) $ rankedAssignments res g
  where (res, info)   = runKAStar kst agenda k graph g h ins others
        (ins, others) = edgesForward graph
        kst = do
          agendaInsert =<< initialAssignments
          loop
        loop = do
          m <- process  -- try to pop new assignment
          case m of
            Nothing        -> chart  -- we're done, return the chart
            (Just trigger) -> (agendaInsert =<< newAssignments trigger) 
                              >> loop -- generate new assignments and continue




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
    compute f = foldl' (\m (a, b) -> M.insertWith' (++) a b m) M.empty 
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
                   , hyperedge 'g' "g"  "loop"    1.0 ()
                   , hyperedge 'x' "g"  "horst"   0.5 ()
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


t2 = t test2 'x' heur1 20

t3 :: [(T.Tree (Hyperedge Char Char Double ()), Double)]
t3 = kbest (Test.testHypergraphs !! 1) 'S' heur1 1000

t3' :: [(T.Tree (Hyperedge Char Char Double ()), Double)]
t3' = nBest' 1000 'S' (Test.testHypergraphs !! 1)

t4 = t (Test.testHypergraphs !! 1) 'S' heur1 10

comparison 
  :: (Fractional w, Ord v, Ord w, Ord i, Ord l, Show i, Show l, Show v) 
  => Hypergraph v l w i -> v -> (v -> w) -> Int -> IO Bool
comparison graph goal heur k = and `liftM` zipWithM put mine others
  where put w1 w2 = do 
          putStrLn $ show w1 ++ "\t" ++ show w2
          return $ w1 == w2
        mine = map snd $ kbest graph goal heur k
        others = nBest k goal graph

diff
  :: (Fractional w, Ord v, Ord w, Ord i, Ord l, Show i, Show l, Show v) 
  => Hypergraph v l w i 
  -> v 
  -> (v -> w) 
  -> Int 
  -> [((T.Tree (Hyperedge v l w i), w), (T.Tree (Hyperedge v l w i), w))]
diff graph goal heur k = filter neq $  zip mine others
  where neq ((_, w1), (_, w2)) = w1 /= w2
        mine = kbest graph goal heur k
        others = nBest' k goal graph

test :: IO ()
test = comparison (Test.testHypergraphs !! 1) 'S' heur1 10 >>= putStrLn . show
--test = t3 `deepseq` return ()
--test = t (Test.testHypergraphs !! 1) 'S' heur1 500
--test = (zipWith (\graph goal -> kbest graph goal (heur1::Char->Double) 1000) 
--                Test.testHypergraphs "AStt")
--       `deepseq` return ()
--test = mapM_ (uncurry go) (tail $ zip Test.testHypergraphs "AStt")
  -- where
  --   go graph start = mapM_ (uncurry pr) $ diff graph start heur1 50
  --   pr l r = do
  --     putStrLn "===MINE==="
  --     putStrLn . uncurry str $ l
  --     putStrLn "===OTHER==="
  --     putStrLn . uncurry str $ r
  --   str t w = "w = " ++ show w ++ "\n" 
  --             ++ (T.drawTree . fmap drawHyperedge $ t)
