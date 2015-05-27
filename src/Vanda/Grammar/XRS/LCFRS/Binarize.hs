module Vanda.Grammar.XRS.LCFRS.Binarize where

import           Control.Exception.Base (assert)
import           Control.Monad.State.Strict
import qualified Data.Array as A
import           Data.List (intercalate, intersperse, elemIndex)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust, catMaybes, listToMaybe)
import qualified Data.Set as S
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Tree as T
import           Data.Tuple (swap)
import           Text.Printf

import           Data.NTT
import           Vanda.Hypergraph.IntHypergraph
import qualified Vanda.Hypergraph.Tree as VT
import           Vanda.Corpus.Negra.Text (parseNegra)
import           Vanda.Util.Memorysavers (invertMap)

import           Vanda.Grammar.XRS.LCFRS
import           Vanda.Grammar.XRS.LCFRS.Extraction
import           Vanda.Grammar.XRS.LCFRS.Evaluation

import Debug.Trace

---------------------------------------------------------------------------------
-- ProtoRules are necessary to check for duplicates when binarizing many rules --
---------------------------------------------------------------------------------

type ProtoNT = Either (String, Int) Int -- unintified ProtoNTs have to carry their fanout
type ProtoRule = ((ProtoNT, [ProtoNT]), [[NTT]])

ruleToProto :: (Rule, Double) -> (ProtoRule, Double)
ruleToProto (((lhs, rhs), h'), d) = (((Right lhs, map Right rhs), h'), d)

intifyProtoRules :: M.Map String Int -> [(ProtoRule, Double)] -> ([(Rule, Double)], M.Map String Int)
intifyProtoRules m_in rs = runState (mapM intifyProtoAction rs) m_in
  where
    intifyProtoAction :: (ProtoRule, Double) -> State (M.Map String Int) (Rule, Double)
    intifyProtoAction (((lhs, rhs), h'), d)
      = do newLhs <- intifyProtoNT lhs
           newRhs <- mapM intifyProtoNT rhs
           return (((newLhs, newRhs), h'), d)
    intifyProtoNT :: ProtoNT -> State (M.Map String Int) Int
    intifyProtoNT (Right i) = return i
    intifyProtoNT (Left (s, _))  = do oldmap <- get
                                      case M.lookup s oldmap of
                                        Just i  -> return i
                                        Nothing -> let i = M.size oldmap
                                                   in put (M.insert s i oldmap) >> return i


------------------------------------------------------------------------------------------
-- Actual Binarization = extraction of two NTs into a new rule leaving a remaining rule --
------------------------------------------------------------------------------------------

getRk, getFo :: (((a, [a]), [[NTT]]), Double) -> Int
getRk (((_, rhs),  _), _) = length rhs
getFo (((_, _  ), h'), _) = length h'

getH :: [[NTT]] -> [Maybe NTT]
getH = intercalate [Nothing] . map (map Just)
getNTOnlyH :: [[NTT]] -> [Maybe Int]
getNTOnlyH = map (fmap (\(NT b) -> b))
           . filter (\m -> case m of Just (T _) -> False; _ -> True)
           . getH

getRhs :: (((l, [a]), [[NTT]]), Double) -> [a]
getRhs (((_, rhs), _), _) = rhs

getNTPosOfVar
  :: [Int] -- ^ fanout "annotated" rhs containing only the fanouts
  -> Int -- ^ variable at which we search
  -> Int -- ^ position of NT in rhs
getNTPosOfVar fanoutAnnotatedRHS v
  = let searchWorker i ((pos, fo) : xs) = if i + fo > v
                                          then pos
                                          else searchWorker (i + fo) xs
    in searchWorker 0 $ zip [0..] $ fanoutAnnotatedRHS

data TriState = Yes | Perhaps | No deriving Show

extractFromRule
  :: A.Array Int Int -- ^ fanouts
  -> (ProtoRule, Double) -- ^ rule
  -> (Int, Int) -- ^ indices of the NTs that are to be extracted (ascending order unnecessary, makes calling this function easier)
  -> ((ProtoRule, Double), (ProtoRule, Double)) -- ^ extracted rule, remainder rule
extractFromRule fanouts (((lhs, rhs), h'), d) (posBa, posBb) = (innerRule, remainderRule)
  where
    minmax :: Ord a => a -> a -> (a, a)
    minmax a b = (min a b, max a b)
    (posB1, posB2) = minmax posBa posBb
    -- Conventions I would like to adhere to: 'v' for variables in the homomorphisms, 'n' for NT positions, 'b[i]' for NTs and no more ambiguous 'i's
    -- capital H homomorphisms are akin to the characteristic strings you can create from tuples by replacing the tuples commas with delimiters (Nothing)
    [b1, b2] = [rhs !! posB1, rhs !! posB2]
    outerNTs = let (preB1, postB1) = splitAt posB1 rhs
                   (preB2, postB2) = splitAt (posB2 - posB1) postB1
                in [preB1, drop 1 preB2, drop 1 postB2] -- drop 1 because the NTs we split at are still in the second lists
    [offset1, offset2, offset3] = map (sum . map getFoNT) outerNTs
    outerFanoutSum = sum [offset1, offset2, offset3]
    remainderRule = (((lhs, concat outerNTs ++ [newNT]), resplit finRemH), d) -- Arbitrary definition: new NTs come at the end of a rule, to make the 'offset' at which the new variables start easily computable
    innerRule = (((newNT, [b1, b2]), inn_h'), 1.0)
    inn_h' = resplit finInnH
    newNT = Left (show [b1, b2] ++ show inn_h', length inn_h') :: ProtoNT -- any good hash would do, but even these fat strings will be intified away anyway
    oldH = getH h'
    (finRemH, finInnH) = let (rs,is,_,_,_) = foldl inspect ([], [], ([], []), No, 0) (oldH ++ [Nothing])
                         in (reverse (tail rs), reverse (tail is)) -- tail rs removes the just added Nothing (so every extract ends), tail is removes the final Nothing (one is added at every extract end)
    inspect (remH, innH, (perhapsRemH, perhapsInnH), extractionState, e) maybeNTT -- 'e' will count the number of extractions already performed; we need two 'Perhaps'-lists to account for the different readjusting strategies taking place, see below
      = let -- since we said that the new NT always comes last in the rule its variables start at 'outerFanSum'
            extractionTrace = intoRemain ((Just $ NT $ outerFanoutSum + e) : remH)
            -- here's how the two 'Perhaps'-Lists pay off
            isActuallyExtraction = intoInner (perhapsInnH ++ innH)
            isActuallyNoExtraction = intoRemain (perhapsRemH ++ remH)
            unsure = (intoRemain perhapsRemH, intoInner perhapsInnH)
            -- holes in the outer rule come from taking out NTs. If the NT the current variable is from comes after such an extracted NT, we obviously have to subtract its fanout, if its after the second we have to subtract both
            offsetAtTheRemainingNT v
              | getNTPosOfVar' v < posB1 = 0
              | getNTPosOfVar' v < posB2 = getFoNT b1
              | otherwise                = getFoNT b1 + getFoNT b2
            intoRemain xs = case maybeNTT of
                              (Just (NT v)) -> (Just $ NT $ v - offsetAtTheRemainingNT v) : xs
                              x -> x : xs
            -- the extraced rule should have variables start at 0, so shift it back w.r.t. where it started
            -- Again, same idea as above.
            offsetAtTheInnerNT v
              | getNTPosOfVar' v == posB1 = offset1
              | getNTPosOfVar' v == posB2 = offset1 + offset2
            intoInner xs = case maybeNTT of
                             (Just (NT v)) -> (Just $ NT $ v - offsetAtTheInnerNT v) : xs
                             x -> x : xs
        in case (extractionState, isExtractable maybeNTT) of -- extractionState: Yes = we're in a real extraction, Perhaps = might become an extraction, only read Ts so far, No = only reading Nothings or outer NTs
             (Yes, Yes)         -> (           remH,             intoInner innH,  ([], []),  Yes,     e)
             (Yes, Perhaps)     -> (           remH,             intoInner innH,  ([], []),  Yes,     e)
             (Yes, No)          -> (extractionTrace,             Nothing : innH,  ([], []),  No,      e + 1)
             (Perhaps, Yes)     -> (           remH,       isActuallyExtraction,  ([], []),  Yes,     e)
             (Perhaps, Perhaps) -> (           remH,                       innH,   unsure,   Perhaps, e)
             (Perhaps, No)      -> (isActuallyNoExtraction,                innH,  ([], []),  No,      e)
             (No, Yes)          -> (           remH,             intoInner innH,  ([], []),  Yes,     e)
             (No, Perhaps)      -> (           remH,                       innH,   unsure,   Perhaps, e)
             (No, No)           -> (intoRemain remH,                       innH,  ([], []),  No,      e)
    -- resplit aka explode in glorious languages like PHP
    resplit [] = []
    resplit s = let (xs, ys) = span (/= Nothing) s
                in (map fromJust xs) : resplit (drop 1 ys)
    isExtractable Nothing = No
    isExtractable (Just (T _)) = Perhaps
    isExtractable (Just (NT v)) = if getNTPosOfVar' v `elem` [posB1, posB2] then Yes else No
    getNTPosOfVar' = getNTPosOfVar (map getFoNT rhs)
    getFoNT (Right b) = fanouts A.! b
    getFoNT (Left (_, f)) = f

invertArray :: (Ord x) => A.Array Int x -> M.Map x Int
invertArray = M.fromList . map swap . A.assocs

getFoNTArrayFromRules :: [(Rule, Double)] -> A.Array Int Int -- ^ from NTs into fanouts
getFoNTArrayFromRules = toArray . foldl worker M.empty
  where
    toArray m = A.array (0, M.size m) $ M.assocs m
    worker m (((lhs, _), h'), _) = case M.lookup lhs m of
                                     Nothing -> M.insert lhs (length h') m
                                     Just _  -> m


-------------------------------------------------------
-- Naive Binarization (always extracts last two NTs) --
-------------------------------------------------------

binarizeNaively :: A.Array Int Int -> (Rule, Double) -> [(ProtoRule, Double)]
binarizeNaively fanouts r = binWorker (ruleToProto r)
  where
    binWorker :: (ProtoRule, Double) -> [(ProtoRule, Double)]
    binWorker r@(((_, rhs), _), _)
      | getRk r <= 2 = [r]
      | otherwise = let (innerRule, remainderRule) = extractFromRule fanouts r (length rhs - 2, length rhs - 1)
                    in innerRule : binWorker remainderRule
    


-----------------------------------------------------------------
-- Bounded binarization: cleverly choosing NTs to be extracted --
-----------------------------------------------------------------

-- Even number of sorted elements = start and end points of intervals. Start points are index of first variable in NTOnlyH *minus 1*, end point the index of the last.
-- Afaik dependent types could guarantee some of this, but if I really wanted to, I guess I could just use Int tuples to describe the intervals.
type Endpoints = [Int]

getFoEps :: Endpoints -> Int
getFoEps = (`div` 2) . length

overlaps :: Endpoints -> Endpoints -> Bool
overlaps _ [] = False
overlaps [] _ = False
overlaps (s1:e1:eps1) (s2:e2:eps2)
  | s1 >= e2 = overlaps (s1:e1:eps1) eps2 -- >= because of the -1 inside each start index
  | s2 >= e1 = overlaps eps1 (s2:e2:eps2)
  | otherwise = True

-- Union minus intersection. Doesn't check for overlap!
merge :: Endpoints -> Endpoints -> Endpoints
merge eps [] = eps
merge [] eps = eps
merge (e1:eps1) (e2:eps2)
  | e1 == e2 = merge eps1 eps2
  | e1 < e2 = e1 : merge eps1 (e2:eps2)
  | e1 > e2 = e2 : merge (e1:eps1) eps2

-- merges if sets are fo-adjacent, otherwise returns 'Nothing'
tryMerge :: Int -> Endpoints -> Endpoints -> Maybe Endpoints
tryMerge fo eps1 eps2 = let merged = merge eps1 eps2
                        in if (not $ overlaps eps1 eps2) && (getFoEps merged <= fo)
                           then Just merged
                           else Nothing

data NTTree = NTTreeLeaf Int -- leaves contain 'RealNT's (consistent index!)
            | NTTreeInner ([(ProtoIndexedRule, Double)] -> ([(ProtoIndexedRule, Double)], Int)) (T.Tree Int) -- massive WAT: inner nodes contain the binarized rule... sort-of. They only need the actual rule from above to actually start computing. order of lists is still (outer : manyinners), but this binarizer also returns the consistent-index of the newly created NT. The Int-Tree at the end is just a sort of history of the extraction process with which the Eq and Ord instances will be defined. Seems stupid and wasteful, I know.

-- We need these instances to use nice sets.
instance Eq NTTree where
  (NTTreeLeaf i1) == (NTTreeLeaf i2) = i1 == i2
  (NTTreeInner _ h1) == (NTTreeInner _ h2) = h1 == h2
  _ == _ = False

instance Ord NTTree where
  compare (NTTreeLeaf i1) (NTTreeLeaf i2) = compare i1 i2
  compare (NTTreeInner _ h1) (NTTreeInner _ h2) = compare h1 h2
  compare (NTTreeLeaf _) (NTTreeInner _ _) = LT
  compare (NTTreeInner _ _) (NTTreeLeaf _) = GT

-- To compare history trees... you guessed it: another Ord instance. Again we don't really care about the contents as long as it works.
instance Ord a => Ord (T.Tree a) where
  compare (T.Node i1 cs1) (T.Node i2 cs2)
    | i1 == i2 = compare cs1 cs2
    | otherwise = compare i1 i2

-- Why not.
instance Show NTTree where
  show (NTTreeLeaf i) = show i
  show (NTTreeInner _ h) = showHistTree h
    where showHistTree (T.Node i cs)
            = if null cs
              then show i
              else "("++(concat $ intersperse "," $ map showHistTree cs)++")"


type CandidateEndpoints = (NTTree, Endpoints) -- NT extractions (to be) performed in this tree and its endpoints for extension

-- We need to index the rhs or our rules to keep adresses consistent in this whole data flow mess:
-- This "consistent index" should be called 'i'
type ProtoIndexedRule = ((ProtoNT, [(Int, ProtoNT)]), [[NTT]])
indexRule indices (((lhs, rhs), h'), d) = (((lhs, zip indices rhs), h'), d)
deIndexRule (((lhs, rhs), h'), d) = (((lhs, map snd rhs), h'), d)
getIndices (((_, rhs), _), _) = map fst rhs
getCurPosFromIndex rhs i = fromJust $ elemIndex i $ map fst rhs

binarizeByAdjacency :: A.Array Int Int -> (Rule, Double) -> [(ProtoRule, Double)]
binarizeByAdjacency fanouts r@(((_, rhs), h'), _)
  | getRk r <= 2 = [ruleToProto r]
  | otherwise = traceShow r $ crunchRule $ tryAdjacenciesFrom $ getFo r
  where
    -- This will try all adjacencies to... some bound. All rules are binarizable, I promise.
    tryAdjacenciesFrom :: Int -> NTTree
    tryAdjacenciesFrom f = case traceShow f chooseGoodTree (computeAll f) of
                             Just t -> t
                             Nothing -> tryAdjacenciesFrom (f + 1)
    
    -- the working set shall not consist of mere endpoint sets, instead store trees representing the final extraction process (alongside the endpoints, we still need these for the algorithm)! Initially the set consists of leaves only which will be represented as the NT and its endpoints.
    candidates :: S.Set CandidateEndpoints
    cList_ = map getEndpointCandidatesForNT $ zip [0..] rhs
    candidates = S.fromList cList_
    indexedH = zip [0..] $ getNTOnlyH h'
    globalEndpoints :: Endpoints
    globalEndpoints = foldl1 merge $ map snd cList_
    
    getEndpointCandidatesForNT (n, b)
      = (,) (NTTreeLeaf n)
      $ foldl1 merge -- looks pretty stupid, consider a one-pass-fold
      $ map (\(i, _) -> [i-1, i])
      $ filter (\(_, m) -> case m of Just i -> getNTPosOfVar' i == n; _ -> False)
      $ indexedH
    getNTPosOfVar' = getNTPosOfVar $ map (fanouts A.!) rhs
    
    -- This functions generates all new trees from one tree and a working set. fo: target maximum fanout
    pairWith :: Int -> CandidateEndpoints -> S.Set CandidateEndpoints -> S.Set CandidateEndpoints
    pairWith fo c = S.map fromJust . S.delete Nothing . S.map (mergeCandidates fo c)
    
    -- We have to generate all trees from these leaves and choose one (see below) which has endpoints containing all NTs.
    -- All newly generated candidates (using pairWith) are inserted into a fringe from which the first argument for the pairWith-method is taken until the fringe is empty.
    -- This is acceptable for computing the closure because the merging process is symmetric!
    -- Also, we can easily compute all trees, since the number must be finite: each new element contains larger endpoint-intervals than both its children, and since the number of endpoints possible is very bounded - everything is awesome!
    computeAll :: Int -> [CandidateEndpoints]
    computeAll fo = computeAllWorker fo candidates (S.toList candidates)
      where
        computeAllWorker :: Int -> S.Set CandidateEndpoints -> [CandidateEndpoints] -> [CandidateEndpoints]
        computeAllWorker _ workingSet []
          = S.toList workingSet
        computeAllWorker fo workingSet (todo:fringe)
          = let new = pairWith fo todo workingSet
            in traceShow (length fringe) computeAllWorker fo (S.union workingSet new) (fringe ++ S.toList new)
    
    -- TODO: choose the best rather than the first!
    chooseGoodTree :: [CandidateEndpoints] -> Maybe NTTree
    chooseGoodTree = fmap fst . listToMaybe . take 1 . filter ((==globalEndpoints) . snd)
    
    -- Looks like there is a bidirectional dataflow: the initial rule goes top-down through the tree and is dismantled more and more the farther to the right it goes, but the true extractions happen from the leaves to towards the root. Once all nodes are evaluated the rule flows upwards through the recursive calls and all binarization-extractions are actually performed.
    -- tl;dr: trees of half applied binarization functions.
    mergeCandidates :: Int -> CandidateEndpoints -> CandidateEndpoints -> Maybe CandidateEndpoints
    mergeCandidates fo (t1', es1') (t2', es2')
      = -- Equality here stays ambigous, but merging equal trees results in Nothing anyway.
        let ((t1, es1), (t2, es2)) = if t1' < t2'
                                     then ((t1', es1'), (t2', es2'))
                                     else ((t2', es2'), (t1', es1'))
        in tryMerge fo es1 es2 >>= \mergedEps -> case (t1, t2) of
          (NTTreeLeaf i1, NTTreeLeaf i2)
            -> return (NTTreeInner (binarizer i1 i2)
                                   (T.Node 0 [T.Node i1 [], T.Node i2 []])
                      , mergedEps)
          (NTTreeInner bin1 h1, NTTreeLeaf i2)
            -> return (NTTreeInner (\rs -> let (binneds, i1) = bin1 rs
                                           in binarizer i1 i2 binneds)
                                   (T.Node 0 [h1, T.Node i2 []])
                      , mergedEps)
          (NTTreeLeaf i1, NTTreeInner bin2 h2)
            -> return (NTTreeInner (\rs -> let (binneds, i2) = bin2 rs
                                           in binarizer i1 i2 binneds)
                                   (T.Node 0 [T.Node i1 [], h2])
                      , mergedEps)
          (NTTreeInner bin1 h1, NTTreeInner bin2 h2)
            -> return (NTTreeInner (\rs -> let (rs', i1) = bin1 rs
                                               (binneds, i2) = bin2 rs'
                                           in binarizer i1 i2 binneds)
                                   (T.Node 0 [h1, h2])
                      , mergedEps)
      where
        
        minmaxFst :: Ord a => (a, b) -> (a, b) -> ((a, b), (a, b))
        minmaxFst (a1, b1) (a2, b2) = if a1 < a2
                                      then ((a1, b1), (a2, b2))
                                      else ((a2, b2), (a1, b1))
        binarizer i1 i2 (outer:oldInners)
          = let (newInn, newRem) = extractFromRule fanouts (deIndexRule outer)
                                                   ( (getCurPosFromIndex (getRhs outer) i1)
                                                   , (getCurPosFromIndex (getRhs outer) i2))
                oldIndices = getIndices outer
                newlyCreatedNTIndex = maximum oldIndices + 1
                newIndices = filter (\i -> i /= i1 && i /= i2) oldIndices ++ [newlyCreatedNTIndex]
                
            in (indexRule newIndices newRem : indexRule (repeat undefined) newInn : oldInners, newlyCreatedNTIndex)
    
    -- Now use the rule to evaluate the root - it's time to fully evaluate all these functions in the inner nodes!
    crunchRule :: NTTree -> [(ProtoRule, Double)]
    crunchRule (NTTreeInner binarizer _)
      = let (binned, _) = binarizer [indexRule [0..] $ ruleToProto r]
            (uselessRule@(((lhs, _), _), d) : firstRule@(((_, rhs), h'), _) : rules) = map deIndexRule binned
            newFirstRule = (((lhs, rhs), h'), d)
        in assert (getRk uselessRule == 1 && getFo uselessRule == getFo firstRule) $ newFirstRule : rules
    
    -- sometesttree
    sometesttree = let cs = S.toList candidates
                   in fst $ fromJust $ mergeCandidates 2 (fromJust $ mergeCandidates 3 (cs !! 0) (cs !! 2)) (cs !! 1)

-------------------------------------------------------------------------------------
-- This is what I'm currently working on so this is where the main function stays. --
-------------------------------------------------------------------------------------

main :: IO ()
main = do
       
       let makeRealRuleTree rs = fmap (\i -> mkHyperedge (fst . fst $ rs !! i) (snd . fst $ rs !! i) i i)
       let my_a_t = A.array (0,5) [(0,"a"), (1,"A"), (2, "B"), (3, "C"), (4,"AA"), (5, "CC")]
           myProtoRules = [ (((Left ("S", 2), [Left ("A", 2), Left ("B", 1), Left ("C", 2)]),
                                [[nt 0, tt 0, nt 2, nt 1], [nt 3, nt 4]]), 0.5)
                          , (((Left ("A", 2), []),
                                [[tt 1], [tt 4]]), 0.5)
                          , (((Left ("B", 1), []),
                                [[tt 2]]), 0.5)
                          , (((Left ("C", 2), []),
                                [[tt 3], [tt 5]]), 0.5)
                          ]
           (myCleanRules, my_m_nt) = intifyProtoRules M.empty myProtoRules
           newRules = binarizeByAdjacency (getFoNTArrayFromRules myCleanRules) $ head myCleanRules
           (cleanNewRules, my_new_m_nt) = intifyProtoRules my_m_nt newRules
           
           uMXRS = getMXRSFromProbabilisticRules myCleanRules [0]
           bMXRS = getMXRSFromProbabilisticRules (cleanNewRules ++ tail myCleanRules) [0]
           uDeriv = makeRealRuleTree (map fst myCleanRules) $ VT.node 0 [VT.node 1 [], VT.node 2 [], VT.node 3 []]
           -- Since the algorithm is deterministic I know which rules should be created and can guess their ids.
           bInner = [3,4] -- 2:A, 3:B, 4:C
           bOuter = 2
           bDeriv = makeRealRuleTree (map fst $ cleanNewRules ++ tail myCleanRules)
                  $ VT.node 0 [
                        VT.node bOuter [],
                        VT.node 1 (map (\i -> VT.node i []) bInner)
                    ] -- 0 and 1 come from the binarized S-Rule, 2,3,4 are for the others
       
       putStrLn "\nNew rules look like this:"
       mapM_ (putStrLn . retranslateRule (invertMap my_new_m_nt) my_a_t) $ map fst cleanNewRules
       
       putStrLn "\nThe old rule yields:"
       print $ sententialFront (irtg uMXRS) (invertMap my_m_nt) my_a_t uDeriv
       print $ getDerivProbability uMXRS uDeriv
       
       putStrLn "\nThe new rules yield:"
       print $ sententialFront (irtg bMXRS) (invertMap my_new_m_nt) my_a_t bDeriv
       print $ getDerivProbability bMXRS bDeriv
       
       
       putStrLn "\n\nLook at how insanely long this takes:"
       
       let bigRule = (((40,[19,18,19,7,12,51]),[[NT 0,NT 1,NT 2,NT 3],[NT 4,NT 5]]),0.5)
           itsFoArray = A.array (7,52) $ zip [7..52] $ repeat 1
       print $ binarizeByAdjacency itsFoArray bigRule
       
       {-
       corpusText' <- TIO.readFile "/home/sjm/programming/LCFRS/tiger_release_aug07_shorttest.export"
       
       let (countRuleMap, (a_nt, a_t)) = extractCountedRulesFromNegra $ parseNegra corpusText'
           
           pRuleMap = normalizeRuleProbs countRuleMap
           flatCountRuleMap = M.foldl' M.union M.empty countRuleMap
           flatPRuleMap = M.foldl' M.union M.empty pRuleMap
           
           rulesAndCounts = M.assocs flatCountRuleMap
           rulesAndProbs = M.assocs flatPRuleMap
           myLCFRS = getMXRSFromProbabilisticRules rulesAndProbs [0] -- assumptions, assumptions...
       
       -- And now for the total memory devastation:
       
       putStr "loading original corpus... "
       print $ length $ edges $ rtg $ irtg myLCFRS
       putStr "after binarization I expect this many rules: "
       print $ (sum $ map (\r -> if getRk r > 2 then getRk r - 1 else 1) $ toProbabilisticRules myLCFRS)
       
       -- print $ F.foldl' (+) (0::Integer) $ take 10000000 $ repeat 1 -- busy waiting
       
       let (binnedMXRS, binned_m_nt) = binarizeMXRS (invertArray a_nt) myLCFRS
       
       putStr "binarizing some thousand rules leads to... "
       print $ length $ edges $ rtg $ irtg binnedMXRS
       
       -- print $ F.foldl' (+) (0::Integer) $ take 10000000 $ repeat 1 -- busy waiting
       
       putStrLn $ printf "%d NTs have become %d NTs." (snd $ A.bounds a_nt) (M.size binned_m_nt)
       
       -- -}
       
       {-
       Results: binarizeNaively
       loading original corpus... 205
       after binarization I expect this many rules: 279
       binarizing some thousand rules leads to... 279
       52 NTs have become 117 NTs.
       
       Results: binarizeByAdjacence
       binarizing some thousand rules leads to... 279
       52 NTs have become 114 NTs.
       
       
       ... wait this is absolutely uninteresting.
       I should write a nice getStatistics function.
       Next commit. Maybe.
       -}

binarizeMXRS :: M.Map String Int -> MXRS -> (MXRS, M.Map String Int)
binarizeMXRS m_nt inLCFRS = (getMXRSFromProbabilisticRules newRules [0], newMap)
  where
    (newRules, newMap) = intifyProtoRules m_nt $ concatMap (binarizeNaively fanouts) oldRules
    oldRules = toProbabilisticRules inLCFRS
    fanouts = getFoNTArrayFromRules oldRules
