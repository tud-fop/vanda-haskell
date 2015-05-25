module Vanda.Grammar.XRS.LCFRS.Binarize where

import           Control.Exception.Base (assert)
import           Control.Monad.State.Strict
import qualified Data.Array as A
import           Data.List (intercalate, elemIndex)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import qualified Data.Text.Lazy.IO as TIO
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
    posB1 = min posBa posBb
    posB2 = max posBa posBb
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

binarizeNaively :: A.Array Int Int -> (ProtoRule, Double) -> [(ProtoRule, Double)]
binarizeNaively fanouts r@(((_, rhs), _), _)
  | getRk r <= 2 = [r]
  | otherwise = innerRule : binarizeNaively fanouts remainderRule
  where
    (innerRule, remainderRule) = extractFromRule fanouts r (length rhs - 2, length rhs - 1)


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

data NTTree = NTTreeInner ([(ProtoIndexedRule, Double)] -> ([(ProtoIndexedRule, Double)], Int)) -- massive WAT: inner nodes contain the binarized rule... sort-of. They only need the actual rule from above to actually start computing. order of lists is still (outer : manyinners), but this binarizer also returns the consistent-index of the newly created NT.
            | NTTreeLeaf Int -- leaves contain 'RealNT's (consistent index!)
type CandidateEndpoints = (NTTree, Endpoints) -- NT extractions (to be) performed in this tree and its endpoints for extension

-- We need to index the rhs or our rules to keep adresses consistent in this whole data flow mess:
-- This "consistent index" should be called 'i'
type ProtoIndexedRule = ((ProtoNT, [(Int, ProtoNT)]), [[NTT]])
indexRule indices (((lhs, rhs), h'), d) = (((lhs, zip indices rhs), h'), d)
deIndexRule (((lhs, rhs), h'), d) = (((lhs, map snd rhs), h'), d)
getIndices (((_, rhs), _), _) = map fst rhs
getCurPosFromIndex rhs i = fromJust $ elemIndex i $ map fst rhs

binarizeByAdjacence :: A.Array Int Int -> (Rule, Double) -> [(ProtoRule, Double)]
binarizeByAdjacence fanouts r@(((_, rhs), h'), _)
  | getRk r <= 2 = [ruleToProto r]
  | otherwise = crunchRule sometesttree
  where
    -- the working set shall not consist of mere endpoint sets, instead store trees representing the final extraction process (alongside the endpoints, we still need these for the algorithm)! Initially the set consists of leaves only which will be annotated with the NTs and their endpoints.
    candidates = map getEndpointCandidatesForNT $ zip [0..] rhs
    indexedH = zip [0..] $ getNTOnlyH h'
    getEndpointCandidatesForNT (n, b)
      = (,) (NTTreeLeaf n)
      $ foldl1 merge -- looks pretty stupid, consider a one-pass-fold
      $ map (\(i, _) -> [i-1, i])
      $ filter (\(i, m) -> case m of Just i -> getNTPosOfVar' i == n; _ -> False)
      $ indexedH
    getNTPosOfVar' = getNTPosOfVar (map (fanouts A.!) rhs)
    -- ooooh, the Maybe monad!
    mergeCandidates fo (t1, es1) (t2, es2)
      = tryMerge fo es1 es2 >>= \merged -> case (t1, t2) of
          (NTTreeLeaf i1, NTTreeLeaf i2)
            -> return (NTTreeInner (binarizer i1 i2), merged)
          (NTTreeInner bin1, NTTreeLeaf i2)
            -> return (NTTreeInner (\rs -> let (binneds, i1) = bin1 rs
                                           in binarizer i1 i2 binneds), merged)
          (NTTreeLeaf i1, NTTreeInner bin2)
            -> return (NTTreeInner (\rs -> let (binneds, i2) = bin2 rs
                                           in binarizer i1 i2 binneds), merged)
          (NTTreeInner bin1, NTTreeInner bin2)
            -> return (NTTreeInner (\rs -> let (rs', i1) = bin1 rs
                                               (binneds, i2) = bin2 rs'
                                           in binarizer i1 i2 binneds), merged)
          
      where
        binarizer i1 i2 (outer:oldInners)
          = let (newInn, newRem) = extractFromRule fanouts (deIndexRule outer)
                                                   ( (getCurPosFromIndex (getRhs outer) i1)
                                                   , (getCurPosFromIndex (getRhs outer) i2))
                oldIndices = getIndices outer
                newlyCreatedNTIndex = maximum oldIndices + 1
                newIndices = filter (\i -> i /= i1 && i /= i2) oldIndices ++ [newlyCreatedNTIndex]
                
            in (indexRule newIndices newRem : indexRule (repeat undefined) newInn : oldInners, newlyCreatedNTIndex)
    -- some sort of function i guess or something like that
    -- looks like there is a bidirectional dataflow:
    -- the initial rule goes top-down through the function calls and is dismantled more and more the farther to the right it goes
    -- once all nodes are evaluated the rule flows upwards trhoug the recursive calls and all binarization-extractions are performed
    -- ready lets go
    crunchRule :: NTTree -> [(ProtoRule, Double)]
    crunchRule (NTTreeInner binarizer)
      = let (binned, _) = binarizer [indexRule [0..] $ ruleToProto r]
            (uselessRule@(((lhs, _), _), d) : firstRule@(((_, rhs), h'), _) : rules) = map deIndexRule binned
            newFirstRule = (((lhs, rhs), h'), d)
        in assert (getRk uselessRule == 1 && getFo uselessRule == getFo firstRule) $ newFirstRule : rules
    
    -- sometesttree
    sometesttree = fst $ fromJust $ mergeCandidates 2 (fromJust $ mergeCandidates 3 (candidates !! 0) (candidates !! 2)) (candidates !! 1)

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
           newRules = binarizeByAdjacence (getFoNTArrayFromRules myCleanRules) $ head myCleanRules
           (cleanNewRules, my_new_m_nt) = intifyProtoRules my_m_nt newRules
           
           uMXRS = getMXRSFromProbabilisticRules myCleanRules [0]
           bMXRS = getMXRSFromProbabilisticRules (cleanNewRules ++ tail myCleanRules) [0]
           uDeriv = makeRealRuleTree (map fst myCleanRules) $ VT.node 0 [VT.node 1 [], VT.node 2 [], VT.node 3 []]
           bDeriv = makeRealRuleTree (map fst $ cleanNewRules ++ tail myCleanRules)
                  $ VT.node 0 [
                        VT.node 3 [],
                        VT.node 1 [
                            VT.node 2 [], VT.node 4 []
                        ]
                    ] -- 1 and 0 come from the S-Rule, 2,3,4 are for the others
       
       putStrLn "\nNew rules look like this:"
       mapM_ (putStrLn . retranslateRule (invertMap my_new_m_nt) my_a_t) $ map fst cleanNewRules
       
       putStrLn "\nThe old rule yields:"
       print $ sententialFront (irtg uMXRS) (invertMap my_m_nt) my_a_t uDeriv
       print $ getDerivProbability uMXRS uDeriv
       
       putStrLn "\nThe new rules yield:"
       print $ sententialFront (irtg bMXRS) (invertMap my_new_m_nt) my_a_t bDeriv
       print $ getDerivProbability bMXRS bDeriv
       
       
       
       
       
       
        {-
       corpusText' <- TIO.readFile "/home/sjm/programming/LCFRS/tiger_release_aug07_semi.export"
       
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

binarizeMXRS :: M.Map String Int -> MXRS -> (MXRS, M.Map String Int)
binarizeMXRS m_nt inLCFRS = error "binarizeMXRS todo" -- (getMXRSFromProbabilisticRules newRules [0], newMap)
  where
    (newRules, newMap) = intifyProtoRules m_nt $ concatMap (binarizeNaively fanouts . ruleToProto) oldRules
    oldRules = toProbabilisticRules inLCFRS
    fanouts = getFoNTArrayFromRules oldRules
