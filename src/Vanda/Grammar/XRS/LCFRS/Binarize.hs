-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Sebastian Mielke 2015
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Grammar.XRS.LCFRS.Binarize
( Binarizer
, binarizeNaively
, binarizeByAdjacency
, binarizeHybrid
, binarizeUsing
, binarizeRuleSubset
) where

import qualified Control.Error
import           Control.Exception.Base (assert)
import           Control.Monad.Trans.State.Strict (State, get, put, runState)
import qualified Data.Array as A
import           Data.List (intercalate, intersperse, elemIndex)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust, listToMaybe)
import qualified Data.Set as S
import qualified Data.Tree as T
import           Data.Tuple (swap)

import           Data.NTT
import           Vanda.Util.Memorysavers (invertMap)

import           Vanda.Grammar.XRS.LCFRS

errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.Grammar.XRS.LCFRS.Binarize"


{------------------------------------------------------------------------------
- ProtoRules are necessary to check for duplicates when binarizing many rules -
------------------------------------------------------------------------------}

-- The new NTs should be "named" with what they produce to eliminate duplicates.
-- Saving "what they produce" in a tree containing the fusion history,
-- where the inner node contain the comp.f. of their fusion rule
data NTRepTree = NTRepLeaf NTIdent -- original NTs at the leaves
               | NTRepInner [[CompFuncEntry]] NTRepTree NTRepTree
               deriving (Show, Eq, Ord)
-- Saving the "unfinished" NTs with their complete history/meaning allows us to
-- later maybe join even more NTs when intifying them, because given the right
-- homomorphisms, AB and BA for example are absolutely equivalent! TODO.
type ProtoNT = (NTRepTree, Fanout) -- unintified ProtoNTs need their fanout!
type ProtoRule = ((ProtoNT, [ProtoNT]), [[CompFuncEntry]])

ruleToProto :: A.Array NTIdent Fanout -> (Rule, Double) -> (ProtoRule, Double)
ruleToProto fanouts (((lhs, rhs), h'), d)
  = ((( (NTRepLeaf lhs, fanouts A.! lhs)
      , map (\r -> (NTRepLeaf r, fanouts A.! r)) rhs
    ), h'), d)

intifyProtoRules
  :: M.Map String NTIdent
  -> [(ProtoRule, Double)]
  -> ([(Rule, Double)], M.Map String NTIdent)
intifyProtoRules m_in rs = runState (mapM intifyProtoAction rs) m_in
  where
    intifyProtoAction
      :: (ProtoRule, Double)
      -> State (M.Map String NTIdent) (Rule, Double)
    intifyProtoAction (((lhs, rhs), h'), d)
      = do newLhs <- intifyProtoNT lhs
           newRhs <- mapM intifyProtoNT rhs
           return (((newLhs, newRhs), h'), d)
    intifyProtoNT :: ProtoNT -> State (M.Map String NTIdent) NTIdent
    intifyProtoNT (NTRepLeaf i, _) = return i
    intifyProtoNT (t, _)
      = do oldmap <- get
           let s = show t -- Still strings, like the original extracted Map
           case M.lookup s oldmap of
             Just i  -> return i
             Nothing -> let i = M.size oldmap
                        in put (M.insert s i oldmap) >> return i


{-------------------------------------------------------------
- Fusion of two NTs into a new rule leaving a remaining rule -
-------------------------------------------------------------}

getH :: [[CompFuncEntry]] -> [Maybe CompFuncEntry]
getH = intercalate [Nothing] . map (map Just)
getNTOnlyH :: [[CompFuncEntry]] -> [Maybe Int]
getNTOnlyH = map (fmap (\(NT b) -> b))
           . filter (\m -> case m of Just (T _) -> False; _ -> True)
           . getH

getNTPosOfVar
  :: [Fanout] -- ^ fanout "annotated" rhs containing only the fanouts
  -> Int -- ^ variable at which we search
  -> Int -- ^ position of NT in rhs
getNTPosOfVar fanoutAnnotatedRHS v
  = let searchWorker i ((pos, fo) : xs) = if i + fo > v
                                          then pos
                                          else searchWorker (i + fo) xs
        searchWorker _ [] = errorHere "getNTPosOfVar.searchWorker" "the rhs should not be empty"
    in searchWorker 0 $ zip [0..] $ fanoutAnnotatedRHS

data TriState = Yes | Perhaps | No deriving Show

-- ascending order of indices is unnecessary, makes calling this function easier
fuseInRule
  :: (ProtoRule, Double) -- ^ rule
  -> (Int, Int) -- ^ indices of the NTs that are to be fused
  -> ((ProtoRule, Double), (ProtoRule, Double)) -- ^ fusion rule, remainder rule
fuseInRule (((lhs, rhs), h'), d) (posBa, posBb) = (fusionRule, remRule)
  where
    minmax :: Ord a => a -> a -> (a, a)
    minmax a b = (min a b, max a b)
    (posB1, posB2) = minmax posBa posBb
    -- Conventions I would like to adhere to: 'v' for variables in the comp.f.s,
    -- 'n' for NT positions, 'b[i]' for NTs and no more ambiguous 'i's
    -- capital H homomorphisms/comp.f.s are akin to the characteristic strings 
    -- you can create from tuples by replacing the tuples commas with 'Nothing's
    [b1, b2] = [rhs !! posB1, rhs !! posB2]
    outerNTs = let (preB1, postB1) = splitAt posB1 rhs
                   (preB2, postB2) = splitAt (posB2 - posB1) postB1
                in [preB1, drop 1 preB2, drop 1 postB2]
    -- ^ drop 1 because the NTs we split at are still in the second lists
    [offset1, offset2, _] = map (sum . map getFoNT) outerNTs
    remRule = (((lhs, newNT : (concat outerNTs)), resplit finRemH), d)
    -- ^ Arbitrary definition: new NTs come at the front of a rule,
    -- the 'offset' at which the new variables start can still be used in
    -- construction thanks to lazy evaluation (see newVariables below)
    fusionRule = (((newNT, [b1, b2]), inn_h'), 1.0)
    inn_h' = resplit finInnH
    newNT = (NTRepInner inn_h' (fst b1) (fst b2), length inn_h') :: ProtoNT
    oldH = getH h'
    (finRemH, finInnH) = let (rs,is,_,_,_)
                               = foldl inspect
                                       ([], [], ([], []), No, 0)
                                       (oldH ++ [Nothing])
                         in (reverse (tail rs), reverse (tail is))
                         -- tail rs removes the just added Nothing
                         -- (so every extract ends), tail is removes the final
                         -- Nothing (one is added at every extract end)
    newVariables = length inn_h'
    -- We only know this after all extractions are performed, but thanks to
    -- lazy evaluation we can use it when folding inspect over the old H to
    -- create the new Hs!
    inspect (remH, innH, (perhapsRemH,perhapsInnH), extractionState, e) maybeNTT
    -- 'e' will count the number of extractions already performed;
    -- we need two 'Perhaps'-lists to account for the different readjusting
    -- strategies taking place, see below
      = let -- since we said that the new NT always comes first in the rule
            -- its variables start at 0
            extractionTrace = intoRemain ((Just $ NT e) : remH)
            -- here's how the two 'Perhaps'-Lists pay off
            isActuallyExtraction = intoInner (perhapsInnH ++ innH)
            isActuallyNoExtraction = intoRemain (perhapsRemH ++ remH)
            unsure = (intoRemain perhapsRemH, intoInner perhapsInnH)
            -- holes in the outer rule come from taking out NTs.
            -- If the NT the current variable is from comes after such an
            -- extracted NT, we obviously have to subtract its fanout,
            -- if its after the second we have to subtract both.
            offsetAtTheRemainingNT v
              | getNTPosOfVar' v < posB1 = 0
              | getNTPosOfVar' v < posB2 = getFoNT b1
              | otherwise                = getFoNT b1 + getFoNT b2
            intoRemain xs
              = case maybeNTT of
                  (Just (NT v))
                    -> (Just $ NT $ newVariables + v - offsetAtTheRemainingNT v)
                       : xs
                  x -> x : xs
            -- The extraced rule should have variables start at 0,
            -- so shift it back w.r.t. where it started.
            -- Again, same idea as above.
            offsetAtTheInnerNT v
              | getNTPosOfVar' v == posB1 = offset1
              | getNTPosOfVar' v == posB2 = offset1 + offset2
              | otherwise                 = errorHere "fuseInRule.inspect.offsetAtTheInnerNT" "position does not match"
            intoInner xs
              = case maybeNTT of
                  (Just (NT v))
                    -> (Just $ NT $ v - offsetAtTheInnerNT v)
                       : xs
                  x -> x : xs
        in case (extractionState, isExtractable maybeNTT) of
        -- extractionState:
        -- Yes = we're in a real extraction,
        -- Perhaps = might become an extraction, only read Ts so far,
        -- No = only reading Nothings or outer NTs
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
    isExtractable (Just (NT v)) = if getNTPosOfVar' v `elem` [posB1, posB2]
                                  then Yes else No
    getNTPosOfVar' = getNTPosOfVar (map getFoNT rhs)
    getFoNT = snd

invertArray :: (Ord x, A.Ix y) => A.Array y x -> M.Map x y
invertArray = M.fromList . map swap . A.assocs

-- FoNT = fanout of NT
getFoNTArrayFromRules
  :: [(Rule, Double)]
  -> A.Array NTIdent Fanout -- ^ from NTs into fanouts
getFoNTArrayFromRules = toArray . foldl worker M.empty
  where
    toArray m = A.array (0, M.size m) $ M.assocs m
    worker m (((lhs, _), h'), _) = case M.lookup lhs m of
                                     Nothing -> M.insert lhs (length h') m
                                     Just _  -> m


type Binarizer =  A.Array NTIdent Fanout -> (Rule, Double) -> [(ProtoRule, Double)]

{-------------------------------------------------
- Naive Binarization (always fuses last two NTs) -
-------------------------------------------------}

binarizeNaively :: Binarizer
binarizeNaively fanouts r = binWorker (ruleToProto fanouts r)
  where
    binWorker :: (ProtoRule, Double) -> [(ProtoRule, Double)]
    binWorker r@(((_, rhs), _), _)
      | getRk r <= 2 = [r]
      | otherwise = let (innerRule, remainderRule)
                          = fuseInRule r (length rhs - 2, length rhs - 1)
                    in innerRule : binWorker remainderRule
    


{----------------------------------------------------------
- Bounded binarization: cleverly choosing NTs to be fused -
----------------------------------------------------------}

-- Even number of sorted elements = start and end points of intervals.
-- Start points are index of first variable in NTOnlyH *minus 1*,
-- end point the index of the last.
type Endpoints = [Int]

getFoEps :: Endpoints -> Fanout
getFoEps = (`div` 2) . length

overlaps :: Endpoints -> Endpoints -> Bool
overlaps _ [] = False
overlaps [] _ = False
overlaps (s1:e1:eps1) (s2:e2:eps2)
  -- use >= because of the -1 inside each start index
  | s1 >= e2 = overlaps (s1:e1:eps1) eps2
  | s2 >= e1 = overlaps eps1 (s2:e2:eps2)
  | otherwise = True
overlaps _ _ = errorHere "overlaps" "odd number of endpoints"

-- Union minus intersection. Doesn't check for overlap!
merge :: Endpoints -> Endpoints -> Endpoints
merge eps [] = eps
merge [] eps = eps
merge (e1:eps1) (e2:eps2)
  | e1 == e2  = merge eps1 eps2
  | e1 < e2   = e1 : merge eps1 (e2:eps2)
  | otherwise = e2 : merge (e1:eps1) eps2

-- merges if sets are fo-adjacent, otherwise returns 'Nothing'
tryMerge :: Fanout -> Endpoints -> Endpoints -> Maybe Endpoints
tryMerge fo eps1 eps2
  = let merged = merge eps1 eps2
    in if (not $ overlaps eps1 eps2) && (getFoEps merged <= fo)
       then Just merged
       else Nothing

-- These are the "blueprint trees" / binarisation-order-trees for the fusion.
-- Massive WAT: inner nodes contain the binarized rule... sort-of.
-- They only need the actual rule from above to actually start computing.
-- The order of lists is still (outer : manyinners), but this binarizer also
-- returns the consistent-index of the newly created NT. The Int-Tree at the end
-- is just a sort of history of the fusion process with which the Eq and Ord
-- instances will be defined. Seems stupid and wasteful, I know.
data NTTree = NTTreeLeaf CInd -- leaves contain 'RealNT's (consistent index!)
            | NTTreeInner
              (   [(ProtoIndexedRule, Double)]
               -> ([(ProtoIndexedRule, Double)], CInd) )
              (T.Tree CInd)

-- We need these instances to use nice sets.
instance Eq NTTree where
  (NTTreeLeaf i1) == (NTTreeLeaf i2) = i1 == i2
  (NTTreeInner _ h1) == (NTTreeInner _ h2) = h1 == h2
  _ == _ = False

instance Ord NTTree where
  compare (NTTreeLeaf i1) (NTTreeLeaf i2) = compare i1 i2
  compare (NTTreeInner _ h1) (NTTreeInner _ h2) = compare h1 h2
  compare (NTTreeLeaf _) (NTTreeInner _ _) = GT
  compare (NTTreeInner _ _) (NTTreeLeaf _) = LT

-- To compare history trees... you guessed it: another Ord instance.
-- Again we don't really care about the contents as long as it works.
instance Ord a => Ord (T.Tree a) where
  compare (T.Node i1 cs1) (T.Node i2 cs2)
    | i1 == i2 = compare cs1 cs2
    | otherwise = compare i1 i2

-- Why not.
instance Show NTTree where
  show (NTTreeLeaf i) = show i
  show (NTTreeInner _ h') = showHistTree h'
    where showHistTree (T.Node i cs)
            = if null cs
              then show i
              else "("++(concat $ intersperse "," $ map showHistTree cs)++")"

-- NT fusions (to be) performed in this tree and its endpoints for extension
type CandidateEndpoints = (NTTree, Endpoints)

-- We need to index the rhs of our rules to keep adresses consistent
-- in this whole data flow mess: This "consistent index" should be called 'i'
type CInd = Int
type ProtoIndexedRule = ((ProtoNT, [(CInd, ProtoNT)]), [[CompFuncEntry]])

indexRule :: [Int] -> (ProtoRule, Double) -> (ProtoIndexedRule, Double)
indexRule indices (((lhs, rhs), h'), d) = (((lhs, zip indices rhs), h'), d)

deIndexRule :: (ProtoIndexedRule, Double) -> (ProtoRule, Double)
deIndexRule (((lhs, rhs), h'), d) = (((lhs, map snd rhs), h'), d)

getIndices :: (ProtoIndexedRule, Double) -> [CInd]
getIndices (((_, rhs), _), _) = map fst rhs

getCurPosFromIndex
  :: [(CInd, ProtoNT)] -- ^ a rhs
  -> CInd -- ^ consistent index
  -> Int -- ^ actual current index
getCurPosFromIndex rhs i = fromJust $ elemIndex i $ map fst rhs

binarizeHybrid
  :: Int -- ^ bound for the rank up to which we binarize optimally
  -> Binarizer
binarizeHybrid b a r@(((_, nts), _), _)
  = (if length nts <= b then binarizeByAdjacency else binarizeNaively) a r


binarizeByAdjacency
  :: Binarizer
binarizeByAdjacency fanouts r@(((_, rhs), h'), _)
  | getRk r <= 2 = [ruleToProto fanouts r]
  | otherwise = crunchRule $ tryAdjacenciesFrom $ getFo r
  where
    -- This will try all adjacencies to... some bound.
    -- All rules are binarizable, I promise.
    tryAdjacenciesFrom :: Fanout -> NTTree
    tryAdjacenciesFrom f = case chooseGoodTree (computeAll f) of
                             Just t -> t
                             Nothing -> tryAdjacenciesFrom (f + 1)
    -- the working set shall not consist of mere endpoint sets, instead store
    -- trees representing the final fusion process (alongside the endpoints,
    -- we still need these for the algorithm)! Initially the set consists of
    -- leaves only which will be represented as the NT and its endpoints.
    candidates :: S.Set CandidateEndpoints
    cList_ = map getEndpointCandidatesForNT $ zip [0..] rhs
    candidates = S.fromList cList_
    indexedH = zip [0..] $ getNTOnlyH h'
    globalEndpoints :: Endpoints
    globalEndpoints = foldl1 merge $ map snd cList_
    getEndpointCandidatesForNT (n, _)
      = (,) (NTTreeLeaf n)
      $ foldl1 merge -- looks pretty stupid, consider a one-pass-fold
      $ map (\(i, _) -> [i-1, i])
      $ filter (maybe False (\i -> getNTPosOfVar' i == n) . snd)
      $ indexedH
    getNTPosOfVar' = getNTPosOfVar $ map (fanouts A.!) rhs
    -- This functions generates all new trees from one tree and a working set.
    pairWith
      :: Fanout -- ^ target maximum fanout
      -> CandidateEndpoints
      -> S.Set CandidateEndpoints
      -> S.Set CandidateEndpoints
    pairWith fo c = S.map fromJust
                  . S.delete Nothing
                  . S.map (mergeCandidates fo c)
    -- We have to generate all trees from these leaves and choose one (see
    -- below) which has endpoints containing all NTs.
    -- All newly generated candidates (using pairWith) are inserted into a
    -- fringe from which the first argument for the pairWith-method is taken
    -- until the fringe is empty.
    -- This is acceptable for computing the closure because the merging process
    -- is symmetric! Also, we can easily compute all trees, since the number
    -- must be finite: each new element contains larger endpoint-intervals than
    -- both its children, and since the number of endpoints possible is very
    -- bounded.
    -- Complexity is still insane.
    computeAll :: Fanout -> [CandidateEndpoints]
    computeAll fo = computeAllWorker fo candidates (S.toList candidates)
      where
        computeAllWorker
          :: Fanout
          -> S.Set CandidateEndpoints
          -> [CandidateEndpoints]
          -> [CandidateEndpoints]
        computeAllWorker _ workingSet []
          = S.toList workingSet
        computeAllWorker fo workingSet (todo:fringe)
          = let new = pairWith fo todo workingSet
            in computeAllWorker fo
                                (S.union workingSet new)
                                (fringe ++ S.toList new)
    -- TODO: choose the best rather than the first!
    chooseGoodTree :: [CandidateEndpoints] -> Maybe NTTree
    chooseGoodTree = fmap fst
                   . listToMaybe
                   . take 1
                   . filter ((==globalEndpoints) . snd)
    -- Looks like there is a bidirectional dataflow: the initial rule goes
    -- top-down through the tree and is dismantled more and more the farther to
    -- the right it goes, but the true fusions happen from the leaves 
    -- towards the root. Once all nodes are evaluated the rule flows upwards
    -- through the recursive calls and all binarization-fusions are actually
    -- performed.
    -- tl;dr: trees of half applied binarization functions.
    mergeCandidates
      :: Fanout
      -> CandidateEndpoints
      -> CandidateEndpoints
      -> Maybe CandidateEndpoints
    mergeCandidates fo (t1', es1') (t2', es2')
      = -- Merging equal trees would result in Nothing anyway.
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
        binarizer i1 i2 (outer:oldInners)
          = let (newInn, newRem)
                  = fuseInRule (deIndexRule outer)
                                    ( (getCurPosFromIndex (getRhs outer) i1)
                                    , (getCurPosFromIndex (getRhs outer) i2))
                oldIndices = getIndices outer
                newlyCreatedNTIndex = maximum oldIndices + 1
                newIndices = filter (\i -> i /= i1 && i /= i2)
                                    oldIndices ++ [newlyCreatedNTIndex]
            in (  indexRule newIndices newRem
                : indexRule (repeat undefined) newInn
                : oldInners
               , newlyCreatedNTIndex)
        binarizer _ _ [] = errorHere "binarizeByAdjacency.mergeCandidates.binarizer" "third argument should not be empty"
    -- Now use the rule to evaluate the root - it's time to fully evaluate
    -- all these functions in the inner nodes!
    crunchRule :: NTTree -> [(ProtoRule, Double)]
    crunchRule (NTTreeInner binarizer _)
      = let (binned, _) = binarizer [indexRule [0..] $ ruleToProto fanouts r]
            (  uselessRule@(((lhs, _), _), d)
             : firstRule@(((_, rhs), h'), _)
             : rules)
              = map deIndexRule binned
            newFirstRule = (((lhs, rhs), h'), d)
        in assert (   getRk uselessRule == 1
                   && getFo uselessRule == getFo firstRule) $
           newFirstRule : rules
    crunchRule (NTTreeLeaf _)
      = errorHere "binarizeByAdjacency.crunchRule" "argument must not be NTTreeLeaf"

binarizeUsing
  :: (A.Array NTIdent Fanout -> (Rule, Double) -> [(ProtoRule, Double)]) -- ^ binarizer
  -> PLCFRS
  -> PLCFRS
binarizeUsing binarizer (initials, oldRules, (a_nt, a_t))
  = (initials, ordNub newRules, (invertMap newMap, a_t))
  where
    (newRules, newMap) = intifyProtoRules m_nt
                       $ concatMap (binarizer fanouts) oldRules
    m_nt = invertArray a_nt
    fanouts = getFoNTArrayFromRules oldRules

binarizeRuleSubset
  :: (A.Array NTIdent Fanout -> (Rule, Double) -> [(ProtoRule, Double)]) -- ^ binarizer
  -> ((Rule, Double) -> Bool) -- ^ predicate for which rules to binarize
  -> A.Array NTIdent String -- ^ NT array
  -> [(Rule, Double)] -- ^ all rules
  -> [(Rule, Double)] -- ^ binarized subset of rules
binarizeRuleSubset binarizer pred a_nt fullRules = ordNub newRules
  where
    (newRules, _) = intifyProtoRules m_nt
                  $ concatMap (binarizer fanouts)
                  $ filter pred fullRules
    m_nt = invertArray a_nt
    fanouts = getFoNTArrayFromRules fullRules

-- https://github.com/nh2/haskell-ordnub
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go S.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `S.member` s then go s xs
                                    else x : go (S.insert x s) xs

{-
binarizeMXRS :: M.Map String NTIdent -> MXRS -> (MXRS, M.Map String NTIdent)
binarizeMXRS m_nt inLCFRS
  = (getMXRSFromProbabilisticRules initials (ordNub newRules), newMap)
  where
    (newRules, newMap) = intifyProtoRules m_nt
                       $ concatMap (binarizeByAdjacency fanouts) oldRules
    (initials, oldRules) = toProbabilisticRules inLCFRS
    fanouts = getFoNTArrayFromRules oldRules
-}
