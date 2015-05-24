module Vanda.Grammar.XRS.LCFRS.Binarize where

import           Control.Exception.Base (assert)
import           Control.Monad.State.Strict
import qualified Data.Array as A
import           Data.List (intercalate)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import qualified Data.Text.Lazy.IO as TIO
import           Data.Tuple (swap)

import           Data.NTT
import           Vanda.Hypergraph.IntHypergraph
import qualified Vanda.Hypergraph.Tree as VT
import           Vanda.Corpus.Negra.Text (parseNegra)
import           Vanda.Util.Memorysavers (invertMap)

import           Vanda.Grammar.XRS.LCFRS
import           Vanda.Grammar.XRS.LCFRS.Extraction
import           Vanda.Grammar.XRS.LCFRS.Evaluation
import Debug.Trace

getRk, getFo :: (((a, [a]), [b]), c) -> Int
getRk (((_, rhs),  _), _) = length rhs
getFo (((_, _  ), h'), _) = length h'

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

binarizeNaively :: A.Array Int Int -> (ProtoRule, Double) -> [(ProtoRule, Double)]
binarizeNaively fanouts r@(((_, rhs), _), _)
  | getRk r <= 2 = [r]
  | otherwise = innerRule : binarizeNaively fanouts remainderRule
  where
    (innerRule, remainderRule) = extractFromRule fanouts r (length rhs - 2, length rhs - 1)


data TriState = Yes | Perhaps | No deriving Show

extractFromRule
  :: A.Array Int Int -- ^ fanouts
  -> (ProtoRule, Double) -- ^ rule
  -> (Int, Int) -- ^ indices of the NTs that are to be extracted (ascending order!)
  -> ((ProtoRule, Double), (ProtoRule, Double)) -- ^ extracted rule, remainder rule
extractFromRule fanouts (((lhs, rhs), h'), d) (posB1, posB2) = (innerRule, remainderRule)
  where
    -- Conventions I would like to adhere to: 'v' for variables in the homomorphisms, 'n' for NT positions, 'bi' for NTs and no more ambiguous 'i's
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
    oldH = intercalate [Nothing] $ map (map Just) h'
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
              | getNTPosOfVar v < posB1 = 0
              | getNTPosOfVar v < posB2 = getFoNT b1
              | otherwise               = getFoNT b1 + getFoNT b2
            intoRemain xs = case maybeNTT of
                              (Just (NT v)) -> (Just $ NT $ v - offsetAtTheRemainingNT v) : xs
                              x -> x : xs
            -- the extraced rule should have variables start at 0, so shift it back w.r.t. where it started
            -- Again, same idea as above.
            offsetAtTheInnerNT v
              | getNTPosOfVar v == posB1 = offset1
              | getNTPosOfVar v == posB2 = offset1 + offset2
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
    isExtractable (Just (NT v)) = if getNTPosOfVar v `elem` [posB1, posB2] then Yes else No
    getNTPosOfVar v = let searchWorker i ((pos, fo) : xs) = if i + fo > v then pos else searchWorker (i + fo) xs
                      in searchWorker 0 $ zip [0..] $ map getFoNT rhs
    getFoNT (Right i) = fanouts A.! i
    getFoNT (Left (_, i)) = i

invertArray :: (Ord x) => A.Array Int x -> M.Map x Int
invertArray = M.fromList . map swap . A.assocs

getFoNTArrayFromRules :: [(Rule, Double)] -> A.Array Int Int -- ^ from NTs into fanouts
getFoNTArrayFromRules = toArray . foldl worker M.empty
  where
    toArray m = A.array (0, M.size m) $ M.assocs m
    worker m (((lhs, _), h'), _) = case M.lookup lhs m of
                                     Nothing -> M.insert lhs (length h') m
                                     Just _  -> m

-- This is what I'm currently working on so this is where the main function stays.

main :: IO ()
main = do
       -- {-
       corpusText' <- TIO.readFile "/home/sjm/programming/LCFRS/tiger_release_aug07.export"
       
       let (countRuleMap, (a_nt, a_t)) = extractCountedRulesFromNegra $ parseNegra corpusText'
           
           pRuleMap = normalizeRuleProbs countRuleMap
           flatCountRuleMap = M.foldl' M.union M.empty countRuleMap
           flatPRuleMap = M.foldl' M.union M.empty pRuleMap
           
           rulesAndCounts = M.assocs flatCountRuleMap
           rulesAndProbs = M.assocs flatPRuleMap
           myLCFRS = getMXRSFromProbabilisticRules rulesAndProbs [0] -- assumptions, assumptions...
       -- -}
       
       let makeRealRuleTree rs = fmap (\i -> mkHyperedge (fst . fst $ rs !! i) (snd . fst $ rs !! i) i i)
       
       {-
       -- Cool example on real data
       let e1__s2_xy1 = 709 - 1 -- [["0","2","1"]]
           xy1__eqsign = 139961 - 1 -- [["="]]
           s2__s1_vmfin1_vp2_np1 = 106222 - 1 -- [["0"],["1","2","4","3"]]
           s1__s1 = 2717 - 1 -- [["0"]]
           s1__nn1 = 4939 - 1 -- [["0"]]
           nn1__mai = 41180 - 1 -- [["Mai"]]
           vmfin1__muesste = 117459 - 1 -- [["müßte"]]
           vp2__adv1_vvinf1 = 123147 - 1 -- [["0"], ["1"]]
           np1__nn1 = 26488 - 1 -- [["0"]]
           nn1__arbeit = 41675 - 1 -- [["Arbeit"]]
           adv1__irgendwann = 25256 - 1 -- [["irgendwann"]]
           vvinf1__schreiben = 115067 - 1 -- [["schreiben"]]
       
       let intDTree = VT.node e1__s2_xy1 [
                        VT.node s2__s1_vmfin1_vp2_np1 [
                          VT.node s1__s1 [
                            VT.node s1__nn1 [
                              VT.node nn1__mai []
                            ]
                          ],
                          VT.node vmfin1__muesste [],
                          VT.node vp2__adv1_vvinf1 [
                            VT.node adv1__irgendwann [],
                            VT.node vvinf1__schreiben []
                          ],
                          VT.node np1__nn1 [
                            VT.node nn1__arbeit []
                          ]
                        ],
                        VT.node xy1__eqsign []
                      ]
       let ruleList = map fst rulesAndProbs
       let dTree = makeRealRuleTree ruleList intDTree
       
       print $ getDerivProbability myLCFRS dTree
       print $ sententialFront (irtg myLCFRS) a_nt a_t dTree
       
       let binNr = binarizeNaively (getFoNTArrayFromRules rulesAndProbs)
                 . ruleToProto
                 . (rulesAndProbs !!)
       
       let (binFatRules, new_fat_m_nt) = intifyProtoRules (invertArray a_nt) $ binNr s2__s1_vmfin1_vp2_np1
       mapM_ (putStrLn . retranslateRule (invertMap new_fat_m_nt) a_t) $ map fst $ binFatRules
       -- -}
       
       -- Smaller Test:
       
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
           newRules = binarizeNaively (getFoNTArrayFromRules myCleanRules) $ ruleToProto $ head myCleanRules
           (cleanNewRules, my_new_m_nt) = intifyProtoRules my_m_nt newRules
           
           uMXRS = getMXRSFromProbabilisticRules myCleanRules [0]
           bMXRS = getMXRSFromProbabilisticRules (cleanNewRules ++ tail myCleanRules) [0]
           uDeriv = makeRealRuleTree (map fst myCleanRules) $ VT.node 0 [VT.node 1 [], VT.node 2 [], VT.node 3 []]
           bDeriv = makeRealRuleTree (map fst $ cleanNewRules ++ tail myCleanRules)
                  $ VT.node 1 [
                        VT.node 2 [],
                        VT.node 0 [
                            VT.node 3 [], VT.node 4 []
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
       
       putStrLn "All done!"
       -- -}
       
       -- More batch testing
       let rule1 = (((Right 0, [Right 1]), [[NT 0]]), 0.5)
           test1 [(((Right 0, [Right 1]), [[NT 0]]), 0.5)] = True
           rule2 = (((Left ("S", 1), [Left ("A", 3), Left ("B", 3), Left ("C", 3)]), [[NT 0, NT 3, NT 6, NT 1, NT 4, NT 7, NT 2, NT 5, NT 8]]), 0.5)
           test2 [(((Left (x, 3), [Left ("B", 3), Left ("C", 3)]), [[NT 0, NT 3], [NT 1, NT 4], [NT 2, NT 5]]), 1.0), (((Left ("S", 1), [Left ("A", 3), Left (y, 3)]), [[NT 0, NT 3, NT 1, NT 4, NT 2, NT 5]]), 0.5)] = x == y
           rule3 = (((Left ("S", 1), [Left ("A", 3), Left ("B", 3), Left ("C", 3)]), [[NT 0, T 0, NT 3, NT 6, NT 1, NT 4, NT 7, NT 2, NT 5, NT 8, T 1]]), 0.5)
           test3 [(((Left (x, 3), [Left ("B", 3), Left ("C", 3)]), [[T 0, NT 0, NT 3], [NT 1, NT 4], [NT 2, NT 5, T 1]]), 1.0), (((Left ("S", 1), [Left ("A", 3), Left (y, 3)]), [[NT 0, NT 3, NT 1, NT 4, NT 2, NT 5]]), 0.5)] = x == y
           rule4 = (((Left ("S", 1), [Left ("A", 3), Left ("B", 3), Left ("C", 3)]), [[NT 0, T 0, NT 3, NT 6, NT 1, NT 4, NT 7, NT 2, NT 5, NT 8, T 1]]), 0.5)
           test4 ((((Left (x, 4), [Left ("A", 3), Left ("C", 3)]), [[NT 0, T 0], [NT 3, NT 1], [NT 4, NT 2], [NT 5, T 1]]), 1.0), (((Left ("S", 1), [Left ("B", 3), Left (y, 4)]), [[NT 3, NT 0, NT 4, NT 1, NT 5, NT 2, NT 6]]), 0.5)) = x == y
       putStrLn $ if test1 (binarizeNaively undefined rule1) then "yay" else "nay"
       putStrLn $ if test2 (binarizeNaively undefined rule2) then "yay" else "nay"
       putStrLn $ if test3 (binarizeNaively undefined rule3) then "yay" else "nay"
       putStrLn $ if test4 (extractFromRule undefined rule4 (0,2)) then "yay" else "nay"

binarizeMXRS :: M.Map String Int -> MXRS -> (MXRS, M.Map String Int)
binarizeMXRS m_nt inLCFRS = (getMXRSFromProbabilisticRules newRules [0], newMap)
  where
    (newRules, newMap) = intifyProtoRules m_nt $ concatMap (binarizeNaively fanouts . ruleToProto) oldRules
    oldRules = toProbabilisticRules inLCFRS
    fanouts = getFoNTArrayFromRules oldRules
