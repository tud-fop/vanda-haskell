module Vanda.Grammar.XRS.LCFRS.Binarize where

import           Control.Monad.State.Strict
import qualified Data.Array as A
import           Data.List (intercalate)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V
import           Text.Printf (printf)

import           Data.NTT
import           Vanda.Hypergraph.IntHypergraph
import qualified Vanda.Hypergraph.Tree as VT
import           Vanda.Corpus.Negra.Text (parseNegra)
import           Vanda.Util.Memorysavers (invertMap)

import           Vanda.Grammar.XRS.LCFRS
import           Vanda.Grammar.XRS.LCFRS.Extraction
import           Vanda.Grammar.XRS.LCFRS.Evaluation
import Debug.Trace

type ProtoNT = Either String Int
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
    intifyProtoNT (Left s)  = do oldmap <- get
                                 case M.lookup s oldmap of
                                   Just i  -> return i
                                   Nothing -> let i = M.size oldmap
                                              in put (M.insert s i oldmap) >> return i

data TriState = YesPlease | Perhaps | HellNo deriving Show
binarizeNaively :: (ProtoRule, Double) -> [(ProtoRule, Double)]
binarizeNaively r@(((lhs, rhs), h'), d)
  | getRk r <= 2 = [r]
  | otherwise = trace (unlines $ map (\m -> show m ++ "    " ++ show (isExtractable m)) oldH ++ ["\n\n"])
              $ trace (show offset ++ "\n\n")
              $ trace ((\(r, i) -> unlines (map show r) ++ "\n---\n" ++ unlines (map show i)) $ splitH $ map transMe "oyyonpnyonppnynyy") -- only [o*y*o*] should come out
              $ innerRule : binarizeNaively remainderRule
  where
    transMe 'n' = Nothing
    transMe 'y' = Just $ nt 100
    transMe 'o' = Just $ tt 100
    transMe 'p' = Just $ tt 0
    -- capital H homomorphisms are akin to the characteristic strings you can create from tuples by replacing the tuples commas with delimiters (Nothing)
    [b1, b2] = reverse . take 2 . reverse $ rhs
    outerNTs = init $ init rhs
    remainderRule = (((lhs, outerNTs ++ [newNT]), resplit remH), d)
    innerRule = (((newNT, [b1, b2]), resplit innH), 1.0)
    newNT = Left $ show [b1, b2] ++ show innH
    oldH = intercalate [Nothing] $ map (map Just) h'
    (remH, innH) = splitH oldH
    splitH someH = let (rs,is,_,_,_) = foldl inspect ([], [], [], HellNo, offset) (someH ++ [Nothing])
                   in (reverse (tail rs), map readjustNT $ reverse (tail is)) -- tail rs removes the just added Nothing (so every extract ends), tail is remove the Nothing added at every extract end
    inspect (remH, innH, perhapsH, state, i) maybeNTT
      = let into xs = maybeNTT : xs
        in case (state, isExtractable maybeNTT) of -- state: YesPlease = were in a real extraction, Perhaps = might become an extraction, only read ts so far, HellNo = only reading Nothings or outer NTs
             (YesPlease, YesPlease) -> (      remH,                   into innH,       []             , YesPlease, i)
             (YesPlease, Perhaps)   -> (      remH,                   into innH,       []             , YesPlease, i)
             (YesPlease, HellNo)    -> ( into ((Just $ NT i) : remH), Nothing : innH,  []             , HellNo, i + 1)
             (Perhaps, YesPlease)   -> (      remH,        into (perhapsH++innH),      []             , YesPlease, i)
             (Perhaps, Perhaps)     -> (      remH,                        innH,   maybeNTT : perhapsH, Perhaps, i)
             (Perhaps, HellNo)      -> ( into (perhapsH++remH),            innH,       []             , HellNo, i)
             (HellNo, YesPlease)    -> (      remH,                   into innH,       []             , YesPlease, i)
             (HellNo, Perhaps)      -> (      remH,                        innH,   maybeNTT : perhapsH, Perhaps, i)
             (HellNo, HellNo)       -> ( into remH,                        innH,       []             , HellNo, i)
    resplit [] = []
    resplit s = let (xs, ys) = span (/= Nothing) s
                in (map fromJust xs) : resplit (drop 1 ys)
    isExtractable Nothing = HellNo
    isExtractable (Just (T _)) = Perhaps
    isExtractable (Just (NT i)) = if i >= offset then YesPlease else HellNo
    offset = sum $ map getFoNT $ outerNTs
    readjustNT (Just (NT i)) = Just (NT  $ i - offset)
    readjustNT x = x
    -- TODO take from *once* generated list
    getFoNT (Right 2) = 1
    getFoNT (Right 30) = 1
    getFoNT (Right 42) = 2
    getFoNT (Right 7) = 1
    getFoNT (Left "S") = 2
    getFoNT (Left "A") = 2
    getFoNT (Left "B") = 1
    getFoNT (Left "C") = 2

getRk, getFo :: (((a, [a]), [b]), c) -> Int
getRk (((_, rhs),  _), _) = length rhs
getFo (((_, _  ), h'), _) = length h'


-- This is what I'm currently working on so this is where the main function stays.

main :: IO ()
main = do
       corpusText' <- TIO.readFile "/home/sjm/programming/LCFRS/tiger_release_aug07.export"
       
       let (countRuleMap, (a_nt, a_t)) = extractCountedRulesFromNegra $ parseNegra corpusText'
           
           pRuleMap = normalizeRuleProbs countRuleMap
           flatCountRuleMap = M.foldl' M.union M.empty countRuleMap
           flatPRuleMap = M.foldl' M.union M.empty pRuleMap
           
           rulesAndCounts = M.assocs flatCountRuleMap
           rulesAndProbs = M.assocs flatPRuleMap
           myLCFRS = getMXRSFromProbabilisticRules rulesAndProbs [0] -- assumptions, assumptions...
       
       -- possibly expensive binarizations
       -- print (length $ filter (\r -> getFo r > 2 && getRk r > 2) rulesAndProbs)
       
       -- NTs gathered so far
       -- print (A.bounds a_nt)
       
       -- writeFile "/tmp/nt_dict" (unlines $ map (\(i, s) -> printf "%3d %s" i s) $ A.assocs a_nt)
       -- writeFile "/tmp/t_dict" (unlines $ map (\(i, s) -> printf "%5d %s" i s) $ A.assocs a_t)
       -- writeFile "/tmp/rules" (unlines $ map (\(r, d) -> printf "%1.4f %s" d (retranslateRule a_nt a_t r)) rulesAndProbs)

       -- picking some rules
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
       let makeRealRuleTree rs = fmap (\i -> mkHyperedge (fst . fst $ rs !! i) (snd . fst $ rs !! i) i i)
       let dTree = makeRealRuleTree ruleList intDTree
       
       -- print $ getDerivProbability myLCFRS dTree
       -- print $ sententialFront (irtg myLCFRS) a_nt a_t dTree
       
       let binNr = binarizeNaively
                 . ruleToProto
                 . (rulesAndProbs !!)
       
       -- print $ binNr s2__s1_vmfin1_vp2_np1
       let my_a_t = A.array (0,5) [(0,"a"), (1,"A"), (2, "B"), (3, "C"), (4,"AA"), (5, "CC")]
           myProtoRules = [(((Left "S", [Left "A", Left "B", Left "C"]), [[nt 0, tt 0, nt 2, nt 1], [nt 3, nt 4]]), 0.5), (((Left "A", []), [[tt 1], [tt 4]]), 0.5), (((Left "B", []), [[tt 2]]), 0.5), (((Left "C", []), [[tt 3], [tt 5]]), 0.5)]
           (myCleanRules, my_m_nt) = intifyProtoRules M.empty myProtoRules
           newRules = binarizeNaively $ head myProtoRules
           (cleanNewRules, my_new_m_nt) = intifyProtoRules my_m_nt newRules
           
           uMXRS = getMXRSFromProbabilisticRules myCleanRules [0]
           bMXRS = getMXRSFromProbabilisticRules (cleanNewRules ++ tail myCleanRules) [0]
           uDeriv = makeRealRuleTree (map fst myCleanRules) $ VT.node 0 [VT.node 1 [], VT.node 2 [], VT.node 3 []]
           bDeriv = makeRealRuleTree (map fst $ cleanNewRules ++ tail myCleanRules) $ VT.node 1 [VT.node 2 [], VT.node 0 [VT.node 3 [], VT.node 4 []]] -- 1 and 0 come from the S-Rule, 2,3,4 are for the others
       
       putStrLn "New rules look like this:"
       mapM_ (putStrLn . retranslateRule (invertMap my_new_m_nt) my_a_t) $ map fst cleanNewRules
       
       putStrLn "The old rule yields:"
       print $ sententialFront (irtg uMXRS) (invertMap my_m_nt) my_a_t uDeriv
       print $ getDerivProbability uMXRS uDeriv
       
       putStrLn "The new rules yield:"
       print $ sententialFront (irtg bMXRS) (invertMap my_new_m_nt) my_a_t bDeriv
       print $ getDerivProbability bMXRS bDeriv
