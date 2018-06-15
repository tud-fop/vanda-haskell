{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Grammar.XRS.LCFRS.IncrementalParser
  ( testParse,
    parse,
    parse',
    exampleGrammar,
    Container
  ) where

-- Prepare: 1. Regelsortieren (S -> [Rules, die mit S starten], A ->...),  2. NT -> (inputw, outputw), Alle Start-NT mit inputw >0
-- Default werte: Weight - 1, Beam Width - 10000, max number of ret. Trees -1 (!= Fanout)

import Data.Hashable (Hashable(hashWithSalt))
import Data.Converging (Converging)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList,catMaybes, isNothing)
import Data.Range
import Data.Semiring
import Data.Tree (Tree)
import Data.Weight
import Vanda.Grammar.PMCFG
import Debug.Trace(trace)

import qualified Data.HashMap.Lazy             as Map

import qualified Data.MultiHashMap             as MMap
import qualified Data.IntMap                   as IMap
import qualified Data.HashSet                  as Set
import qualified Vanda.Grammar.XRS.LCFRS.Chart as C

testParse :: String
testParse = "File Connected"


exampleGrammar :: String
exampleGrammar = prettyPrintWPMCFG prettyShowString prettyShowString exampleWPMCFG

prettyShowString :: (Show w) => w -> String
prettyShowString s = '\"' : concatMap g (show s) ++ "\"" where
  g c    = [c]

trace' :: (Show s) => String -> s -> s
trace' prefix s = trace ("\n" ++ prefix ++": "++ (show s) ++ "\n") s

data Item nt t wt = Active (IMap.IntMap Range) (Rule nt t) wt Int Range [VarT t] [(Int, [VarT t])] (IMap.IntMap (IMap.IntMap Range)) (IMap.IntMap wt) deriving (Show) 
-- TODONew Gewichte als Map + in Combine aktulaisern

instance (Eq nt, Eq t) => Eq (Item nt t wt) where
  (Active cr r _ ri left right nc completions _) == (Active cr' r' _ ri' left' right' nc' completions' _) 
    =  cr          == cr'
    && r           == r' 
    && ri          == ri'
    && left        == left'
    && right       == right'
    && nc          == nc'
    && completions == completions'


instance (Hashable nt, Hashable t) => Hashable (Item nt t wt) where
  salt `hashWithSalt` (Active _ r _ _ left _ _ _ _) 
    = salt `hashWithSalt` r `hashWithSalt` left


--TODO 
{-instance (Show nt, Show t) => Show (Item nt t wt) where
  show (Active r _ _ ri left right _ _ _)
    = "[Active] " ++ show r ++ "\n" 
    ++ "current status: Ri:" ++ show(ri)++  ", " ++ show (left) ++ " • " ++ prettyPrintComposition show [right] -- TODO Ausführlicher
-}

type Container nt t wt = ( C.Chart nt t wt -- Passive Items
                         , MMap.MultiMap (nt, Int) (Item nt t wt) --Map Variablen onto List of Active Items, that need NT in the next Step
                         , Set.HashSet nt -- All NTs, which are not init. right now
                         , MMap.MultiMap (nt, Int) (Item nt t wt) -- Map, welche zeigt, wer alles schon Variable aufgelöst hat (known)
                         , [Item nt t wt] -- All Items in Chart(all), TODO Optimize this
                         , [Rule nt t]  --All Rules
                         )

parse :: forall nt t wt.(Show nt, Show t, Show wt, Hashable nt, Hashable t, Eq t, Ord wt, Weight wt, Ord nt, Converging wt) 
      => WPMCFG nt wt t -- Grammar
      -> Int -- Beam Width
      -> Int -- Max. Amount of Parse Trees
      -> [t] -- Word
      -> [Tree (Rule nt t)]
parse g bw tops w = parse' (prepare g w) bw tops w

parse' :: forall nt t wt.(Show nt, Show t, Show wt, Hashable nt, Hashable t, Eq t, Ord wt, Weight wt, Ord nt, Converging wt) 
       => (MMap.MultiMap nt (Rule nt t, wt), Map.HashMap nt (wt,wt), [nt]) -- prepare Result (RuleMap NT-Rules, IO-Weights NTs, Reachable Items
       -> Int -- Beam Width
       -> Int -- Max. Amount of Parse Trees
       -> [t] -- Word
       -> [Tree (Rule nt t)]
parse' (rmap, iow, s') bw tops w
  = trace' "Trees" (C.parseTrees tops (trace' "Start Rules" s')
    (singleton $ entire w) -- Goal Item 
  $ (\ (e, _, _, _, _, _) -> (trace' "Passive Items End" e)) -- parse Trees just needs passive Items from Container
   $ (\container@(_,_,_,_,all,_) -> (trace ("\nAll Items End: " ++ ( show all) ++ "\n") container))
  $ C.chartify (C.empty, MMap.empty, nset, MMap.empty, [], (map fst $ map snd $ MMap.toList rmap)) update rules bw tops)
    where
      nset = Set.fromList $ filter (not . (`elem` s')) $ Map.keys rmap
      
      rules = (initialPrediction w (s' >>= (`MMap.lookup` rmap)) iow)
            : predictionRule w (trace' "All Rules" (map snd $ MMap.toList (trace' "Map with all Rules" rmap))) iow -- Mache aus Rule Map eine Liste aller Rules
            : scanRule w iow -- Mache aus Rule Map eine Liste aller Rules
            : combineRule w iow
            : [complete w iow]

-- | Prediction rule for rules of initial nonterminals.
-- Predicted alles, bei dem Terminale am Anfang stehen und Startsymbol auf lhs hat
initialPrediction :: forall nt t wt. (Hashable nt, Eq nt, Semiring wt, Eq t, Show nt, Show t, Show wt) 
                  => [t]
                  -> [(Rule nt t, wt)]
                  -> Map.HashMap nt (wt, wt)
                  -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
initialPrediction word srules ios 
  = Left 
      (trace' "initPred" [ (Active IMap.empty r w ri left right'' fs' IMap.empty insides, heuristic)  
      | (r@(Rule ((_, as), (f:fs))), w) <- (trace' "Rules" srules) -- TODO Was, wenn Variable Fan-Out 0?
      , let fsindex = prepareComps fs
      , ((ri, right'), fs') <- trace' "allComb" (allCombinations [] (0, f) fsindex)
      , (left, right'') <- completeNextTerminals word Epsilon right'
      , let insides = IMap.fromList $ zip [0..] (map (fst . (ios Map.!)) as)
      , let heuristic = w <.> calcInsideWeight insides
      ] )

calcInsideWeight :: forall wt. (Semiring wt) => IMap.IntMap wt -> wt
calcInsideWeight insides = foldl (<.>) one (map snd (IMap.toList insides ))

-- give every companent its index
prepareComps :: Function t -> [(Int, [VarT t])]
prepareComps = zip [1..]

-- Get all Componentens of a function with all remaining components
allCombinations :: [(Int, [VarT t])]  -> (Int, [VarT t]) -> [(Int, [VarT t])] -> [((Int, [VarT t]),  [(Int, [VarT t])])]
allCombinations xs x [] = [(x, xs)]
allCombinations xs x y'@(y:ys) = (x, xs ++ y') : (allCombinations (x:xs) y ys)

-- complete Terminals
completeNextTerminals :: (Eq t, Show t)
                    => [t] 
                    -> Range
                    -> [VarT t]
                    -> [(Range, [VarT t])]
completeNextTerminals _ r [] = [(r, [])]
completeNextTerminals w r (T t:fs)
    = trace' ("comT;" ++ show t ++ show fs) [ (r', fs)
        | r' <- mapMaybe (safeConc r) $ singletons t w
       ] >>= uncurry (completeNextTerminals w )
--Warum in Active Parser noch schauen nach Variablen? -> Weil ich evnt. durch komplett eingesetzte NTs schon weitere Var-Ranges habe
completeNextTerminals _ r fs@((Var _ _):_) = [(r, fs)]
completeNextTerminals _ _ _ = []
-- | Prediction rule for rules of initial nonterminals.
-- Predicted alles, bei dem Terminale am Anfang stehen 
-- TODO Mit initPred zusammenwerfen?
predictionRule :: forall nt t wt. (Hashable nt, Eq nt, Semiring wt, Eq t, Show nt, Show t, Show wt) 
                  => [t]
                  -> [(Rule nt t, wt)]
                  -> Map.HashMap nt (wt, wt)
                  -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
predictionRule word rules ios 
  = Left 
      (trace' "Pred" [ (Active IMap.empty r w ri left right'' fs' IMap.empty insides, heuristic)  --TODO Darf fs theoretisch nicht durch [] ersetzen, aber da es sowieso bald wegkommt, ist das egal
      | (r@(Rule ((a, as), (f:fs))), w) <- (trace' "Rules" rules) -- TODO, was ist, wenn Rule nicht f:fs ist, sondern nur 1 ELement ist. Geht das überhaupt?
      , let fsindex = prepareComps fs
      , ((ri, right'), fs') <- trace' "allCombpR" (allCombinations [] (0, f) fsindex)
      , (left, right'') <- trace' ("complTok" ++ show word ++ show right' ) (completeNextTerminals word Epsilon right')
      --, (left, right,ri) <- completeKnownTokensWithRI word fs 0 -- Jede Funktion einmal komplete known Tokens übegeben -> 1 Item für jedes Ri
      , let insides = IMap.fromList $ zip [0..] (map (fst . (ios Map.!)) as)
            outside = snd $ ios Map.! a
            heuristic = w <.> (calcInsideWeight insides) <.> outside
      ] )

scanRule :: forall nt t wt. (Show nt, Show t, Show wt, Hashable nt, Eq nt, Eq t, Weight wt)
        => [t] -- Word
        -> Map.HashMap nt (wt, wt) -- weights
        -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
scanRule word iow = Right app
    where
        app :: Item nt t wt -> Container nt t wt -> [(Item nt t wt, wt)]
        app (Active cr r@(Rule ((a, _), _)) wt ri left right fs completions insides) _ 
            = trace' "Scan"[((Active cr r wt ri left' right' fs completions insides), heuristic)
            |(left', right')  <- completeNextTerminals word left right -- Klammer ist hier nur, damit ich das so mit <- schreiben kann
            , let outside = snd $ iow Map.! a -- Doesn't Change from Prediction
                  heuristic = wt <.> (calcInsideWeight insides) <.> outside
                ]
        app _ _ = []



complete :: forall nt t wt. (Show nt, Show t, Show wt, Hashable nt, Eq nt, Eq t, Eq wt, Weight wt)
        => [t] -- Word
        -> Map.HashMap nt (wt, wt) -- weights
        -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
-- TODO Parameter von Ded-Regeln anpassen, brauche z.B. ios bei Scan nicht oder word bei complete
complete word ios = Right app
    where
        app :: Item nt t wt -> Container nt t wt -> [(Item nt t wt, wt)]
--        app trigger@(Active cr r w ri left [] [] completions ios) _
 --               |found <- (findPassiveForAllRules' (trigger:allI) allR)
  --              , let cr' = IMap.insert ri left cr
   --             ]
        app trigger@(Active cr r@(Rule ((a,_), _)) wt ri left [] (f:fs) completions insides) _ -- If item isn't f:fs, then it's finshed, so we add it to chart in update function
            = trace' "Skip" [(Active cr' r wt ri' left' right'' fs' completions insides, heuristic) -- TODO Fix weight
                | ((ri', right'), fs') <- allCombinations [] f fs
                , (left', right'') <- completeNextTerminals word Epsilon right'
                , let cr' = IMap.insert ri left cr
                , let outside = snd $ ios Map.! a -- Doesn't Change from Prediction
                      heuristic = wt <.> (calcInsideWeight insides) <.> outside
                ]
        app _ _ = []

combineRule :: forall nt t wt. (Show nt, Show t, Show wt, Hashable nt, Eq nt, Eq t, Weight wt)
        => [t] -- Word
        -> Map.HashMap nt (wt, wt) -- weights
        -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
combineRule word ios = Right app
    where
        app :: Item nt t wt -> Container nt t wt -> [(Item nt t wt, wt)]
        app trigger (_, _, _, _, all, _)
         = trace' "Combine" [consequence
           | chartItem <- trace' "all in Combine" all
           , consequence <- (consequences trigger chartItem)  ++ (consequences chartItem trigger)
         ] 
        app trigger _ = trace ("Combine - Not Matched " ++ show trigger) []
    
        consequences :: Item nt t wt -- first Item
                        -> Item nt t wt -- second Item
                        -> [(Item nt t wt, wt)] -- Liste nur, damit ich [] zurückgeben kann
        consequences searcher@(Active cr rule@(Rule ((_, as), _)) wt ri left ((Var i j):rights) fs completeds insidess) finished@(Active crf r@(Rule ((a, _), _)) wtf ri' left' [] _ _ insidesf)
            = trace' ("Consequences - First Item searches Var" ++ "\nSearch Item:" ++ (show searcher) ++ "\nFinish Item:"  ++ (show finished)) [(Active cr rule wt ri left'' rights fs completed' insides', heuristic) 
            | j == ri' -- Betrachte ich richtiges Ri? 
            , a == (as!!i) -- Betrache ich richtiges NT?
            , isCompatible (IMap.toList $ fromMaybe IMap.empty (completeds IMap.!? i)) -- All Ranges that are used of this NT are in the current finished Item? If nothing used by now, than empty map instead of Nothing of Maybe
            , left'' <- maybeToList $ safeConc left left'
            , let completed' = doubleInsert completeds i j left' 
                  insides' = IMap.insert i (wtf <.> calcInsideWeight insidesf) insidess -- THOMAS so richtig?
                  outside =  snd $ ios Map.! a
                  heuristic = wt <.> (calcInsideWeight insides') <.> outside
                ] 
                where isCompatible :: [(Int, Range)] -> Bool
                      isCompatible used = 
                        foldr (checkRange) True used
                      checkRange :: (Int, Range) -> Bool -> Bool
                      checkRange (i, usedRange) acc = case crf  IMap.!? i of
                          Just foundRange -> ((usedRange == foundRange) && acc)
                          Nothing -> False

        consequences _ _ = trace' "Consequences - Not Matched (On One Combination)"[]

doubleInsert :: IMap.IntMap (IMap.IntMap Range) -> Int -> Int -> Range -> IMap.IntMap (IMap.IntMap Range)
doubleInsert map i j r = IMap.insertWith IMap.union i (IMap.singleton j r) map
   
update :: (Show nt, Show t, Show wt, Eq nt, Eq t, Eq wt, Hashable nt, Semiring wt) => Container nt t wt -> Item nt t wt -> (Container nt t wt, Bool)
-- TODONew Chart kürzen + Optimieren (Zuerst Rules rausschmeißen, Dann AllItems in eine Map
update (p, a, n, k, all, allRules) item@(Active cr r@(Rule ((nt, _), _)) wt ri left [] [] completed insides) =
    case getRangevector cr' of
        Just crv ->  case getBacktrace r wt completed of 
            Just bt -> case C.insert p nt crv bt (calcInsideWeight insides) of -- THOMAS richtig?
                (p', isnew) -> ((p', a, n, k, item:all, allRules), isnew || (not $ item `elem` all))
            Nothing -> ((p, a, n, k, all, allRules), False)
        Nothing -> ((p, a, n, k, item:all, allRules), False)
    where cr' = IMap.insert ri left cr

update (p, a, n, k, all, allRules) item = ((p,a,n, k, trace' ("Update - All Items Without New Passive" ++ (show $ not $ item `elem` all)) (addIfNew item all), allRules), not $ item `elem` all) -- Nicht neu

getRangevector :: IMap.IntMap Range -> Maybe Rangevector
getRangevector cr = fromList $ map snd $ IMap.toAscList cr 

getBacktrace :: 
    Rule nt t
    -> wt
    -> IMap.IntMap (IMap.IntMap Range) -- Completions
    -> Maybe (C.Backtrace nt t wt)
getBacktrace rule iw completions = 
    case containsANothing rvs of
        Just rvs' -> Just $ C.Backtrace rule iw (trace' "Backtrace" rvs')  -- Rangevectors von jedem NT  - Ist das so richtig?
        Nothing -> Nothing
    where rvs = [rv
                | rangesOfOneNT <- trace' "Backtrace Completions" (IMap.elems completions)
                , let rv = fromList $ IMap.elems rangesOfOneNT
                ]

containsANothing :: [Maybe Rangevector] -> Maybe [Rangevector]
containsANothing xs = case null $ filter isNothing xs of 
    True -> Just $ catMaybes xs--Every RV was succefully calculated
    False -> Nothing

-- Wenn Item noch nicht in Chart Liste, dann rein, sonst nicht
addIfNew :: (Eq nt, Eq t, Eq wt) => Item nt t wt -> [Item nt t wt] -> [Item nt t wt]
addIfNew item chart = if item `elem` chart then chart else item:chart
