{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Grammar.XRS.LCFRS.IncrementalParser
  ( parse,
    parse',
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

import qualified Data.HashMap.Lazy             as Map

import qualified Data.MultiHashMap             as MMap
import qualified Data.IntMap                   as IMap
import qualified Vanda.Grammar.XRS.LCFRS.Chart as C
--TODO Doch eine 2x HashMap nutzen

data Item nt t wt = Active (IMap.IntMap Range) (Rule nt t) wt Int Range [VarT t] [(Int, [VarT t])] (IMap.IntMap (IMap.IntMap Range)) (IMap.IntMap wt) deriving (Show) 

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



type Container nt t wt = ( C.Chart nt t wt -- Passive Items
                         , [Item nt t wt] -- All Items in Chart
                         )

parse :: forall nt t wt.(Show nt, Show t, Show wt, Hashable nt, Hashable t, Eq t, Ord wt, Weight wt, Ord nt, Converging wt) 
      => WPMCFG nt wt t -- Grammar
      -> Int -- Beam Width
      -> Int -- Max. Amount of Parse Trees
      -> [t] -- Word
      -> [Tree (Rule nt t)]
parse g bw tops w = parse' (prepare g w) bw tops w

parse' :: forall nt t wt.(Show nt, Show t, Show wt, Hashable nt, Hashable t, Eq t, Ord wt, Weight wt, Ord nt, Converging wt) 
       => (MMap.MultiMap nt (Rule nt t, wt), Map.HashMap nt (wt,wt), [nt]) -- prepare Result (RuleMap NT-Rules, IO-Weights NTs, Start NTs)
       -> Int -- Beam Width
       -> Int -- Max. Amount of Parse Trees
       -> [t] -- Word
       -> [Tree (Rule nt t)]
parse' (rmap, iow, s') bw tops w
  = C.parseTrees tops s'
    (singleton $ entire w) -- Goal Item 
  $ (\ (e,  _) -> e) -- parse Trees just needs passive Items from Container
  $ C.chartify (C.empty, []) update rules bw tops
    where
      rules = (initialPrediction w (s' >>= (`MMap.lookup` rmap)) iow)
            : predictionRule w (map snd $ MMap.toList rmap) iow
            : combineRule w iow
            : [complete w iow]

-- Prediction rule for rules of initial nonterminals.
initialPrediction :: forall nt t wt. (Hashable nt, Eq nt, Semiring wt, Eq t, Show nt, Show t, Show wt) 
                  => [t]
                  -> [(Rule nt t, wt)]
                  -> Map.HashMap nt (wt, wt)
                  -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
initialPrediction word srules iow 
  = Left [ (Active IMap.empty r w ri left right'' fs' IMap.empty insides, heuristic)  
      | (r@(Rule ((_, as), (f:fs))), w) <- srules -- TODO Was, wenn Variable Fan-Out 0?
      , let fsindex = prepareComps fs
      , ((ri, right'), fs') <- allCombinations [] (0, f) fsindex
      , (left, right'') <- completeNextTerminals word Epsilon right'
      , let insides = IMap.fromList $ zip [0..] (map (fst . (iow Map.!)) as)
      , let heuristic = w <.> calcInsideWeight insides
      ]

calcInsideWeight :: forall wt. (Semiring wt) => IMap.IntMap wt -> wt
calcInsideWeight insides = foldl (<.>) one (map snd (IMap.toList insides ))

-- give every companent its index
prepareComps :: Function t -> [(Int, [VarT t])]
prepareComps = zip [1..]

-- Get all Componentens of a function with all remaining componentsItems in the second Item
allCombinations :: [(Int, [VarT t])]  -> (Int, [VarT t]) -> [(Int, [VarT t])] -> [((Int, [VarT t]),  [(Int, [VarT t])])]
allCombinations xs x [] = [(x, xs)]
allCombinations xs x y'@(y:ys) = (x, xs ++ y') : (allCombinations (x:xs) y ys)

-- complete Terminals
completeNextTerminals :: (Eq t, Show t)
                    => [t] -- Word
                    -> IMap.IntMap Range -- Already Completed TODO IntMap
                    -> Int -- Curr Index
                    -> Range -- Curr Left
                    -> [VarT t] -- Curr Right
                    -> [(Int, [VarT t])] -- Next Components
                    -> [([(Int, Range)], Int, Range, [VarT t], [(Int, [VarT t])])] -- Completed Components with Index, curr Left, curr Index, next Functions
completeNextTerminals _ cr ri left [] [] = [(cr, ri, r, [], [])]
-- Complete Rule Part
completeNextTerminals w cr ri left [] ((fi, f):fs) = [(cr', ri', Epsilon, right', fs')
    | ((ri', right'), fs') <- allCombinations [] (fi, f) fs
    ] >>= (\(cr, ri, left, right, fs) -> completeNextTerminals w cr ri left right fs)
    where cr' = IMap.insert ri left cr
-- TODO Better Names Vars
-- Scan Rule Part
completeNextTerminals w cr ri left (T t:rights) fs
    = [ left'
        | left' <- mapMaybe (safeConc left) $ singletons t w
       ] >>= (\left -> completeNextTerminals w cr ri left rights fs)
-- Have to Complete in Next Step -> Stop this Method
completeNextTerminals _ cr ri left right@((Var _ _):_) fs = [cr, ri, left, right, fs]
--NIKLAS Warum in Active Parser noch schauen nach Variablen? -> Weil ich evnt. durch komplett eingesetzte NTs schon weitere Var-Ranges habe
--  Prediction rule for rules of not initial nonterminals.
predictionRule :: forall nt t wt. (Hashable nt, Eq nt, Semiring wt, Eq t, Show nt, Show t, Show wt) 
                  => [t]
                  -> [(Rule nt t, wt)]
                  -> Map.HashMap nt (wt, wt)
                  -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
predictionRule word rules iow 
  = Left 
      [ (Active IMap.empty r w ri left right'' fs' IMap.empty insides, heuristic)  
      | (r@(Rule ((a, as), (f:fs))), w) <- rules -- TODO, Was, wenn Fanout 0?
      , let fsindex = prepareComps fs
      , ((ri, right'), fs') <- allCombinations [] (0, f) fsindex
      , (left, right'') <- completeNextTerminals word Epsilon right'
      , let insides = IMap.fromList $ zip [0..] (map (fst . (iow Map.!)) as)
            outside = snd $ iow Map.! a
            heuristic = w <.> (calcInsideWeight insides) <.> outside
      ]

complete :: forall nt t wt. (Show nt, Show t, Show wt, Hashable nt, Eq nt, Eq t, Eq wt, Weight wt)
        => [t] -- Word
        -> Map.HashMap nt (wt, wt) -- weights
        -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
complete word iow = Right app
    where
        app :: Item nt t wt -> Container nt t wt -> [(Item nt t wt, wt)]
        app (Active cr r@(Rule ((a,_), _)) wt ri left [] (f:fs) completions insides) _ -- If item isn't f:fs, then it's finshed and we add it to chart in update function
            =  [(Active cr' r wt ri' left' right'' fs' completions insides, heuristic)
                | ((ri', right'), fs') <- allCombinations [] f fs
                , (left', right'') <- completeNextTerminals word Epsilon right'
                , let cr' = IMap.insert ri left cr
                , let outside = snd $ iow Map.! a 
                      heuristic = wt <.> (calcInsideWeight insides) <.> outside
                ]
        app _ _ = []


combineRule :: forall nt t wt. (Show nt, Show t, Show wt, Hashable nt, Eq nt, Eq t, Weight wt)
        => [t]
        -> Map.HashMap nt (wt, wt) -- weights
        -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
combineRule word iow = Right app
    where
        app :: Item nt t wt -> Container nt t wt -> [(Item nt t wt, wt)]
        app trigger (_, allItems)
         =  [(Active cr r wt ri left' right' fs completions insides, heu)
           | chartItem <- allItems
           , ((Active cr r wt ri left right fs completions insides), heu) <- (consequences trigger chartItem)  ++ (consequences chartItem trigger)
           , (left', right') <- completeNextTerminals word left right
         ] 
    
        consequences :: Item nt t wt -- first Item
                        -> Item nt t wt -- second Item
                        -> [(Item nt t wt, wt)] --NIKLAS Liste nur, damit ich [] zurückgeben kann
        consequences (Active cr rule@(Rule ((_, as), _)) wt ri left ((Var i j):rights) fs completeds insidess) (Active crf (Rule ((a, _), _)) wtf ri' left' [] _ _ insidesf)
            = [(Active cr rule wt ri left'' rights fs completed' insides', heuristic) 
            | j == ri' -- Betrachte ich richtiges Ri? 
            , a == (as!!i) -- Betrache ich richtiges NT?
            , isCompatible (IMap.toList $ fromMaybe IMap.empty (completeds IMap.!? i)) -- All Ranges that are used of this NT are in the current finished Item? If nothing used by now, than empty map instead of Nothing of Maybe
            , left'' <- maybeToList $ safeConc left left'
            , let completed' = doubleInsert completeds i j left' 
                  insides' = IMap.insert i (wtf <.> calcInsideWeight insidesf) insidess 
                  outside =  snd $ iow Map.! a
                  heuristic = wt <.> (calcInsideWeight insides') <.> outside
                ] 
                where isCompatible :: [(Int, Range)] -> Bool
                      isCompatible used = 
                        foldr (checkRange) True used
                      checkRange :: (Int, Range) -> Bool -> Bool
                      checkRange (compi, usedRange) acc = case crf  IMap.!? compi of
                          Just foundRange -> ((usedRange == foundRange) && acc)
                          Nothing -> False

        consequences _ _ = []

doubleInsert :: IMap.IntMap (IMap.IntMap Range) -> Int -> Int -> Range -> IMap.IntMap (IMap.IntMap Range)
doubleInsert m i j r = IMap.insertWith IMap.union i (IMap.singleton j r) m
   
update :: (Show nt, Show t, Show wt, Eq nt, Eq t, Eq wt, Hashable nt, Semiring wt) => Container nt t wt -> Item nt t wt -> (Container nt t wt, Bool)
update (p, allItems) item@(Active cr r@(Rule ((nt, _), _)) wt ri left [] [] completed insides) =
    case getRangevector cr' of
        Just crv ->  case getBacktrace r wt completed of 
            Just bt -> case C.insert p nt crv bt (calcInsideWeight insides) of 
                (p', isnew) -> ((p',  addIfNew item allItems), isnew || (not $ item `elem` allItems))
            Nothing -> ((p, allItems), False)
        Nothing -> ((p, allItems), False)
    where cr' = IMap.insert ri left cr

update (p, allItems) item = ((p, (addIfNew item allItems)), not $ item `elem` allItems) 

getRangevector :: IMap.IntMap Range -> Maybe Rangevector
getRangevector cr = fromList $ map snd $ IMap.toAscList cr 

getBacktrace :: 
    Rule nt t
    -> wt
    -> IMap.IntMap (IMap.IntMap Range) -- Completions
    -> Maybe (C.Backtrace nt t wt)
getBacktrace rule iw completions = 
    case containsANothing rvs of
        Just rvs' -> Just $ C.Backtrace rule iw rvs'  -- Rangevector of every NT
        Nothing -> Nothing
    where rvs = [rv
                | rangesOfOneNT <- (IMap.elems completions)
                , let rv = fromList $ IMap.elems rangesOfOneNT
                ]
--Could each RV of every NT be succesfully calculated
containsANothing :: [Maybe Rangevector] -> Maybe [Rangevector]
containsANothing xs = case null $ filter isNothing xs of 
    True -> Just $ catMaybes xs 
    False -> Nothing

-- Wenn Item noch nicht in Container Liste, dann rein, sonst nicht
addIfNew :: (Eq nt, Eq t, Eq wt) => Item nt t wt -> [Item nt t wt] -> [Item nt t wt]
addIfNew item chart = if item `elem` chart then chart else item:chart
