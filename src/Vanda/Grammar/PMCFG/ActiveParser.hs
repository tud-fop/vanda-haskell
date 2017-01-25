module Vanda.Grammar.PMCFG.ActiveParser
    ( parse
    , weightedParse
    ) where

import Vanda.Grammar.PMCFG (PMCFG(..), WPMCFG(..), Rule(..), VarT(..), prettyPrintRule, prettyPrintComposition)
import Vanda.Grammar.PMCFG.CYKParser (Derivation(..), Range, Rangevector, Function, prettyPrintRangevector, node)
import Vanda.Grammar.PMCFG.WeightedDeductiveSolver (WeightedDeductiveSolver(..), DeductiveRule(..), solve, Cost(..), Dividable(divide))

import qualified Data.Map.Strict as Map
import Data.Tree (Tree)
import Data.List (sort, elemIndices)
import Data.Maybe (mapMaybe, maybeToList)

parse :: (Ord nt, Ord t) => PMCFG nt t -> [t] -> [Tree (Rule nt t)]
parse (PMCFG s rs) = weightedParse $ WPMCFG s $ zip rs $ repeat (Cost 1 :: Cost Double)

weightedParse :: (Ord nt, Ord t, Ord wt, Monoid wt, Dividable wt) => WPMCFG nt wt t -> [t] -> [Tree (Rule nt t)]
weightedParse (WPMCFG s rs) w = map (\ (Passive (_, _, Derivation t)) -> t) 
                                $ filter (resultfilter s targetrange)
                                $ solve ds
    where
        ds = WeightedDeductiveSolver (conversionRule : terminalCompletionRule w : (rs >>= completionRules)) id
        targetrange = [ if null w then Nothing else Just (0, length w) ]
        
        resultfilter :: (Eq nt) => [nt] -> Rangevector -> Item nt t -> Bool
        resultfilter start target (Passive (a, rho, _)) = a `elem` start && rho == target
        resultfilter _ _ _ = False 

data Item nt t = Passive (nt, Rangevector, Derivation nt t)
                 | Active (Rule nt t, [Range], Function t, Map.Map (VarT t) Range, [(Int, Derivation nt t)]) deriving (Eq)

instance (Ord nt, Ord t) => Ord (Item nt t) where
    (Passive tup) `compare` (Passive tup') = tup `compare` tup'
    (Active (r, rv, f, _, _)) `compare` (Active (r', rv', f', _, _)) = (r, rv, f) `compare` (r', rv', f')
    (Active _) `compare` (Passive _) = GT
    (Passive _) `compare` (Active _) = LT

instance (Show nt, Show t) => Show (Item nt t) where
    show (Passive (a, rv, _)) = "[Passive] " ++ show a ++ " " ++ prettyPrintRangevector rv
    show (Active (r, rv, f, _, _)) = "[Active] " ++ prettyPrintRule r ++ " " ++ prettyPrintRangevector rv ++ " " ++ prettyPrintComposition f

conversionRule :: (Ord nt, Ord t, Monoid wt) => (DeductiveRule (Item nt t), wt)
conversionRule = (DeductiveRule [conversionFilter] convert, mempty)
  where
    conversionFilter :: Item nt t -> Bool
    conversionFilter (Active (_, _, []:_, _, _)) = True
    conversionFilter _ = False
    
    convert :: (Ord nt, Ord t) => [Item nt t] -> [Item nt t]
    convert [Active (r@(Rule ((a, _), _)), rs, []:[], _, ts)] = [Passive (a, reverse rs, node r $ snd $ unzip $ sort ts)]
    convert [Active (r, rs, []:fs, m, ts)] = [Active (r, Nothing:rs, fs, m, ts)]
    convert _ = []
    
terminalCompletionRule :: (Monoid wt, Eq t) => [t] -> (DeductiveRule (Item nt t), wt)
terminalCompletionRule w = (DeductiveRule [completeTFilter] (completeT w), mempty)
  where
    completeTFilter :: Item nt t -> Bool
    completeTFilter (Active (_, _, ((T _:_):_), _, _)) = True
    completeTFilter _ = False
    
    completeT :: (Eq t) => [t] -> [Item nt t] -> [Item nt t]
    completeT w' [Active (r, ra:rs, (T t:fs):fss, m, ds)] =  [Active (r, ra':rs, fs:fss, m, ds) 
                                                            | ra' <- mapMaybe (conc ra) $ map (\ i -> Just (i, i+1)) $ elemIndices t w'
                                                            ]
    completeT _ _ = []
    
completionRules :: (Eq t, Eq nt, Ord t, Ord nt, Monoid wt, Dividable wt) => (Rule nt t, wt) -> [(DeductiveRule (Item nt t), wt)]
completionRules (r@(Rule ((_, as), f)), w) = (DeductiveRule [] (\ [] -> [ Active (r, [Nothing], f, Map.empty, []) ]), mempty)
                                              : zip [ DeductiveRule [completeNTFilterPassive a', completeNTFilterActive r a'] completeNT
                                                    | a' <- as 
                                                    ] weights
  where
    weights = divide w $ length as
    completeNTFilterPassive :: (Eq nt) => nt -> Item nt t -> Bool
    completeNTFilterPassive a (Passive (a', _, _)) = a == a'
    completeNTFilterPassive _ _ = False
    
    completeNTFilterActive :: (Eq nt, Eq t) => Rule nt t -> nt -> Item nt t -> Bool
    completeNTFilterActive r''@(Rule ((_, as'), _)) a (Active (r', _, (Var i _:_):_, _, _)) 
          = i `elem` elemIndices a as' && r'' == r'
    completeNTFilterActive _ _ _ = False
    
    completeNT :: (Ord nt, Ord t) => [Item nt t] -> [Item nt t]
    completeNT [Passive (_, rv, d), Active (r', ra:ras, (Var i j:fs):fss, m, ds)]
      | Map.fromList (zip [Var i j' | j' <- [0..]] rv) `Map.isSubmapOf` m
        = [ Active (r', ra':ras, fs:fss, m, ds)
          | ra' <- maybeToList $ conc ra (rv !! j) 
          ]
      | not $ any (`Map.member` m) [Var i j' | j' <- [0..(length rv)]]
        = [ Active (r', ra':ras, fs:fss, m `Map.union` Map.fromList (zip [Var i j' | j' <- [0..]] rv), (i,d):ds)
          | ra' <- maybeToList $ conc ra (rv !! j)
          ]
      | otherwise = []
    completeNT _ = []
    
    
      
conc :: Range -> Range -> Maybe Range
conc Nothing r = Just r
conc r Nothing = Just r
conc (Just (i,j)) (Just (k,l))
  | j == k = Just $ Just (i,l)
  | otherwise = Nothing