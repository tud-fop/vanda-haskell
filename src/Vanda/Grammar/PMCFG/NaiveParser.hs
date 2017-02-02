-----------------------------------------------------------------------------
-- |
-- Module      :  CYKParser
-- Copyright   :  (c) Thomas Ruprecht 2017
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  thomas.ruprecht@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- This module provides two functions for parsing words using the 
-- naive (active) parsing algorithm by Burden and Ljunglöf.
-- @weightedParse@ uses a weighted PMCFG to find a list of all possible
-- derivation trees ordered by minimal cost / maximum probability. The
-- rules' weights need to be instances of @Monoid@ and @Dividable@.
-- @parse@ uses an unweighted grammar to find a list of derivation trees
-- ordered by least rule applications.
--
-- 
--
-----------------------------------------------------------------------------
module Vanda.Grammar.PMCFG.NaiveParser
    ( weightedParse
    , parse
    ) where

import Vanda.Grammar.PMCFG.WeightedDeductiveSolver (solve, WeightedDeductiveSolver(..), DeductiveRule(..), Cost, cost, Dividable(divide))
import Vanda.Grammar.PMCFG.CYKParser (InstantiatedFunction, Range, entire, Rangevector, concVarRange, toRange, isNonOverlapping, instantiate, prettyPrintInstantiatedFunction, prettyPrintRangevector, Derivation(Derivation), node)
import Vanda.Grammar.PMCFG (Rule(Rule), PMCFG(PMCFG), WPMCFG(WPMCFG), VarT(Var, T), prettyPrintRule)
import Data.Tree (Tree)
import Data.Maybe (maybeToList)

-- | Two types of deductive items:
-- * active items need to be completed by substituting variables with ranges
-- * passive items are completely instantiated
data Item nt t = ActiveItem (Rule nt t, [nt], Int, [Derivation nt t], InstantiatedFunction)
                    | PassiveItem (nt, Rangevector, Derivation nt t) deriving (Eq, Ord)

instance (Show nt, Show t) => Show (Item nt t) where
    show (ActiveItem (r, as, i, _, fs)) = "[active] " ++ prettyPrintRule r ++ " " ++ show as ++ "+" ++ show i ++ " " ++ prettyPrintInstantiatedFunction fs
    show (PassiveItem (a, rv, _)) = "[passive] " ++ show a ++ " " ++ prettyPrintRangevector rv 

-- | Top-level function to parse a word using a PMCFG.
parse :: (Ord t, Ord nt) 
      => PMCFG nt t         -- ^ unweighted grammar
      -> [t]                -- ^ terminal word
      -> [Tree (Rule nt t)] -- ^ derivation tree of applied rules
parse (PMCFG s rs) = weightedParse $ WPMCFG s $ zip rs $ repeat (cost 1 :: Cost Double)

-- | Top-level function to parse a word using a weighted PMCFG.
weightedParse :: (Ord t, Ord nt, Ord wt, Dividable wt) 
              => WPMCFG nt wt t     -- ^ weighted grammar
              -> [t]                -- ^ terminal word
              -> [Tree (Rule nt t)] -- ^ derivation tree of applied rules
weightedParse (WPMCFG s rs) w = map (\ (PassiveItem (_, _, Derivation t)) -> t) 
                        $ filter (resultfilter s [entire w])
                        $ solve ds
    where
        ds = WeightedDeductiveSolver (conversionRule : (rs >>= \ r -> predictionRule w r : completionRules r)) 100
        
        resultfilter :: (Eq nt) => [nt] -> Rangevector -> Item nt t -> Bool
        resultfilter start target (PassiveItem (a, rho, _)) = a `elem` start && rho == target
        resultfilter _ _ _ = False 

-- | Constructs deductive rules using one rule of a grammar.
-- * prediction: initializes an active item without using antecendent items
predictionRule :: (Eq t, Monoid wt) => [t] -> (Rule nt t, wt) -> (DeductiveRule (Item nt t), wt)
predictionRule w (r@(Rule ((_, as), f)), _) = (DeductiveRule [] predict, mempty)
  where
    predict _ = [ ActiveItem (r, as, 0, [], inst) 
                | inst <- instantiate w f 
                ]
                
-- | Constructs deductive rules using one rule of a grammar.
-- * conversion: converts an active item into a passive one, if there are no variables left
conversionRule :: (Monoid wt) => (DeductiveRule (Item nt t), wt)
conversionRule = (DeductiveRule [filterConversion] convert, mempty)
  where
    filterConversion :: Item nt t -> Bool
    filterConversion (ActiveItem (_, [], _, _, _)) = True
    filterConversion _ = False

    convert :: [Item nt t] -> [Item nt t]
    convert [ActiveItem (r'@(Rule ((a, _),_))
            , [], _, ts, fs)]               = [ PassiveItem (a, rv', node r' $ reverse ts)
                                              | rv' <- maybeToList $ mapM ((>>= toRange) . concVarRange) fs
                                              , isNonOverlapping rv'
                                              ]
    convert _ = []

-- | Constructs deductive rules using one rule of a grammar.
-- * completion: step-by-step substituting of variables in instantiated function using ranges of passive items
completionRules :: (Eq nt, Eq t, Dividable wt) => (Rule nt t, wt) -> [(DeductiveRule (Item nt t), wt)]
completionRules (r@(Rule ((_, as), _)), weight) = zip [ DeductiveRule [filterCompletePassive a', filterCompleteActive r a'] complete 
                                                      | a' <- as 
                                                      ] singleNTWeight
    where
        singleNTWeight = divide weight $ length as
        filterCompletePassive :: (Eq nt) => nt -> Item nt t -> Bool
        filterCompletePassive a (PassiveItem (a', _, _)) = a == a'
        filterCompletePassive _ _ = False

        filterCompleteActive :: (Eq nt, Eq t) => Rule nt t -> nt -> Item nt t -> Bool
        filterCompleteActive r' a (ActiveItem (r'', a':_, _, _, _)) = r'' == r' && a == a'
        filterCompleteActive _ _ _ = False

        complete :: [Item nt t] -> [Item nt t]
        complete [ PassiveItem (_, rv, t)
                 , ActiveItem (r', _:as', offset, ts, fs) ] =   [ ActiveItem (r', as', offset+1, t:ts, fs')
                                                                | fs' <- maybeToList $ mapM concVarRange $ insert offset rv fs
                                                                ]
        complete _ = []

        

-- | substitutes variables with index 'off' with a ranges of a vector
insert :: Int -> Rangevector -> InstantiatedFunction -> InstantiatedFunction
insert off rv = map (map (substitute off rv))
    where
        substitute :: Int -> Rangevector -> VarT Range -> VarT Range
        substitute i rv' (Var i' j)
            | i' == i =  T $ rv' !! j
            | otherwise = Var i' j
        substitute _ _ r = r