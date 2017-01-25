module Vanda.Grammar.PMCFG.NaiveParser
    ( weightedParse
    , parse
    ) where

import Vanda.Grammar.PMCFG.WeightedDeductiveSolver (solve, WeightedDeductiveSolver(..), DeductiveRule(..), Cost(..), Dividable(divide))
import Vanda.Grammar.PMCFG.CYKParser (InstantiatedFunction, Range, Rangevector, concVarRange, toRange, isNonOverlapping, instantiate, prettyPrintInstantiatedFunction, prettyPrintRangevector, Derivation(Derivation), node)
import Vanda.Grammar.PMCFG (Rule(Rule), PMCFG(PMCFG), WPMCFG(WPMCFG), VarT(Var, T), prettyPrintRule)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Tree (Tree)
import Data.Maybe (maybeToList)

-- | Two types of deductive items:
-- * active items need to be completed by substituting variables with ranges
-- * passive items are completely instantiated
data Item nt t = ActiveItem (Rule nt t, [nt], Int, [Derivation nt t], InstantiatedFunction)
                    | PassiveItem (nt, Rangevector, Derivation nt t) deriving (Eq, Ord)

instance (Hashable nt, Hashable t) => Hashable (Item nt t) where
    salt `hashWithSalt` (ActiveItem tup) = salt `hashWithSalt` tup
    salt `hashWithSalt` (PassiveItem tup) = salt `hashWithSalt` tup

instance (Show nt, Show t) => Show (Item nt t) where
    show (ActiveItem (r, as, i, _, fs)) = "[active] " ++ prettyPrintRule r ++ " " ++ show as ++ "+" ++ show i ++ " " ++ prettyPrintInstantiatedFunction fs
    show (PassiveItem (a, rv, _)) = "[passive] " ++ show a ++ " " ++ prettyPrintRangevector rv 

-- | Top-level function to parse a word using a PMCFG.
parse :: (Ord t, Ord nt) 
      => PMCFG nt t         -- ^ unweighted grammar
      -> [t]                -- ^ terminal word
      -> [Tree (Rule nt t)] -- ^ derivation tree of applied rules
parse (PMCFG s rs) = weightedParse $ WPMCFG s $ zip rs $ repeat (Cost 1 :: Cost Double)

-- | Top-level function to parse a word using a weighted PMCFG.
weightedParse :: (Ord t, Ord nt, Ord wt, Monoid wt, Dividable wt) 
              => WPMCFG nt wt t     -- ^ weighted grammar
              -> [t]                -- ^ terminal word
              -> [Tree (Rule nt t)] -- ^ derivation tree of applied rules
weightedParse (WPMCFG s rs) w = map (\ (PassiveItem (_, _, Derivation t)) -> t) 
                        $ filter (resultfilter s targetrange)
                        $ solve ds
    where
        ds = WeightedDeductiveSolver (rs >>= makeRule w) id
        targetrange = [ if null w then Nothing else Just (0, length w) ]
        
        resultfilter :: (Eq nt) => [nt] -> Rangevector -> Item nt t -> Bool
        resultfilter start target (PassiveItem (a, rho, _)) = a `elem` start && rho == target
        resultfilter _ _ _ = False 
        
-- | Constructs deductive rules using one rule of a grammar.
-- Per grammar rule, there are 3 types of deductive rules:
-- * prediction: initializes an active item without using antecendent items
-- * completion: step-by-step substituting of variables in instantiated function using ranges of passive items
-- * conversion: converts an active item into a passive one, if there are no variables left
makeRule :: (Eq nt, Eq t, Monoid wt, Dividable wt) => [t] -> (Rule nt t, wt) -> [(DeductiveRule (Item nt t), wt)]
makeRule w (r@(Rule ((_, as), f)), weight) = (DeductiveRule [filterConversion r] convert, mempty)
                                              : (DeductiveRule [] (\ [] -> [ ActiveItem (r, as, 0, [], inst) | inst <- instantiate w f ]), mempty)
                                              : zip [ DeductiveRule [filterCompletePassive a', filterCompleteActive r] complete | a' <- as ] singleNTWeight
    where
        singleNTWeight = divide weight $ length as
        filterCompletePassive :: (Eq nt) => nt -> Item nt t -> Bool
        filterCompletePassive a (PassiveItem (a', _, _)) = a == a'
        filterCompletePassive _ _ = False

        filterCompleteActive :: (Eq nt, Eq t) => Rule nt t -> Item nt t -> Bool
        filterCompleteActive r' (ActiveItem (r'', _, _, _, _)) = r'' == r'
        filterCompleteActive _ _ = False

        complete :: [Item nt t] -> [Item nt t]
        complete [ PassiveItem (_, rv, t)
                    , ActiveItem ( r'@(Rule ((_, _), _))
                                , _:as', offset, ts, fs) ] =  [ ActiveItem (r', as', offset+1, t:ts, fs')
                                                              | fs' <- maybeToList $ mapM concVarRange $ insert offset rv fs
                                                              ]
        complete _ = []

        filterConversion :: (Eq t, Eq nt) => Rule nt t -> Item nt t -> Bool
        filterConversion r'' (ActiveItem (r', [], _, _, _)) = r'' == r'
        filterConversion _ _ = False

        convert :: [Item nt t] -> [Item nt t]
        convert [ActiveItem (r'@(Rule ((a, _),_))
                , [], _, ts, fs)]               = [ PassiveItem (a, rv', node r' $ reverse ts)
                                                  | rv' <- maybeToList $ mapM ((>>= toRange) . concVarRange) fs
                                                  , isNonOverlapping rv'
                                                  ]
        convert _ = []

-- | substitutes variables with index 'off' with a ranges of a vector
insert :: Int -> Rangevector -> InstantiatedFunction -> InstantiatedFunction
insert off rv = map (map (substitute off rv))
    where
        substitute :: Int -> Rangevector -> VarT Range -> VarT Range
        substitute i rv' (Var i' j)
            | i' == i =  T $ rv' !! j
            | otherwise = Var i' j
        substitute _ _ r = r