module Vanda.Grammar.PMCFG.ActiveParser
    ( parse
    , weightedParse
    ) where

import Vanda.Grammar.PMCFG (PMCFG(..), WPMCFG(..), Rule(..), VatT(..), prettyPrintRule, prettyPrintComposition)
import Vanda.Grammar.PMCFG.CYKParser (Derivation(..), Range, Rangevector, Function, prettyPrintRangevector)
import Vanda.Grammar.PMCFG.DeductiveSolcer (DeductiveRule(..))
import Vanda.Grammar.PMCFG.WeightedDeductiveSolver (WeightedDeductiveSolver(..), solve)
import qualified Data.Map.Strict as Map

parse :: PMCFG nt t -> [t] -> [Tree (Rule nt t)]
parse = undefined

weightedParse :: WPMCFG nt wt t -> [t] -> [Tree (Rule nt t)]
weightedParse = undefined

data Item nt t = Passive (nt, Rangevector, Derivation nt t)
                 | Active (Rule nt t, [Range], Function, Map.Map VarT Range)

instance (Ord nt, Ord t) => Ord (Item nt t) where
    (Passive tup) `compare` (Passive tup') = tup `compare` tup'
    (Active (r, rv, f, _)) `compare` (Active (r', rv', f', _)) = (r, rv, f) `compare` `r, rv, f`
    (Active _) `compare` (Passive _) = GT
    (Passive _) `compare` (Active _) == LT

instance (Show nt, Show t) => Show (Item nt t) where
    show (Passive (a, rv, _)) = "[Passive] " ++ show a ++ " " ++ prettyPrintRangevector rv
    show (Active (r, rv, f, _)) = "[Active] " ++ prettyPrintRule r ++ " " ++ prettyPrintRangevector rv ++ " " ++ prettyPrintComposition f

makeRules :: (Rule nt t, wt) -> [(DeductiveRule (Item nt t), wt)]
