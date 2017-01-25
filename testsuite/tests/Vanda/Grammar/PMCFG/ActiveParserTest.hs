module Vanda.Grammar.PMCFG.ActiveParserTest
    (tests) where

import Test.HUnit
import Vanda.Grammar.PMCFG
import Vanda.Grammar.PMCFG.ActiveParser
import Vanda.Grammar.PMCFG.WeightedDeductiveSolver (Probabilistic(..))
import Data.Maybe (mapMaybe)

exampleWPMCFG' :: WPMCFG Int (Probabilistic Double) Char
exampleWPMCFG' = case exampleWPMCFG of
                      (WPMCFG s rs) -> WPMCFG s $ map (\ (r, w) -> (r, Probabilistic w)) rs
                      
tests :: Test
tests = TestList    [ TestCase $ assertEqual "Cannot reproduce exmaple derivation" [exampleDerivation] $ parse examplePMCFG "aabccd"
                    , TestCase $ assertEqual "Cannot reproduce parsed string in yield" ["aabbccdd"] $ mapMaybe yield $ parse examplePMCFG "aabbccdd"
                    , TestCase $ assertEqual "Cannot reproduce weighted example derivation" [exampleDerivation] $ weightedParse exampleWPMCFG' "aabccd"
                    ]