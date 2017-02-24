module Vanda.Grammar.PMCFG.NaiveParserTest
    (tests) where

import Test.HUnit
import Vanda.Grammar.PMCFG
import Vanda.Grammar.PMCFG.NaiveParser
import Vanda.Grammar.PMCFG.DeductiveSolver
import Vanda.Grammar.PMCFG.Weights
import Data.Maybe (mapMaybe)
import Numeric.Log (Log)

exampleWPMCFG' :: WPMCFG Int (Probabilistic (Log Double)) Char
exampleWPMCFG' = case exampleWPMCFG of
                      (WPMCFG s rs) -> WPMCFG s $ map (\ (r, w) -> (r, probabilistic w)) rs
                      
tests :: Test
tests = TestList    [ TestCase $ assertEqual "Cannot reproduce exmaple derivation" [exampleDerivation] $ parse examplePMCFG 100 "aabccd"
                    , TestCase $ assertEqual "Cannot reproduce parsed string in yield" ["aabbccdd"] $ mapMaybe yield $ parse examplePMCFG 100 "aabbccdd"
                    , TestCase $ assertEqual "Cannot reproduce weighted example derivation" [exampleDerivation] $ weightedParse exampleWPMCFG' 100 "aabccd"
                    ]