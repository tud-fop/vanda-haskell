module Vanda.Grammar.PMCFG.CYKParserTest
    (tests) where

import Test.HUnit
import Vanda.Grammar.PMCFG
import Vanda.Grammar.PMCFG.CYKParser
import Data.Maybe (mapMaybe)

tests :: Test
tests = TestList    [ TestCase $ assertEqual "Cannot reproduce exmaple derivation" [exampleDerivation] $ parse examplePMCFG "aabccd"
                    , TestCase $ assertEqual "Cannot reproduce parsed string in yield" ["aabbccdd"] $ mapMaybe yield $ parse examplePMCFG "aabbccdd"
                    , TestCase $ assertEqual "Cannot reproduce weighted example derivation" [(exampleDerivation, exampleWeight)] $ weightedParse exampleWPMCFG "aabccd"
                    ]
                        where
                            exampleWeight = product [ exampleWeights !! 0
                                                    , exampleWeights !! 1
                                                    , exampleWeights !! 1
                                                    , exampleWeights !! 2
                                                    , exampleWeights !! 3
                                                    , exampleWeights !! 4 
                                                    ]