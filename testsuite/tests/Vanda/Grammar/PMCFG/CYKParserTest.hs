module Vanda.Grammar.PMCFG.CYKParserTest
    (tests) where

import Test.HUnit
import Vanda.Grammar.PMCFG
import Vanda.Grammar.PMCFG.CYKParser
import Data.Maybe (mapMaybe)
import Data.Hashable (hash)

testcomposition :: Function Char
testcomposition = [ [T 'a', Var 1 1, T 'a']
                  , []
                  ]

testrule :: Rule String Char
testrule = Rule (("A", ["A"]), testcomposition)

testword :: String
testword = "aaaa"

testinstance :: InstantiatedFunction
testinstance =  [ [T $ Just (0,1), Var 1 1, T $ Just (3,4)]
                , [T Nothing]
                ]
                
testderivation = node testrule [node testrule []]
testderivation2 = node testrule []

tests :: Test
tests = TestList    [ TestCase $ assertEqual "Cannot reproduce exmaple derivation" [exampleDerivation] $ parse examplePMCFG "aabccd"
                    , TestCase $ assertEqual "Cannot reproduce parsed string in yield" ["aabbccdd"] $ mapMaybe yield $ parse examplePMCFG "aabbccdd"
                    , TestCase $ assertEqual "Cannot reproduce weighted example derivation" [exampleDerivation] $ weightedParse exampleWPMCFG "aabccd"
                    , TestCase $ assertEqual "Cannot reproduce parsed string in yield (weighted)" ["aabbccdd"] $ mapMaybe yield $ weightedParse exampleWPMCFG "aabbccdd"
                    , TestCase $ assertBool "instantiate Fails" $ testinstance `elem` instantiate testword testcomposition
                    , TestCase $ assertEqual "instatntiate Fails (2)" 16 $ length $ instantiate testword testcomposition
                    , TestCase $ assertBool "Derivation Equality" $ testderivation == testderivation
                    , TestCase $ assertBool "Derivation Equality (2)" $ not $ testderivation == testderivation2
                    , TestCase $ assertBool "Derivation Order" $ testderivation > testderivation2
                    , TestCase $ assertBool "Derivation hash" $ not $ hash testderivation == hash testderivation2
                    , TestCase $ assertBool "overlapping test" $ isNonOverlapping [Just (0,0), Just (100, 200), Nothing, Just (50, 99), Nothing]
                    , TestCase $ assertBool "overlapping test (2)" $ not $ isNonOverlapping [Nothing, Nothing, Just (300, 400), Just (150, 301)]
                    ]