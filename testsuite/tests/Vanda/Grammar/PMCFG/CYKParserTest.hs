module Vanda.Grammar.PMCFG.CYKParserTest
    (tests) where

import Test.HUnit
import Data.Interner
import Vanda.Grammar.PMCFG
import Vanda.Grammar.PMCFG.CYKParser
import Vanda.Grammar.PMCFG.DeductiveSolver
import Data.Maybe (mapMaybe)
import Data.Hashable (hash)
import Numeric.Log (Log)

testcomposition :: Function Char
testcomposition = [ [T 'a', Var 1 1, T 'a']
                  , []
                  ]

testrule :: Rule String Char
testrule = Rule (("A", ["A"]), testcomposition)

testword :: String
testword = "aaaa"
 {-
testinstance :: InstantiatedFunction
testinstance =  [ [T $ Just (0,1), Var 1 1, T $ Just (3,4)]
                , [T Nothing]
                ]
-}                
testderivation :: Derivation String Char
testderivation = node testrule [node testrule []]
testderivation2 :: Derivation String Char
testderivation2 = node testrule []

exampleWPMCFG' :: WPMCFG Int (Probabilistic (Log Double)) Char
exampleWPMCFG' = case exampleWPMCFG of
                      (WPMCFG s rs) -> WPMCFG s $ map (\ (r, w) -> (r, probabilistic w)) rs

exampleWPMCFG'' :: (WPMCFG Int (Probabilistic (Log Double)) Int, Interner Int, Interner Char)
exampleWPMCFG'' = integerize exampleWPMCFG'

tests :: Test
tests = TestList    [ TestCase $ assertEqual "Cannot reproduce exmaple derivation" [exampleDerivation] $ parse examplePMCFG 100 "aabccd"
                    , TestCase $ assertEqual "Cannot reproduce parsed string in yield" ["aabbccdd"] $ mapMaybe yield $ parse examplePMCFG 100 "aabbccdd"
                    , TestCase $ assertEqual "Cannot reproduce weighted example derivation" [exampleDerivation] $ weightedParse exampleWPMCFG' 100 "aabccd"
                    , TestCase $ assertEqual "Cannot reproduce weighted example derivation (integerized)" [exampleDerivation] 
                        $ map (deintegerize (nti, ti)) 
                        $ weightedParse ig 100
                        $ snd 
                        $ internListPreserveOrder ti "aabccd"
                    , TestCase $ assertEqual "Cannot reproduce parsed string in yield (weighted)" ["aabbccdd"] 
                                $ mapMaybe yield 
                                $ weightedParse exampleWPMCFG' 100 "aabbccdd"
                    --, TestCase $ assertBool "instantiate Fails" $ testinstance `elem` instantiate testword testcomposition
                    , TestCase $ assertEqual "instatntiate Fails (2)" 16 $ length $ instantiate testword testcomposition
                    , TestCase $ assertBool "Derivation Equality" $ testderivation == testderivation
                    , TestCase $ assertBool "Derivation Equality (2)" $ not $ testderivation == testderivation2
                    , TestCase $ assertBool "Derivation Order" $ testderivation > testderivation2
                    , TestCase $ assertBool "Derivation hash" $ not $ hash testderivation == hash testderivation2
                    --, TestCase $ assertBool "overlapping test" $ isNonOverlapping [Just (0,0), Just (100, 200), Nothing, Just (50, 99), Nothing]
                    --, TestCase $ assertBool "overlapping test (2)" $ not $ isNonOverlapping [Nothing, Nothing, Just (300, 400), Just (150, 301)]
                    ]
                        where (ig, nti, ti) = exampleWPMCFG''