module Vanda.Grammar.LCFRS.CYKParserTest
    (tests) where

import Test.HUnit
import Data.Interner
import Vanda.Grammar.PMCFG
import Vanda.Grammar.XRS.LCFRS.CYKParser
import Data.Weight
import Data.Maybe (mapMaybe)
import Data.Hashable (hash)
import Numeric.Log (Log)
import Control.Arrow (second)

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
                      (WPMCFG s rs) -> WPMCFG s $ map (second probabilistic) rs

exampleWPMCFG'' :: (WPMCFG Int (Probabilistic (Log Double)) Int, Interner Int, Interner Char)
exampleWPMCFG'' = integerize exampleWPMCFG'

tests :: Test
tests = TestList    [ TestCase $ assertEqual "Cannot reproduce exmaple derivation" [exampleDerivation] $ parse exampleWPMCFG' 100 1 "aabccd"
                    , TestCase $ assertEqual "Cannot reproduce parsed string in yield" ["aabbccdd"] $ mapMaybe yield $ parse exampleWPMCFG' 100 1 "aabbccdd"
                    , TestCase $ assertEqual "Cannot reproduce weighted example derivation (integerized)" [exampleDerivation] 
                        $ map (deintegerize (nti, ti)) 
                        $ parse ig 100 1
                        $ snd 
                        $ internListPreserveOrder ti "aabccd"
                    , TestCase $ assertEqual "Cannot reproduce parsed string in yield (weighted)" ["aabbccdd"] 
                                $ mapMaybe yield 
                                $ parse exampleWPMCFG' 100 1 "aabbccdd"
                    , TestCase $ assertEqual "instatntiate Fails (2)" 16 $ length $ instantiate testword testcomposition
                    , TestCase $ assertBool "Derivation Equality" $ testderivation == testderivation
                    , TestCase $ assertBool "Derivation Equality (2)" $ not $ testderivation == testderivation2
                    , TestCase $ assertBool "Derivation Order" $ testderivation > testderivation2
                    , TestCase $ assertBool "Derivation hash" $ not $ hash testderivation == hash testderivation2
                    ]
                        where (ig, nti, ti) = exampleWPMCFG''