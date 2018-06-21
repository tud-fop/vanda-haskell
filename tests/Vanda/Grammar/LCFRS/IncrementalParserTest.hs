-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Technische Universität Dresden 2018
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Grammar.LCFRS.IncrementalParserTest
    (tests) where

import Test.HUnit
import Vanda.Grammar.XRS.LCFRS.IncrementalParser
import Vanda.Grammar.PMCFG 
import Data.Range
import Data.Maybe (mapMaybe)
import Numeric.Log (Log)
import qualified Data.IntMap                   as IMap
import Data.Weight
import Data.Maybe (mapMaybe)
import Numeric.Log (Log)
import Control.Arrow (second)

exampleWPMCFG' :: WPMCFG Int (Probabilistic (Log Double)) Char
exampleWPMCFG' = case exampleWPMCFG of
                      (WPMCFG s rs) -> WPMCFG s $ map (second probabilistic) rs

exampleRules' :: [Rule Int Char]
exampleRules' = [ Rule ((0, []), [[T 'a']]),
                    Rule ((0, []), [[T 'a', T 'b']]),
                  Rule ((0, [1,2]), [[Var 0 0, Var 1 0]]),
                  Rule ((0, [1]), [[Var 0 0]]),
                  Rule ((0, [7]), [[Var 0 0, Var 0 1]]),
                  Rule ((0, [8]), [[Var 0 0, Var 0 1]]),
                  Rule ((0, [1]), [[T 'D', Var 0 0, T 'E' ]]),
                  Rule ((0, [1,3]), [[Var 0 0, Var 1 0, Var 1 1]]),
                  Rule ((0, [4]), [[T 'x', Var 0 0, T 'y']]),
                  Rule ((1, []), [[T 'A']]),
                  Rule ((2, [3]), [[Var 0 1]]),
                  Rule ((3, []), [[T 'B'], [T 'C']]),
                  Rule ((4, [5, 6]), [[T 'q', Var 0 0, Var 0 1, T 'z', Var 1 0]]),
                  Rule ((4, [5]), [[T 'q', Var 0 0, Var 0 1, T 'z']]),
                  Rule ((4, []), [[T 'r']]),
                  Rule ((5, []), [[],[]]),
                  Rule ((5, [4]), [[],[Var 0 0]]),
                  Rule ((6, []), [[T 't']]),

                  Rule ((7, []), [[T 'm'], [T 'm']]),
                  Rule ((7, []), [[T 'n'], [T 'n']]),
                  Rule ((8, [9]), [[Var 0 0], [Var 0 1]]),
                  Rule ((9, []), [[T 'g'], [T 'g']]),
                  Rule ((9, []), [[T 'h'], [T 'h']])
                ]

exampleWPMCFG'' :: WPMCFG Int Double Char
exampleWPMCFG'' = fromWeightedRules [0] $ zip exampleRules' (cycle [1])

exampleWPMCFG''' :: WPMCFG Int (Probabilistic (Log Double)) Char
exampleWPMCFG''' = case exampleWPMCFG'' of
                      (WPMCFG s rs) -> WPMCFG s $ map (second probabilistic) rs
first :: (x,y,z) -> x
first (x,_,_) = x

tests :: Test
tests = TestList    [
                        TestCase $ assertEqual "Can't find item after Initial Prediction" ["a"] $ mapMaybe yield $ parse exampleWPMCFG''' 100 1 "a"
                        ,TestCase $ assertEqual "Can't find item after Initial Prediction, Prediction and Combine" ["A"] $ mapMaybe yield $ parse exampleWPMCFG''' 100 1 "A"
                        ,TestCase $ assertEqual "Can't find item after Initial Prediction, Prediction and Combine" ["ABC"] $ mapMaybe yield $ parse exampleWPMCFG''' 100 1 "ABC"
                        , TestCase $ assertEqual "Can't find item if variable after Terminal" ["DAE"] $ mapMaybe yield $ parse exampleWPMCFG''' 100 1 "DAE"
                        ,TestCase $ assertEqual "Longer Parsing doesn't work" ["aabccd"] $ mapMaybe yield $ parse exampleWPMCFG' 100 1 "aabccd"
                        ,TestCase $ assertEqual "Longer Parsing doesn't work" ["xqzy"] $ mapMaybe yield $ parse exampleWPMCFG''' 100 1 "xqzy"
                        ,TestCase $ assertEqual "Longer Parsing doesn't work" ["xqqrztzty"] $ mapMaybe yield $ parse exampleWPMCFG''' 100 1 "xqqrztzty"
                        ,TestCase $ assertEqual "Compatibility Combine doesn't work mn" [] $ mapMaybe yield $ parse exampleWPMCFG''' 100 1 "mn"
                        ,TestCase $ assertEqual "Compatibility Check Complete fails" [] $ mapMaybe yield $ parse exampleWPMCFG''' 100 1 "gh"

                    ,TestCase $ assertEqual
                      "Cannot reproduce example derivation" 
                      [exampleDerivation] 
                    $ parse 
                        exampleWPMCFG' 100 1 "aabccd"
                  , TestCase 
                    $ assertEqual 
                      "Cannot reproduce parsed string in yield"
                      ["aabbccdd"] 
                    $ mapMaybe yield 
                      $ parse exampleWPMCFG' 100 1 "aabbccdd"
                     ]
