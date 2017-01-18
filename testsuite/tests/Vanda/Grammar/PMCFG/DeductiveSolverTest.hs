module Vanda.Grammar.PMCFG.DeductiveSolverTest
    (tests) where

import Test.HUnit
import Vanda.Grammar.PMCFG.DeductiveSolver

exampleRules :: [DeductiveRule ([Int], Int)]
exampleRules =  DeductiveRule [] (const $ Just ([], 1)) : [ DeductiveRule [(< 50) . snd] (\ [(xs, prod)] -> Just (i:xs, i*prod)) | i <- [2,3,5,7,11,13] ]

exampleSolver :: DeductiveSolver ([Int], Int)
exampleSolver = DeductiveSolver exampleRules id

tests :: Test
tests = TestLabel "DeductiveSolver prime test" $ TestCase $ assertBool "does not contain 7x7=49" (([7,7], 49) `elem` solve exampleSolver)