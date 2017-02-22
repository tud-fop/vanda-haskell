-- module Vanda.Grammar.PMCFG.WeightedDeductiveSolverTest
--     (tests) where

-- import Test.HUnit
-- import Vanda.Grammar.PMCFG.DeductiveSolver

-- exampleRules :: [DeductiveRule ([Int], Int)]
-- exampleRules =  DeductiveRule [] (const $ return ([], 1)) : [ DeductiveRule [(< 50) . snd] (\ [(xs, prod)] -> [(i:xs, i*prod)]) | i <- [2,3,5,7,11,13] ]

-- exampleSolver :: WeightedDeductiveSolver ([Int], Int) (Cost Int)
-- exampleSolver = WeightedDeductiveSolver (zip exampleRules $ repeat $ cost 1) 100

-- tests :: Test
-- tests = TestLabel "DeductiveSolver prime test" $ TestCase $ assertBool "does not contain 7x7=49" (([7,7], 49) `elem` solve exampleSolver)