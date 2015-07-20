module Vanda.CBSM.DovetailingTests where
import Vanda.CBSM.Dovetailing


import Test.HUnit

import Data.List (permutations)


tests :: Test
tests = TestList
  [ "dovetail" ~: TestList
      [ TestList
        [ dovetail step ys ~?= [1 .. n]
        | ys <- permutations (zip [1 .. n] [0, n ..])
        ]
      | n <- [0 .. 5]
      ]
  ]


step :: (Int, Int) -> Either (Int, Int) Int
step (m, 0) = Right m
step (m, n) = Left (m, n - 1)
