-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Technische Universität Dresden 2018
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Data.List.ShuffleTests where
import Data.List.Shuffle


import Data.List
import System.Random
import Test.HUnit


tests :: Test
tests = TestList
  [ "shuffle" ~: TestList
    [       fst (shuffle []               g0)  ~?= ([] :: [()])
    ,       fst (shuffle [()]             g0)  ~?= [()]
    , sort (fst (shuffle [0 .. 99 :: Int] g0)) ~?= [0 .. 99 :: Int]
    ]
  ]
  where
    g0 = mkStdGen 0
