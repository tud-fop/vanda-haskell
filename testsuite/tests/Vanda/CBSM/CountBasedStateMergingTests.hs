module Vanda.CBSM.CountBasedStateMergingTests where
import Vanda.CBSM.CountBasedStateMerging

import qualified Data.RevMap as RM
import           Vanda.Corpus.TreeTerm

import           TestUtil (assertRoughly)

import           Data.List (sortBy)
import qualified Data.Map as M
import           Data.Ord (comparing)
import           Data.Tree
import           Numeric.Log (Log(..))
import           Test.HUnit


tests :: Test
tests = TestList
  [ "sortedCartesianProductWith" ~: TestList
    [ testSortedCartesianProductWith (+) [] ([] :: [Int])
    , testSortedCartesianProductWith (+) [] [0 :: Int]
    , testSortedCartesianProductWith (+) [0 :: Int] []
    , testSortedCartesianProductWith (+) [0] [0 :: Int]
    , testSortedCartesianProductWith (+) [0 .. 2] [0 .. 2 :: Int]
    , testSortedCartesianProductWith (+) [0, 2, 4] [0 .. 2 :: Int]
    , testSortedCartesianProductWith (+) [0, 2, 4] [0, 5, 10 :: Int]
    ]
  , "sortedCartesianProductWith'" ~: TestList
    [ testSortedCartesianProductWith' (+) [] ([] :: [Int])
    , testSortedCartesianProductWith' (+) [] [0 :: Int]
    , testSortedCartesianProductWith' (+) [0 :: Int] []
    , testSortedCartesianProductWith' (+) [0] [0 :: Int]
    , testSortedCartesianProductWith' (+) [0 .. 2] [0 .. 2 :: Int]
    , testSortedCartesianProductWith' (+) [0, 2, 4] [0 .. 2 :: Int]
    , testSortedCartesianProductWith' (+) [0, 2, 4] [0, 5, 10 :: Int]
    ]
  , "ruleEquivalenceClasses" ~: TestList
    [ ruleEquivalenceClasses RM.empty rtg0 ~?= M.empty
    , ruleEquivalenceClasses (createMerge ["Aa"]) rtg0
      ~?= M.fromList
        [ (Rule 'A' "BC" 's' 1, [Rule 'A' "BC" 's' 1])
        , (Rule 'A' "bc" 's' 4, [Rule 'a' "bc" 's' 4])
        , (Rule 'B' "AC" 's' 2, [Rule 'B' "AC" 's' 2])
        , (Rule 'b' "Ac" 's' 5, [Rule 'b' "ac" 's' 5])
        ]
    , ruleEquivalenceClasses (createMerge ["Cc"]) rtg0
      ~?= M.fromList
        [ (Rule 'A' "BC" 's' 1, [Rule 'A' "BC" 's' 1])
        , (Rule 'B' "AC" 's' 2, [Rule 'B' "AC" 's' 2])
        , (Rule 'C' ""   'C' 3, [Rule 'C' ""   'C' 3])
        , (Rule 'C' ""   'c' 6, [Rule 'c' ""   'c' 6])
        , (Rule 'a' "bC" 's' 4, [Rule 'a' "bc" 's' 4])
        , (Rule 'b' "aC" 's' 5, [Rule 'b' "ac" 's' 5])
        ]
    , ruleEquivalenceClasses (createMerge ["Aa", "Bb"]) rtg0
      ~?= M.fromList
        [ (Rule 'A' "BC" 's' 1, [Rule 'A' "BC" 's' 1])
        , (Rule 'A' "Bc" 's' 4, [Rule 'a' "bc" 's' 4])
        , (Rule 'B' "AC" 's' 2, [Rule 'B' "AC" 's' 2])
        , (Rule 'B' "Ac" 's' 5, [Rule 'b' "ac" 's' 5])
        ]
    , ruleEquivalenceClasses (createMerge ["Aa", "Bb", "Cc"]) rtg0
      ~?= M.fromList
        [ (Rule 'A' "BC" 's' undefined, [Rule 'a' "bc" 's' 4, Rule 'A' "BC" 's' 1])
        , (Rule 'B' "AC" 's' undefined, [Rule 'b' "ac" 's' 5, Rule 'B' "AC" 's' 2])
        , (Rule 'C' ""   'C' 3, [Rule 'C' ""   'C' 3])
        , (Rule 'C' ""   'c' 6, [Rule 'c' ""   'c' 6])
        ]
    ]
  , "likelihoodDelta" ~: TestList
    [ ln (likelihoodDelta RM.empty crtg0) ~?= 0
    , ln (likelihoodDelta (createMerge ["Cc"]) crtg0) ~?= log ((3**3 * 6**6) / 9**9)
    , TestCase $ assertRoughly "" 1e-10
        ( ln (likelihoodDelta (createMerge ["Aa", "Bb", "Cc"]) crtg0) )
        ( log
          ( 3**3 / (1**1 * 2**2)  -- } merged initial states
          * 5**5 / (1**1 * 4**4)  -- ⎫ merged
          * 7**7 / (2**2 * 5**5)  -- ⎭ rules
          * (1**1 * 4**4) / 5**5  -- ⎫
          * (2**2 * 5**5) / 7**7  -- ⎬ merged states
          * (3**3 * 6**6) / 9**9  -- ⎭
          )
        )
    ]
  ]


testSortedCartesianProductWith
  :: (Show c, Ord c) => (a -> b -> c) -> [a] -> [b] -> Test
testSortedCartesianProductWith (>+<) xs ys
  =   map fst (naiveSortedCartesianProductWithInternal true2 (>+<) xs ys)
  ~=? map fst (     sortedCartesianProductWith               (>+<) xs ys)
  where true2 _ _= True


testSortedCartesianProductWith'
  :: (Show c, Ord c) => (a -> b -> c) -> [a] -> [b] -> Test
testSortedCartesianProductWith' (>+<) xs ys
  =   map fst (naiveSortedCartesianProductWithInternal  (<=) (>+<) xs ys)
  ~=? map fst (     sortedCartesianProductWith'              (>+<) xs ys)


naiveSortedCartesianProductWithInternal
  :: Ord c => (Int -> Int -> Bool) -> (a -> b -> c) -> [a] -> [b] -> [(c, (a, b))]
naiveSortedCartesianProductWithInternal (?) (>+<) xs ys
  = sortBy (comparing fst)
      [ (x >+< y, (x, y))
      | (x, i) <- zip xs [0 :: Int ..]
      , (y, j) <- zip ys [0 :: Int ..]
      , i ? j
      ]


rtg0 :: RTG Char Char
rtg0 = fromList
  [ Rule 'A' "BC" 's' 1
  , Rule 'B' "AC" 's' 2
  , Rule 'C' ""   'C' 3
  , Rule 'a' "bc" 's' 4
  , Rule 'b' "ac" 's' 5
  , Rule 'c' ""   'c' 6
  ]


crtg0 :: CRTG Char Char
crtg0 = CRTG
  rtg0
  (M.fromList [('A', 1), ('B', 2), ('C', 3), ('a', 4), ('b', 5), ('c', 6)])
  (M.fromList [('A', 1), ('a', 2)])


t0 = parseTree "A(B(D, E), C(D, E))"
t1 = parseTree "X(B(Y, E), C(Z, E))"

