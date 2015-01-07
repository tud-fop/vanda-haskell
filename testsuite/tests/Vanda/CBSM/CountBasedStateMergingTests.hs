module Vanda.CBSM.CountBasedStateMergingTests where
import Vanda.CBSM.CountBasedStateMerging

import qualified Vanda.CBSM.Merge as Merge
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
    [ ruleEquivalenceClasses (bidiStar rtg0) Merge.empty ~?= M.empty
    , ruleEquivalenceClasses (bidiStar rtg0) (Merge.fromLists ["Aa"])
      ~?= M.empty
        {-
        [ (Rule 'A' "BC" 's', [Rule 'A' "BC" 's'])
        , (Rule 'A' "bc" 's', [Rule 'a' "bc" 's'])
        , (Rule 'B' "AC" 's', [Rule 'B' "AC" 's'])
        , (Rule 'b' "Ac" 's', [Rule 'b' "ac" 's'])
        ]
        -}
    , ruleEquivalenceClasses (bidiStar rtg0) (Merge.fromLists ["Cc"])
      ~?= M.empty
        {-
        [ (Rule 'A' "BC" 's', [Rule 'A' "BC" 's'])
        , (Rule 'B' "AC" 's', [Rule 'B' "AC" 's'])
        , (Rule 'C' ""   'C', [Rule 'C' ""   'C'])
        , (Rule 'C' ""   'c', [Rule 'c' ""   'c'])
        , (Rule 'a' "bC" 's', [Rule 'a' "bc" 's'])
        , (Rule 'b' "aC" 's', [Rule 'b' "ac" 's'])
        ]
        -}
    , ruleEquivalenceClasses (bidiStar rtg0) (Merge.fromLists ["Aa", "Bb"])
      ~?= M.empty
        {-
        [ (Rule 'A' "BC" 's', [Rule 'A' "BC" 's'])
        , (Rule 'A' "Bc" 's', [Rule 'a' "bc" 's'])
        , (Rule 'B' "AC" 's', [Rule 'B' "AC" 's'])
        , (Rule 'B' "Ac" 's', [Rule 'b' "ac" 's'])
        ]
        -}
    , ruleEquivalenceClasses (bidiStar rtg0) (Merge.fromLists ["Aa", "Bb", "Cc"])
      ~?= M.fromList
        [ (Rule 'A' "BC" 's', [Rule 'a' "bc" 's', Rule 'A' "BC" 's'])
        , (Rule 'B' "AC" 's', [Rule 'b' "ac" 's', Rule 'B' "AC" 's'])
        {-
        , (Rule 'C' ""   'C', [Rule 'C' ""   'C'])
        , (Rule 'C' ""   'c', [Rule 'c' ""   'c'])
        -}
        ]
    ]
  , "fst . likelihoodDelta" ~: TestList
    [ (ln $ fst $ likelihoodDelta crtg0 Merge.empty) ~?= 0
    , (ln $ fst $ likelihoodDelta crtg0 $ Merge.fromLists ["Cc"]) ~?= log ((3**3 * 6**6) / 9**9)
    , TestCase $ assertRoughly "" 1e-10
        ( ln $ fst $ likelihoodDelta crtg0 $ Merge.fromLists ["Aa", "Bb", "Cc"] )
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
  , "snd . likelihoodDelta" ~: TestList
    [ (snd $ likelihoodDelta crtg0 Merge.empty) ~?= (0, 0, 0)
    , (snd $ likelihoodDelta crtg0 $ Merge.fromLists ["Cc"]) ~?= (0, 1, 0)
    , (snd $ likelihoodDelta crtg0 $ Merge.fromLists ["Aa", "Bb", "Cc"]) ~?= (2, 3, 1)
    ]
  , "saturateMerge" ~: TestList
    [ (Merge.forward $ saturateMerge (forwardStar rtg0) $ Merge.empty) ~?= (Merge.forward $ Merge.empty)
    , (Merge.forward $ saturateMerge (forwardStar rtg1) $ Merge.fromLists ["AB", "GH"]) ~?= (Merge.forward $ Merge.fromLists ["AB", "CD", "EF", "GH"])
    , (Merge.forward $ saturateMerge (forwardStar rtg1) $ Merge.fromLists ["CD", "EF", "IJ"]) ~?= (Merge.forward $ Merge.fromLists ["CD", "EF", "IJ"])
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


rtg0 :: [Rule Char Char]
rtg0
  = [ Rule 'A' "BC" 's'
    , Rule 'B' "AC" 's'
    , Rule 'C' ""   'C'
    , Rule 'a' "bc" 's'
    , Rule 'b' "ac" 's'
    , Rule 'c' ""   'c'
    ]


rtg1 :: [Rule Char Char]
rtg1
  = [ Rule 'C' "AZ" 's'
    , Rule 'D' "BZ" 's'
    , Rule 'E' "CA" 's'
    , Rule 'F' "DB" 's'
    , Rule 'I' "GZ" 'a'
    , Rule 'J' "HZ" 'b'
    ]


crtg0 :: CRTG Char Char
crtg0 = CRTG
  (M.fromList $ zip rtg0 [1 ..])
  (M.fromList [('A', 1), ('B', 2), ('C', 3), ('a', 4), ('b', 5), ('c', 6)])
  (M.fromList [('A', 1), ('a', 2)])


t0, t1 :: Tree String
t0 = parseTree "A(B(D, E), C(D, E))"
t1 = parseTree "X(B(Y, E), C(Z, E))"

