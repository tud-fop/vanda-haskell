module Vanda.CBSM.MainTests where
import Vanda.CBSM.Main


import Control.Monad (liftM2)
import Data.List (sortBy)
import Data.Ord (comparing)
import Test.HUnit


tests :: Test
tests = TestList
  [ "sortedCartesianProductWith" ~: TestList
    [ testSortedCartesianProductWith (+) [] ([] :: [Int])
    , testSortedCartesianProductWith (+) [] [int 0]
    , testSortedCartesianProductWith (+) [int 0] []
    , testSortedCartesianProductWith (+) [int 0] [0]
    , testSortedCartesianProductWith (+) [int 0 .. 2] [0 .. 2]
    , testSortedCartesianProductWith (+) [int 0, 2, 4] [0 .. 2]
    , testSortedCartesianProductWith (+) [int 0, 2, 4] [0, 5, 10]
    ]
  , "sortedCartesianProductWith'" ~: TestList
    [ testSortedCartesianProductWith' (+) [] ([] :: [Int])
    , testSortedCartesianProductWith' (+) [] [int 0]
    , testSortedCartesianProductWith' (+) [int 0] []
    , testSortedCartesianProductWith' (+) [int 0] [0]
    , testSortedCartesianProductWith' (+) [int 0 .. 2] [0 .. 2]
    , testSortedCartesianProductWith' (+) [int 0, 2, 4] [0 .. 2]
    , testSortedCartesianProductWith' (+) [int 0, 2, 4] [0, 5, 10]
    ]
  ]


int :: Int -> Int
int = id


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
