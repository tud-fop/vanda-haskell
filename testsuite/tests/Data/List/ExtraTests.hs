module Data.List.ExtraTests where
import Data.List.Extra


import Data.Function (on)
import Data.Ord (comparing)
import Test.HUnit


tests :: Test
tests = TestList
  [ "spanWithLength" ~: TestList
    [ spanWithLength undefined ([] :: [()]) ~?= ([], 0, [])
    , spanWithLength (5 >) [1 .. 9 :: Int] ~?= ([1 .. 4], 4, [5 .. 9])
    ]
  , "groupWithRanges" ~: TestList
    [ groupWithRanges ([] :: [()]) ~?= []
    , groupWithRanges "aaaeeeaccc" ~?= [(0, 2, "aaa"), (3, 5, "eee"), (6, 6, "a"), (7, 9, "ccc")]
    ]
  , "toRanges" ~: TestList
    [ toRanges ([] :: [()]) ~?= []
    , toRanges [1 .. 9 :: Int] ~?= [(1, 9)]
    , toRanges [1, 2, 3, 5, 6, 7, 2, 4, 5 :: Int] ~?= [(1, 3), (5, 7), (2, 2), (4, 5)]
    ]
  , "groupByWithRanges" ~: TestList
    [ groupByWithRanges undefined ([] :: [()]) ~?= []
    , groupByWithRanges ((==) `on` head) ["a1", "a2", "b1", "c1", "c2", "c3", "a3", "a4", "b2"]
        ~?= [(0, 1, ["a1", "a2"]), (2, 2, ["b1"]), (3, 5, ["c1", "c2", "c3"]), (6, 7, ["a3", "a4"]), (8, 8, ["b2"])]
    ]
  , "minimaBy" ~: TestList
    [ minimaBy undefined [] ~?= ([] :: [()])
    , minimaBy (comparing head) ["d1", "b2", "d2", "b1", "a2", "c1", "a1", "b3", "a3", "c2"] ~?= ["a2", "a1", "a3"]
    ]
  ]
