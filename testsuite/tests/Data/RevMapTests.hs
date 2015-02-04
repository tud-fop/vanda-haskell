module Data.RevMapTests where
import Data.RevMap


import           Data.Binary
import           Data.List (inits)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import qualified Data.MultiMap as MM
import qualified Data.Set as S
import           Data.Tuple (swap)
import           Test.HUnit



tests :: Test
tests =
  let insertions :: [(Int, Char)]
      insertions =
        [ (4, 'a')
        , (6, 'b')
        , (2, 'b')
        , (6, 'a')
        , (8, 'b')
        , (2, 'c')
        ]

      ms :: [RevMap Int Char]
      ms = scanl (\ acc (k, v) -> insert k v acc) empty insertions

      ls :: [[(Int, Char)]]
      ls = [ []
           , [(4, 'a')]
           , [(4, 'a'), (6, 'b')]
           , [(2, 'b'), (4, 'a'), (6, 'b')]
           , [(2, 'b'), (4, 'a'), (6, 'a')]
           , [(2, 'b'), (4, 'a'), (6, 'a'), (8, 'b')]
           , [(2, 'c'), (4, 'a'), (6, 'a'), (8, 'b')]
           ]

      -- equivalence classes
      es :: [[[Int]]]
      es = [ []
           , [[4]]
           , [[4], [6]]
           , [[2, 6], [4], [2, 6]]
           , [[2], [4, 6], [4, 6]]
           , [[2, 8], [4, 6], [4, 6], [2, 8]]
           , [[2], [4, 6], [4, 6], [8]]
           ]

  in TestList
  [ "put/get"  ~: testRevMaps (fmap (decode . encode) ms) ls
  , "insert"   ~: testRevMaps ms ls
  , "fromMap"  ~: testRevMaps (fmap (fromMap . forward) ms) ls
  , "fromList" ~: testRevMaps (fmap fromList $ inits insertions) ls
  , "toList" ~: TestList $ zipWith (\ m l -> toList m ~?= l) ms ls
  , "equivalenceClass" ~: TestList $ zipWith (\ m e ->
          (fmap S.toList $ mapMaybe (\ k -> equivalenceClass k m) $ M.keys $ forward m) ~?= e
        ) ms es
  ]


testRevMaps
  :: (Ord k, Ord v, Show k, Show v) => [RevMap k v] -> [[(k, v)]] -> Test
testRevMaps ms ls = TestList $ zipWith testRevMap ms ls


testRevMap :: (Ord k, Ord v, Show k, Show v) => RevMap k v -> [(k, v)] -> Test
testRevMap m l = TestList
  [ "forward"  ~: M.toAscList (forward m) ~?= l
  , "backward" ~: (S.fromList $ fmap swap $ MM.toList $ backward m)
                  ~?= S.fromList l
  ]


{- tests if operations left-biased

import TestUtil

import           Control.Arrow ((***))


tests :: Test
tests =
  let x00 = (int 0, int 0)
      x01 = (int 0, int 1)
      x10 = (int 1, int 0)
      x11 = (int 1, int 1)

      ox00 = OrdOnFst x00
      ox01 = OrdOnFst x01
      ox10 = OrdOnFst x10
      ox11 = OrdOnFst x11

      insertions =
        [ (ox00, ox00)
        , (ox10, ox10)
        , (ox10, ox11)
        , (ox11, ox11)
        , (ox11, ox01)
        , (ox10, ox10)
        ]

      ms = scanl (\ acc (k, v) -> insert k v acc) empty insertions

      ls = [ []
           , [(x00, x00)]
           , [(x00, x00), (x10, x10)]
           , [(x00, x00), (x10, x11)]
           , [(x00, x00), (x11, x11)]
           , [(x00, x01), (x11, x01)]
           , [(x00, x01), (x10, x10)]
           ]

      -- equivalence classes
      es = [ []
           , [[x00]]
           , [[x00], [x10]]
           , [[x00], [x10]]
           , [[x00], [x11]]
           , [[x00, x11], [x00, x11]]
           , [[x00], [x10]]
           ]

      -- ordOnFstPairs   = map (  OrdOnFst ***   OrdOnFst)
      unOrdOnFstPairs = map (unOrdOnFst *** unOrdOnFst)

      flatForward  = unOrdOnFstPairs . M.toAscList . forward
      -- flatBackward = map (unOrdOnFst *** (map unOrdOnFst . S.toAscList))
      --              . M.toAscList
      --              . backward
  in TestList
  [ "insert/delete/toAscList" ~: TestList $ zipWith (\ m l -> TestList
    [ "forward"  ~: flatForward m ~?= l
    , "backward" ~: (S.fromList $ map swap $ unOrdOnFstPairs $ MM.toList $ backward m) ~?= S.fromList l
    ]) ms ls
  , "fromList" ~: TestList $ zipWith (\ m l -> TestList
    [ "forward"  ~: flatForward m ~?= l
    , "backward" ~: (S.fromList $ map swap $ unOrdOnFstPairs $ MM.toList $ backward m) ~?= S.fromList l
    ]) (map fromList $ inits insertions) ls
  , "toList" ~: TestList $ zipWith (\ m l -> unOrdOnFstPairs (toList m) ~?= l) ms ls
  , "equivalenceClass" ~: TestList $ zipWith (\ m e ->
          (map (map unOrdOnFst . S.toList) $ mapMaybe (\ k -> equivalenceClass k m) $ M.keys $ forward m) ~?= e
        ) ms es
  ]
-}
