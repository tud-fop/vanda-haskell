module Data.RevMapTests where
import Data.RevMap

{-
import TestUtil
-}

import           Control.Arrow ((***))
import           Data.List (inits)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import qualified Data.MultiMap as MM
import qualified Data.Set as S
import           Data.Tuple (swap)
import           Test.HUnit



tests :: Test
tests =
  let insertions =
        [ (4, 'a')
        , (6, 'b')
        , (2, 'b')
        , (6, 'a')
        , (8, 'b')
        , (2, 'c')
        ]

      ms = scanl (\ acc (k, v) -> insert k v acc) empty insertions

      ls = [ []
           , [(4, 'a')]
           , [(4, 'a'), (6, 'b')]
           , [(2, 'b'), (4, 'a'), (6, 'b')]
           , [(2, 'b'), (4, 'a'), (6, 'a')]
           , [(2, 'b'), (4, 'a'), (6, 'a'), (8, 'b')]
           , [(2, 'c'), (4, 'a'), (6, 'a'), (8, 'b')]
           ]

      -- equivalence classes
      es = [ []
           , [[4]]
           , [[4], [6]]
           , [[2, 6], [4], [2, 6]]
           , [[2], [4, 6], [4, 6]]
           , [[2, 8], [4, 6], [4, 6], [2, 8]]
           , [[2], [4, 6], [4, 6], [8]]
           ]

  in TestList
  [ "insert/delete/toAscList" ~: TestList $ zipWith (\ m l -> TestList
    [ "forward"  ~: M.toAscList (forward m) ~?= l
    , "backward" ~: (S.fromList $ map swap $ MM.toList $ backward m) ~?= S.fromList l
    ]) ms ls
  , "fromList" ~: TestList $ zipWith (\ m l -> TestList
    [ "forward"  ~: M.toAscList (forward m) ~?= l
    , "backward" ~: (S.fromList $ map swap $ MM.toList $ backward m) ~?= S.fromList l
    ]) (map fromList $ inits insertions) ls
  , "toList" ~: TestList $ zipWith (\ m l -> toList m ~?= l) ms ls
  , "equivalenceClass" ~: TestList $ zipWith (\ m e ->
          (map S.toList $ mapMaybe (\ k -> equivalenceClass k m) $ M.keys $ forward m) ~?= e
        ) ms es
  ]


{- tests if operations left-biased
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
