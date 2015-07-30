module Data.MultiMapTests where
import Data.MultiMap

import TestUtil

import           Control.Arrow ((***))
import qualified Data.Map as M
import qualified Data.Set as S
import           Test.HUnit


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

      m0 = insert ox00 ox00 empty
      m1 = insert ox00 ox01 m0
      m2 = insert ox10 ox10 m1
      m3 = insert ox01 ox01 m2
      m4 = insert ox00 ox00 m3
      m5 = insert ox00 ox10 m4
      m6 = insert ox00 ox11 m5
      m7 = delete ox00 ox10 m6
      m8 = delete ox11 ox11 m7

      l0 = [(x00, x00)]
      l1 = [(x00, x01)]
      l2 = [(x00, x01), (x10, x10)]
      l3 = [(x01, x01), (x10, x10)]
      l4 = [(x00, x00), (x10, x10)]
      l5 = [(x00, x00), (x00, x10), (x10, x10)]
      l6 = [(x00, x00), (x00, x11), (x10, x10)]
      l7 = [(x00, x00), (x10, x10)]

      ordOnFstPairs   = map (  OrdOnFst ***   OrdOnFst)
      unOrdOnFstPairs = map (unOrdOnFst *** unOrdOnFst)
  in TestList
  [ "insert/delete/toAscList" ~: TestList
    [ unOrdOnFstPairs (toAscList m0) ~?= l0
    , unOrdOnFstPairs (toAscList m1) ~?= l1
    , unOrdOnFstPairs (toAscList m2) ~?= l2
    , unOrdOnFstPairs (toAscList m3) ~?= l3
    , unOrdOnFstPairs (toAscList m4) ~?= l4
    , unOrdOnFstPairs (toAscList m5) ~?= l5
    , unOrdOnFstPairs (toAscList m6) ~?= l6
    , unOrdOnFstPairs (toAscList m7) ~?= l7
    , m8 ~?= M.fromList [(ox00, S.singleton ox00)]
      -- there must not be an empty Set in the Map
    ]
  , "fromList" ~: TestList
    [ fromList (ordOnFstPairs l0) ~?= m0
    , fromList (ordOnFstPairs l1) ~?= m1
    , fromList (ordOnFstPairs l2) ~?= m2
    , fromList (ordOnFstPairs l3) ~?= m3
    , fromList (ordOnFstPairs l4) ~?= m4
    , fromList (ordOnFstPairs l5) ~?= m5
    , fromList (ordOnFstPairs l6) ~?= m6
    , fromList (ordOnFstPairs l7) ~?= m7
    , fromList [] ~?= (empty :: MultiMap () ())
    ]
  , "toList" ~: TestList
    [ S.fromList (unOrdOnFstPairs (toList m0)) ~?= S.fromList l0
    , S.fromList (unOrdOnFstPairs (toList m1)) ~?= S.fromList l1
    , S.fromList (unOrdOnFstPairs (toList m2)) ~?= S.fromList l2
    , S.fromList (unOrdOnFstPairs (toList m3)) ~?= S.fromList l3
    , S.fromList (unOrdOnFstPairs (toList m4)) ~?= S.fromList l4
    , S.fromList (unOrdOnFstPairs (toList m5)) ~?= S.fromList l5
    , S.fromList (unOrdOnFstPairs (toList m6)) ~?= S.fromList l6
    , S.fromList (unOrdOnFstPairs (toList m7)) ~?= S.fromList l7
    ]
  ]
