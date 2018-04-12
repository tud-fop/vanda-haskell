-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Technische Universität Dresden 2018
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Control.ErrorTests where
import Control.Error

import Test.HUnit


tests :: Test
tests = TestList
  [ messageHere ""  ""  ""  ~?= ""
  , messageHere ""  ""  "x" ~?= "x"
  , messageHere ""  "f" ""  ~?= "f"
  , messageHere ""  "f" "x" ~?= "f: x"
  , messageHere "m" ""  ""  ~?= "m"
  , messageHere "m" ""  "x" ~?= "m: x"
  , messageHere "m" "f" ""  ~?= "m.f"
  , messageHere "m" "f" "x" ~?= "m.f: x"
  ]
