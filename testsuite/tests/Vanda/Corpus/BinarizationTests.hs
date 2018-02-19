-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Technische Universität Dresden 2018
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Corpus.BinarizationTests where
import Vanda.Corpus.Binarization

import Data.Tree
import Test.HUnit


tests :: Test
tests = TestList $ map createTest encodeDecodes
  where
    createTest (name, f) = name ~: TestList (map (\ t -> f t ~?= t) (take 6 forest))


encodeDecodes :: [(String, Tree Int -> Tree Int)]
encodeDecodes
  = [ ("fcns"          , decodeFcns          pred . encodeFcns             0 succ)
    , ("leftbranching0", decodeLeftbranching pred . encodeLeftbranching0 0 0 succ)
    , ("leftbranching1", decodeLeftbranching pred . encodeLeftbranching1 0 0 succ)
    ]


forest :: Forest Int
forest = Node 0 [] : map f forest
  where
    f (Node i ts) = let j = succ i in Node j (take j forest)
