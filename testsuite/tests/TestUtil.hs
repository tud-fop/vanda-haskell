-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Technische Universität Dresden 2018
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module TestUtil
( int
, OrdOnFst(..)
, assertRoughly
) where


import Control.Monad (unless)
import Data.Function (on)
import Test.HUnit.Base


int :: Int -> Int
int = id


newtype OrdOnFst a b = OrdOnFst {unOrdOnFst :: (a, b)} deriving Show

instance Eq a => Eq (OrdOnFst a b) where
  (==) = (==) `on` (fst . unOrdOnFst)

instance Ord a => Ord (OrdOnFst a b) where
  compare = compare `on` (fst . unOrdOnFst)


-- | Asserts that the specified actual value differs at most by epsilon from
-- the expected value.
assertRoughly
  :: (Num a, Ord a, Show a)
  => String -- ^ The message prefix
  -> a      -- ^ epsilon
  -> a      -- ^ The expected value
  -> a      -- ^ The actual value
  -> Assertion
assertRoughly preface epsilon expected actual
  = unless (diff <= epsilon) (assertFailure msg)
  where diff = abs (actual - expected)
        msg  = (if null preface then "" else preface ++ "\n")
            ++   "  expected roughly: " ++ show expected
            ++ "\n           but got: " ++ show actual
            ++ "\naccepted deviation: " ++ show epsilon
            ++ "\n  actual deviation: " ++ show diff
