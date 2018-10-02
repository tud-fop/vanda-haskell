-----------------------------------------------------------------------------
-- |
-- Module      :  LimitedQueue
-- Copyright   :  (c) Thomas Ruprecht 2017
-- License     :  BSD-style
--
-- Maintainer  :  thomas.ruprecht@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- This module contains ranges and range vectors to represent subsequences as
-- a range of indices.
-----------------------------------------------------------------------------

module Data.Range 
  ( -- * types and constructors
    Range(Epsilon)
  , RvMem
  , Rangevector
    -- * ranges
  , singletons
  , entire
  , safeConc
  , isNonOverlapping
    -- * range vectos
  , singleton
  , fromList
  , (!)
  , vectorLength
  -- * range memory
  , withCapacity
  , (//)
  , intoRangevector
  , subsetEq
  ) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Intro as Alg
import Data.Hashable (Hashable(hashWithSalt))
import Data.List (elemIndices)
import Data.Maybe (fromMaybe)


-- | A range /(i, j)/ in a word /w/.
-- Consider i \< j for i \>= 0 and j \<= |w|
-- and 'Epsilon' substitutes all /(i, i)/ for /0 <= i <= |w|/.
data Range = Range Int Int
           | Epsilon -- ^ empty range of ε in w
            deriving (Show, Eq, Ord)

instance Hashable Range where
  salt `hashWithSalt` Epsilon = salt `hashWithSalt` (0 :: Int) `hashWithSalt` (0 :: Int)
  salt `hashWithSalt` (Range x y) = salt `hashWithSalt` x `hashWithSalt` y

-- | A range vector is a non-overlapping sequence of ranges.
newtype Rangevector = Rangevector (V.Vector (Int, Int)) deriving(Eq, Ord, Show)

instance Hashable Rangevector where
  salt `hashWithSalt` (Rangevector v) = V.foldl' (hashWithSalt) salt v 


-- | Index access for Range vectors. 
at, (!) :: Rangevector -> Int -> Range
at (Rangevector rv) i = let (x, y) = rv V.! i
                        in if x == y
                           then Epsilon
                           else Range x y
(!) = at


-- | Returns the unary 'Rangevector' of a single 'Range'.
--
-- >>> singleton (Range 0 1)
-- [(0, 1)]
singleton :: Range -> Rangevector
singleton r = fromMaybe undefined (fromList [r])


-- | Checks if 'Range's in a list are non-overlapping;
-- stores them in an /unboxed container/ on success.
--
-- >>> fromList [Range 0 1, Range 1 2]
-- Just [(0, 1), (1,2)]
--
-- >>> fromList [Range 0 1, Range 0 2]
-- Nothing
fromList :: [Range] -> Maybe Rangevector
fromList rs
  | isNonOverlapping rs = Just $ Rangevector $ V.fromList $ map unbox rs
  | otherwise           = Nothing
  where
    unbox (Range x y) = (x, y)
    unbox Epsilon     = (0, 0)


-- | Returns the length of a 'Rangevector'.
-- This is just a wrapper around "Data.Vector.Unboxed"'s function 'V.length'.
vectorLength :: Rangevector -> Int
vectorLength (Rangevector v) = V.length v


-- | Returns all unary singleton 'Range's of a token in a list of tokens.
-- I.e., a singleton 'Range' is a 'Range' of a single character in a word.
--
-- >>> singletons 'a' "aabbcc"
-- [Range 0 1, Range 1 2]
singletons :: (Eq t) 
           => t       -- token /a/
           -> [t]     -- sequence of tokens /w = aabbcc/
           -> [Range] -- list of singleton ranges of /a/ in /w/
singletons c w = map singleton' $ c `elemIndices` w
  where singleton' i = Range i $ i+1


-- | Returns the 'Range' of a word in itself.
--
-- >>> entire "abcd"
-- Range 0 4
entire :: [t]   -- sequence of tokens /w/ 
       -> Range -- range of /w/ in /w/
entire [] = Epsilon
entire xs = Range 0 $ length xs


-- | Concatenates two 'Range's, fails if neighboring ranges do not fit.
--
-- >>> safeConc (Range 0 1) (Range 1 77)
-- Just (Range 0 77)
--
-- >>> safeCOnc (Range 0 2) (Range 6 77)
-- Nothing
safeConc :: Range -> Range -> Maybe Range
safeConc Epsilon r = Just r
safeConc r Epsilon = Just r
safeConc (Range i j) (Range k l)
  | j == k = Just $ Range i l
  | otherwise = Nothing


-- | Checks for overlapping components in a list of ranges.
isNonOverlapping :: [Range] -> Bool
isNonOverlapping = isNonOverlapping' []
  where
    isNonOverlapping' :: [(Int, Int)] -> [Range] -> Bool
    isNonOverlapping' _ []                          = True
    isNonOverlapping' cache (Range i j : rs)
      | any (\ (k, l) -> (j > k && j < l) 
                         || (i > k && i < l) 
                         || i <= k && j >= l) cache = False
      | otherwise                                   = isNonOverlapping' ((i, j):cache) rs
    isNonOverlapping' cache (Epsilon : rs)          = isNonOverlapping' cache rs

newtype RvMem = RvMem (V.Vector (Int, Int)) deriving (Eq, Show)

withCapacity :: Int -> RvMem
withCapacity n = RvMem $ V.replicate n (-1, -1)

(//) :: RvMem -> (Int, Range) -> RvMem
(RvMem v) // (i, Range x y) = RvMem $ v V.// [(i, (x, y))]
(RvMem v) // (i, Epsilon) = RvMem $ v V.// [(i, (0, 0))]

subsetEq :: RvMem -> RvMem -> Bool
(RvMem v) `subsetEq` (RvMem v') = V.and $ V.zipWith go v v'
    where
        go (-1, -1) _ = True
        go a a' = a == a'

intoRangevector :: RvMem -> Maybe Rangevector
intoRangevector (RvMem v)
  | isNonOverlappingAndFull v = Just $ Rangevector v
  | otherwise = Nothing

isNonOverlappingAndFull :: V.Vector (Int, Int) -> Bool
isNonOverlappingAndFull = go 0 . V.toList . V.modify Alg.sort
  where
    go _ [] = True
    go r1 ((l2, r2) : v) = r1 <= l2 && go r2 v

instance Hashable RvMem where
  salt `hashWithSalt` (RvMem v) = V.foldl' hashWithSalt salt v