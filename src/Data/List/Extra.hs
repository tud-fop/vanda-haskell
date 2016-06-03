-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Extra
-- Copyright   :  (c) Technische Universität Dresden 2016
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Additional operations on lists.
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}

module Data.List.Extra
( -- * Basic functions
  isSingleton
, -- * Sublists
  -- ** Extracting sublists
  spanWithLength
, groupWithRanges
, toRanges
, -- * Generalized functions
  -- ** The “By” operations
  -- *** User-supplied equality (replacing an Eq context)
  groupByWithRanges
, -- *** User-supplied comparison (replacing an Ord context)
  minimaBy
)
where


import Data.List (foldl')


-- | Test whether a list contains exactly one element.
isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton  _  = False


-- | Like 'span', but the length of the prefix is returned additionally.
spanWithLength :: (a -> Bool) -> [a] -> ([a], Int, [a])
spanWithLength p xs@(x : xs')
  = if p x
    then let (ys, !l, zs) = spanWithLength p xs' in (x : ys, succ l, zs)
    else ([], 0, xs)
spanWithLength _ []
  = ([], 0, [])


-- | Like 'Data.List.group', but the start and end indices of the groups are
-- returned additionally. For example
--
-- > groupWithRanges "aaabbc" == [(0, 2, "aaa"), (3, 4, "bb"), (5, 5, "c")]
groupWithRanges :: Eq a => [a] -> [(Int, Int, [a])]
groupWithRanges = groupByWithRanges (==)


-- | Summarize contiguous ranges by their lower and upper bound. Note that
--
-- > xs == (concat $ map (uncurry enumFromTo) $ toRanges xs)
toRanges :: (Enum a, Eq a) => [a] -> [(a, a)]
toRanges [] = []
toRanges (x : xs) = go x x xs
  where
    go lo hi []       = [(lo, hi)]
    go lo hi (y : ys) = if succ hi == y
                        then go lo y ys
                        else (lo, hi) : go y y ys


-- | Like 'Data.List.groupBy', but the start and end indices of the groups
-- returned additionally. For example
--
-- > groupByWithRanges ((==) `on` even) [2, 4, 6, 3, 1, 2]
-- >   == [(0, 2, [2, 4, 6]), (3, 4, [3, 1]), (5, 5, [2])]
groupByWithRanges :: (a -> a -> Bool) -> [a] -> [(Int, Int, [a])]
groupByWithRanges eq = go 0
  where
    go !i (x : xs) = (i, i + l, x : ys) : go (i + l + 1) zs
      where (ys, !l, zs) = spanWithLength (eq x) xs
    go _ [] = []


-- | Like 'Data.List.minimumBy', but returns all least elements in their
-- original order.
minimaBy :: (a -> a -> Ordering) -> [a] -> [a]
minimaBy _   []       = []
minimaBy cmp (x : xs) = reverse $ uncurry (:) $ foldl' step (x, []) xs
  where
    step (m, ms) y
      = case cmp m y of
          LT -> (m, ms)
          EQ -> (y, m : ms)
          GT -> (y, [])
