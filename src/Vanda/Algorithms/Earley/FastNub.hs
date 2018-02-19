-------------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Toni Dietze 2010
-- License     :  BSD-style
-- Maintainer  :  Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Stability   :  unknown
-- Portability :  portable
-------------------------------------------------------------------------------

module Vanda.Algorithms.Earley.FastNub(nub) where

import qualified Data.Set as Set


-- | Removes duplicate elements from a list by exploiting the 'Ord' relation.
-- In particular, it keeps only the first occurrence of each element.
nub :: (Ord a) => [a] -> [a]
nub xs = f xs Set.empty
  where
    f (y:ys) s = if Set.member y s
                 then f ys s
                 else y : f ys (Set.insert y s)
    f []     _ = []


-- this would not be lazy
-- nub xs = Set.toList $ Set.fromList xs
