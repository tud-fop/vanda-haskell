-- Copyright (c) 2010, Toni Dietze
module Tools.FastNub(nub) where

import qualified Data.Set as Set

nub :: (Ord a) => [a] -> [a]
nub xs = f xs Set.empty
  where
    f (x:xs) s = if Set.member x s
                 then f xs s
                 else x:(f xs (Set.insert x s))
    f []     _ = []


-- this would not be lazy
-- nub xs = Set.toList $ Set.fromList xs
