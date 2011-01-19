-- Copyright (c) 2010, Toni Dietze

module Tools.Miscellaneous(
-- * Mapping
-- ** Mapping for pairs
  mapFst
, mapSnd
-- **Mapping with randomness
, mapRandomR
, mapRandom
-- * Folding
, sumWith
, productWith
) where

import qualified Data.List as L
import qualified Random as R

-- -----------------------------------------------------------------------------
-- | Apply a function to the first component of a pair.
mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)


-- | Apply a function to the second component of a pair.
mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

-- -----------------------------------------------------------------------------
-- | @mapRandomR r f xs g == (ys, g')@, where @ys@ is the list obtained by
-- applying f to each element of xs and a random value in the range @r@, and
-- @g'@ is a new random generator.
mapRandomR
  :: (R.Random r, R.RandomGen g)
  => (r, r) -> (a -> r -> b) -> [a] -> g -> ([b], g)
mapRandomR r = mapRandom' (R.randomR r)


-- | @mapRandom f xs g == (ys, g')@, where @ys@ is the list obtained by
-- applying f to each element of xs and a random value, and
-- @g'@ is a new random generator.
mapRandom
  :: (R.Random r, R.RandomGen g)
  => (a -> r -> b) -> [a] -> g -> ([b], g)
mapRandom = mapRandom' R.random


-- | @mapRandom' r f xs g == (ys, g')@, where @ys@ is the list obtained by
-- applying f to each element of xs and a random value produced by @r@, and
-- @g'@ is a new random generator.
mapRandom' :: (g -> (r, g)) -> (a -> r -> b) -> [a] -> g -> ([b], g)
mapRandom' random f (x:xs) g
  = let (r , g' ) = random g
        (ys, g'') = mapRandom' random f xs g'
    in  (f x r : ys, g'')
mapRandom' _ _ [] g
  = ([], g)

-- -----------------------------------------------------------------------------
-- | Sum the elements of a list after mapping them to a 'Num'; i.e.
-- @sumWith f == sum . map f@
sumWith :: (Num b) => (a -> b) -> [a] -> b
sumWith f = L.foldl' (\s x -> s + f x) 0


-- | Multiply the elements of a list after mapping them to a 'Num'; i.e.
-- @productWith f == product . map f@
productWith :: (Num b) => (a -> b) -> [a] -> b
productWith f = L.foldl' (\s x -> s * f x) 0
