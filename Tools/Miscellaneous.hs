-- Copyright (c) 2010, Toni Dietze

module Tools.Miscellaneous where

import qualified Data.List as L

-- | Apply a function to the first component of a pair.
mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)


-- | Apply a function to the second component of a pair.
mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)


-- | Sum the elements of a list after mapping them to a 'Num'; i.e.
-- @sumWith f == sum . map f@
sumWith :: (Num b) => (a -> b) -> [a] -> b
sumWith f = L.foldl' (\s x -> s + f x) 0


-- | Multiply the elements of a list after mapping them to a 'Num'; i.e.
-- @productWith f == product . map f@
productWith :: (Num b) => (a -> b) -> [a] -> b
productWith f = L.foldl' (\s x -> s * f x) 0
