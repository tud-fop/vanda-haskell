-- Copyright (c) 2010, Toni Dietze

module Tools.Miscellaneous where


-- | Apply a function to the first component of a pair.
mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)


-- | Apply a function to the second component of a pair.
mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)
