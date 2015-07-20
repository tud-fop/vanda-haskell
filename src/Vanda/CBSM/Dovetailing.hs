-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CBSM.Dovetailing
-- Copyright   :  (c) Technische Universität Dresden 2014
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.CBSM.Dovetailing where


import Data.Either


-- | Interleave the repeated applications of a function to a list of values
-- until the application yields 'Right', respectively.
-- The order of the results is unspecified.
--
-- This function is especially useful in combination with 'take'.
dovetail :: (a -> Either a b) -> [a] -> [b]
dovetail step = go1 []
  where
    go1 xs (y : ys) = let (ls, rs) = partitionEithers (map step (y : xs))
                      in rs ++ go1 ls ys
    go1 xs []       = go2 xs

    go2 []          = []
    go2 xs          = let (ls, rs) = partitionEithers (map step xs)
                      in rs ++ go2 ls


untilRight :: (a -> Either a b) -> a -> b
untilRight f a
  = case f a of
      Left a' -> untilRight f a'
      Right b -> b
