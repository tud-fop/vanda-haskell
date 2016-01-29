-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Maybe.Extra
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
-- Additional operations for 'Maybe'.
-----------------------------------------------------------------------------

module Data.Maybe.Extra
( nothingIf
) where


-- | Return 'Nothing' if predicate is 'False' for the value, otherwise wrap
-- the value in 'Just'.
nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p x = if p x then Nothing else Just x
