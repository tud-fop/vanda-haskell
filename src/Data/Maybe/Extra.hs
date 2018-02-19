-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Maybe.Extra
-- Copyright   :  (c) Technische Universität Dresden 2016
-- License     :  BSD-style
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


-- | Return 'Nothing' if predicate is 'True' for the value, otherwise wrap
-- the value in 'Just'.
nothingIf :: (a -> Bool) -> a -> Maybe a
nothingIf p x = if p x then Nothing else Just x
