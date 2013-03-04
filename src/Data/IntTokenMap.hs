-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMap
-- Copyright   :  (c) Technische Universität Dresden 2013
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Tobias.Denkinger@mailbox.tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Maps a values to 'Int's savely while remembering old mappings.
--
-----------------------------------------------------------------------------

module Data.IntTokenMap where

import Data.Hashable ( Hashable )
import Vanda.Util

empty :: Interner t
empty = emptyInterner

getInt :: (Hashable t, Eq t) => Interner t -> t -> (Interner t, Int)
getInt = intern

getInts :: (Hashable t, Eq t) => Interner t -> [t] -> (Interner t, [Int])
getInts = internList