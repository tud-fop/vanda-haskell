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

import qualified Data.Map as M

data IntTokenMap v = 
  IntTokenMap { _mapping :: M.Map v Int
              , len      :: Int}

empty
  :: IntTokenMap v
empty
  = IntTokenMap M.empty 0

getInt
  :: (Ord v)
  => IntTokenMap v
  -> v
  -> (IntTokenMap v, Int)
getInt im@(IntTokenMap m l) k
  = case M.lookup k m of
         Just v -> (im, v)
         _      -> (im{ _mapping = M.insert k l m, len = l + 1 }, l)

getInts
  :: (Ord v)
  => IntTokenMap v
  -> [v]
  -> (IntTokenMap v, [Int])
getInts im ks
  = let f (m, xs) k = (m', x:xs) where (m', x) = getInt m k
    in  foldl f (im, []) ks