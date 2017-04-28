-----------------------------------------------------------------------------
-- |
-- Module      :  Data.MultiHashMap
-- Copyright   :  (c) Technische Universität Dresden 2017
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  thomas.ruprecht@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Data.MultiHashMap
( MultiMap
, empty
, insert
, delete
, fromList
, toList
, (!)
, lookup
, lookupDefault
, elems
, keys
) where

import           Prelude hiding (lookup)
import qualified Data.List as L
import           Control.Arrow (second)
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Lazy as M
import           Data.HashMap.Lazy (HashMap)


type MultiMap k v = HashMap k [v]


empty :: MultiMap k v
empty = M.empty


insert :: (Eq k, Hashable k) => k -> v -> MultiMap k v -> MultiMap k v
insert k v = M.insertWith ((:) . head) k [v]


delete :: (Eq k, Eq v, Hashable k) => k -> v -> MultiMap k v -> MultiMap k v
delete k v = M.update (nullToNothing . L.delete v) k
  where 
    nullToNothing s = if null s then Nothing else Just s


fromList :: (Eq k, Hashable k) => [(k, v)] -> MultiMap k v
fromList = M.fromListWith ((:) . head) . map (second (: []))


toList :: MultiMap k v -> [(k, v)]
toList = concatMap (\ (k, vs) -> zip (repeat k) vs) . M.toList


(!) :: (Eq k, Hashable k) => MultiMap k v -> k -> [v]
(!) = (M.!)


lookup :: (Eq k, Hashable k) => k -> MultiMap k v -> [v]
lookup = lookupDefault []


lookupDefault :: (Eq k, Hashable k) => [v] -> k -> MultiMap k v -> [v]
lookupDefault = M.lookupDefault


keys :: MultiMap k v -> [k]
keys = M.keys


elems :: MultiMap k v -> [v]
elems = map snd . toList