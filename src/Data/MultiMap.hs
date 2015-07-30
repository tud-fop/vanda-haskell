-----------------------------------------------------------------------------
-- |
-- Module      :  Data.MultiMap
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

module Data.MultiMap
( MultiMap
, empty
, insert
, delete
, fromList
, toList
, toAscList
) where


import           Control.Arrow (second)
import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)


type MultiMap k v = Map k (Set v)


empty :: MultiMap k v
empty = M.empty


insert :: (Ord k, Ord v) => k -> v -> MultiMap k v -> MultiMap k v
-- insert k v m = M.alter (Just . maybe (S.singleton v) (S.insert v)) k m
insert k v m = M.insertWith S.union k (S.singleton v) m
  -- workaround, because alter does not change key


delete :: (Ord k, Ord v) => k -> v -> MultiMap k v -> MultiMap k v
delete k v m = M.update (nullToNothing . S.delete v) k m
  where nullToNothing s = if S.null s then Nothing else Just s


fromList :: (Ord k, Ord v) => [(k, v)] -> MultiMap k v
fromList = M.fromListWith S.union . map (second S.singleton)


toList :: MultiMap k v -> [(k, v)]
toList = concatMap (\ (k, s) -> map ((,) k) (S.toList s)) . M.toList


toAscList :: MultiMap k v -> [(k, v)]
toAscList = concatMap (\ (k, s) -> map ((,) k) (S.toAscList s)) . M.toAscList
