{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.RevMap
-- Copyright   :  (c) Technische Universität Dresden 2014
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- 'M.Map'-like structure which allows fast access to the inverse mapping.
--
-- Implementation is left-biased like 'M.Map'.
--
-- ToDo: Maybe drop left-bias for better efficiency?
-----------------------------------------------------------------------------

module Data.RevMap
( RevMap()
, forward
, backward
, empty
, insert
, fromList
, equivalenceClass
) where


import qualified Data.MultiMap as MM
import           Data.MultiMap (MultiMap)


import           Data.List (foldl')
import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Tuple (swap)


data RevMap k v = RevMap
  { forward  :: Map k v
  , backward :: MultiMap v k
  } deriving Show


empty :: RevMap k v
empty = RevMap M.empty M.empty


insert :: (Ord k, Ord v) => k -> v -> RevMap k v -> RevMap k v
-- insert k v RevMap{..} = RevMap f b
--   where (mv, f) = M.insertLookupWithKey (\ _ x _ -> x) k v forward
--         b = MM.insert v k $ maybe id (\ v' -> MM.delete v' k) mv $ backward
insert k v RevMap{..} = RevMap f b
  where b = MM.insert v k
          $ maybe id (\ v' -> MM.delete v' k) (M.lookup k forward) backward
        f = foldl' (\ m k' -> M.insert k' v m) forward
          $ S.toList
          $ b M.! v


fromList :: (Ord k, Ord v) => [(k, v)] -> RevMap k v
fromList = foldl' (\ m (k, v) -> insert k v m) empty


equivalenceClass :: (Ord k, Ord v) => k -> RevMap k v -> Maybe (Set k)
equivalenceClass x RevMap{..} = M.lookup x forward >>= flip M.lookup backward
