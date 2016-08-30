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
-- Implementation is /not/ left-biased like 'M.Map'.
-----------------------------------------------------------------------------

module Data.RevMap
( RevMap()
, forward
, backward
, empty
, insert
, fromMap
, fromList
, toList
, equivalenceClass
, equivalenceClasses
, map
) where


import Prelude hiding (map)


import qualified Data.MultiMap as MM
import           Data.MultiMap (MultiMap)


import           Control.DeepSeq (NFData(rnf))
import qualified Data.Binary as B
import           Data.List (foldl')
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Tuple (swap)


data RevMap k v = RevMap
  { forward  :: Map k v
  , backward :: MultiMap v k
  } deriving Show


instance (B.Binary k, B.Binary v, Ord k, Ord v) => B.Binary (RevMap k v) where
  put (RevMap f _) = B.put f
  get = fmap fromMap B.get


instance (NFData k, NFData v) => NFData (RevMap k v) where
  rnf (RevMap f b) = rnf f `seq` rnf b


empty :: RevMap k v
empty = RevMap M.empty M.empty


insert :: (Ord k, Ord v) => k -> v -> RevMap k v -> RevMap k v
insert k v RevMap{..} = RevMap f b
  where (mv, f) = M.insertLookupWithKey (\ _ x _ -> x) k v forward
        b = MM.insert v k $ maybe id (\ v' -> MM.delete v' k) mv $ backward
-- insert k v RevMap{..} = RevMap f b
--   where b = MM.insert v k
--           $ maybe id (\ v' -> MM.delete v' k) (M.lookup k forward) backward
--         f = foldl' (\ m k' -> M.insert k' v m) forward
--           $ S.toList
--           $ b M.! v


fromMap :: (Ord k, Ord v) => Map k v -> RevMap k v
fromMap m = RevMap m $ backwardFromList $ M.toList m


backwardFromList :: (Ord k, Ord v) => [(k, v)] -> MultiMap v k
backwardFromList = MM.fromList . fmap swap


fromList :: (Ord k, Ord v) => [(k, v)] -> RevMap k v
fromList = foldl' (\ m (k, v) -> insert k v m) empty


toList :: RevMap k a -> [(k, a)]
toList = M.toList . forward


equivalenceClass :: (Ord k, Ord v) => k -> RevMap k v -> Maybe (Set k)
equivalenceClass x RevMap{..} = M.lookup x forward >>= flip M.lookup backward


equivalenceClasses :: RevMap k v -> [Set k]
equivalenceClasses = M.elems . backward


map :: (Ord k, Ord v2) => (v1 -> v2) -> RevMap k v1 -> RevMap k v2
map fun (RevMap f b) = RevMap (M.map fun f) (M.mapKeysWith S.union fun b)
