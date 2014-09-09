{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CBSM.BiMap
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

module Vanda.CBSM.BiMap
( BiMap()
, forward
, backward
, empty
, insert
, equivalenceClass
) where


import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)


data BiMap k v = BiMap
  { forward  :: Map k v
  , backward :: Map v (Set k)
  } deriving Show


empty :: BiMap k v
empty = BiMap M.empty M.empty


insert :: (Ord k, Ord v) => k -> v -> BiMap k v -> BiMap k v
insert k v BiMap{..}
  = BiMap
      (M.insert k v forward)
      (M.insertWith S.union v (S.singleton k) (M.adjust (S.delete k) v backward))


equivalenceClass :: (Ord k, Ord v) => k -> BiMap k v -> Maybe (Set k)
equivalenceClass x BiMap{..} = M.lookup x forward >>= flip M.lookup backward
