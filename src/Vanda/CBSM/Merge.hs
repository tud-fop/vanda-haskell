-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CBSM.Merge
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

module Vanda.CBSM.Merge
( Merge()
, empty
, fromSets
, fromLists
, insert
, size
, member
, elemS
, forward
, backward
, equivalenceClass
, equivalenceClasses
, apply
) where


import           Data.MultiMap (MultiMap)
import qualified Data.RevMap as RM
import           Data.RevMap (RevMap)

import qualified Data.Binary as B
import           Data.List (foldl')
import           Data.Map (Map, (!))
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Set (Set)


newtype Merge a = Merge (RevMap a a)


instance (B.Binary a, Ord a) => B.Binary (Merge a) where
  put (Merge m) = B.put m
  get           = fmap Merge B.get


empty :: Merge a
empty = Merge RM.empty


fromSets :: Ord a => [Set a] -> Merge a
fromSets = foldl' (flip insert) empty


fromLists :: Ord a => [[a]] -> Merge a
fromLists = fromSets . map S.fromList


insert :: Ord a => Set a -> Merge a -> Merge a
insert new old
  | S.size new < 2 = old
  | otherwise
    = insertList (S.toList new)
    $ flip insertList old
    $ S.toList
    $ S.unions
    $ map ((backward old !) . (forward old !))  -- equivalence class
    $ S.toList
    $ S.intersection new
    $ M.keysSet
    $ forward old
  where
    representative = S.findMin new
    insertList
      = flip $ foldl' (\ (Merge m) k -> Merge (RM.insert k representative m))


size :: Merge a -> Int
size = sum . map (pred . S.size) . equivalenceClasses


member :: Ord a => a -> Merge a -> Bool
member k = M.member k . forward


elemS :: Merge a -> Set a
elemS = M.keysSet . forward


forward :: Merge a -> Map a a
forward (Merge m) = RM.forward m


backward :: Merge a -> MultiMap a a
backward (Merge m) = RM.backward m


equivalenceClass :: Ord a => a -> Merge a -> Maybe (Set a)
equivalenceClass k (Merge m) = RM.equivalenceClass k m


equivalenceClasses :: Merge a -> [Set a]
equivalenceClasses (Merge m) = RM.equivalenceClasses m


apply :: Ord a => Merge a -> a -> a
apply (Merge m) = \ k -> M.findWithDefault k k (RM.forward m)
