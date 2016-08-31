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
, union
, size
, member
, elemS
, forward
, backward
, equivalenceClass
, equivalenceClasses
, apply
, applyMaybe
, applyMergeToMerge
, prettyPrintMerge
) where


import qualified Control.Error
import           Data.MultiMap (MultiMap)
import qualified Data.RevMap as RM
import           Data.RevMap (RevMap)

import           Control.DeepSeq (NFData(rnf))
import qualified Data.Binary as B
import           Data.List (foldl')
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import           Data.Ord
import qualified Data.Set as S
import           Data.Set (Set)


errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.CBSM.Merge"


newtype Merge a = Merge (RevMap a a)


instance (B.Binary a, Ord a) => B.Binary (Merge a) where
  put (Merge m) = B.put m
  get           = fmap Merge B.get


instance NFData a => NFData (Merge a) where
  rnf (Merge m) = rnf m


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
    $ insertList todo old
  where
    eqClasses
      = M.toList
      $ M.intersection (backward old)
      $ M.fromList
      $ map (\ x -> (x, undefined))
      $ M.elems  -- there might be double entries
      $ M.intersection (forward old)
      $ M.fromSet undefined new
    (representative, todo)
      = if null eqClasses
        then (S.findMin new, [])
        else let ((r, _), xs)
                   = cutMaximumBy (comparing (S.size . snd)) eqClasses
             in (r, concatMap (S.toList . snd) xs)
    insertList
      = flip $ foldl' (\ (Merge m) k -> Merge (RM.insert k representative m))


-- | Find maximum and return it and the remainding list.
-- /Caution:/ The returned list may have another order.
cutMaximumBy :: Ord a => (a -> a -> Ordering) -> [a] -> (a, [a])
cutMaximumBy cmp (x : xs) = go x xs
  where
    go m []       = (m, [])
    go m (y : ys) = case cmp y m of
                      GT -> let (m', ys') = go y ys in (m', m : ys')
                      _  -> let (m', ys') = go m ys in (m', y : ys')
cutMaximumBy _ [] = errorHere "cutMaximumBy" "empty list"


union :: Ord a => Merge a -> Merge a -> Merge a
union m = foldl' (flip insert) m . equivalenceClasses


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


applyMaybe :: Ord a => Merge a -> a -> Maybe a
applyMaybe (Merge m) = \ k -> M.lookup k (RM.forward m)


applyMergeToMerge
  :: Ord a
  => Merge a  -- ^ applied 'Merge'
  -> Merge a
  -> Merge a
applyMergeToMerge m = fromSets . map (S.map (apply m)) . equivalenceClasses


prettyPrintMerge :: Show a => Merge a -> String
prettyPrintMerge
  = unlines
  . map (\ (k, vS) -> show k ++ " <- " ++ show (S.toList vS))
  . M.toList
  . backward
