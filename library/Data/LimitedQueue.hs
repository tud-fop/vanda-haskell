-----------------------------------------------------------------------------
-- |
-- Module      :  Data.LimitedQueue
-- Copyright   :  (c) Thomas Ruprecht 2017
-- License     :  BSD-style
--
-- Maintainer  :  thomas.ruprecht@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- This module implements a search queue that:
--
-- * is limited to a fixed number of elements that it contains at once
-- * will never contain an element twice; even if it was dequeued, the item
-- will just be neglected disregarding of its weight
--
-- It is implemented using the 'findMin' and 'findMax' API of "Data.Map.Strict"
-- and ordered minimum first.
-----------------------------------------------------------------------------

module Data.LimitedQueue 
    ( -- * type and constructors
      Queue
    , empty
    , fromList
      -- * operations
    , enq
    , enqList
    , deq
    , null
    ) where

import qualified Data.Map.Strict  as M
import qualified Data.HashSet     as S

import Data.Hashable (Hashable)
import Prelude hiding(null)

-- | Consists of a map for storing (priority, value) pairs,
-- a set of already enqueued values,
-- the currently highest priority (i.e. the worst!) in the queue,
-- the current number of items
-- and the maximum possible amount of items in the queue.
data Queue v p = LPQ (M.Map p [v]) (S.HashSet v) p Int Int

instance (Show v, Show p) => Show (Queue v p) where
  show (LPQ _ known _ 0 limit) 
    = "Empty queue (0 of " ++ show limit ++ " elements)\n" 
    ++ "already visited: " ++ unlines (map show $ S.toList known)
  show (LPQ q known _ current limit) 
    = "Queue (" ++ show current ++ " of " ++ show limit ++ " elements)\n"
    ++ "currently contains " ++ show q ++ "\n"
    ++ "already visited: " ++ unlines (map show $ S.toList known)


-- | Initializes an empty priority queue that is limited to a certain bound.
empty :: Int        -- ^ maximum elements simultanously contained in queue
      -> Queue v p  -- ^ empty queue
empty = LPQ M.empty S.empty undefined 0


removeMax :: (Ord k) => k -> M.Map k [a] -> (Maybe k, M.Map k [a])
removeMax maxkey m = (fst <$> M.lookupMax m', m')
  where
    m' = M.update removeHead maxkey m

    removeHead [_] = Nothing
    removeHead (_:xs) = Just xs
    removeHead _ = error "remove head: empty list"

removeMin :: (Ord k) => M.Map k [a] -> (a, M.Map k [a])
removeMin m = case M.deleteFindMin m of
                   ((_, [a]), m') -> (a, m')
                   ((k, a:as), m') -> (a, M.insert k as m')
                   _ -> error "map containing an empty list"

addSingle :: (Ord k) => M.Map k [a] -> k -> a -> M.Map k [a]
addSingle m key val = M.insertWith prepend key [val] m
  where
    prepend [a] as = a:as
    prepend _ _ = error "prepend: non-singleton list used"


-- | Enqueue a /(value, priority)/ tuple. 
-- If the queue reached its size limit in a previous step, the element with least priority is thrown away. 
enq :: (Ord p, Hashable v, Eq v)
    => Queue v p
    -> (v, p) 
    -> Queue v p
enq (LPQ q known _ 0 lmt) (value, prio)
  | not $ value `S.member` known = LPQ (M.singleton prio [value]) (S.insert value known) prio 1 lmt
  | otherwise = LPQ q known undefined 0 lmt
enq (LPQ q known least current limit) (value, prio)
  | current < limit && unknown = LPQ (addSingle q prio value) known' (min least prio) (current + 1) limit
  | least < prio && unknown = case removeMax least q of
                                   (Just least', q') -> LPQ (addSingle q' prio value) known' (min least' prio) limit limit
                                   (Nothing, q') -> LPQ (addSingle q' prio value) known' prio limit limit
  | otherwise = LPQ q known least current limit
    where 
      unknown = not $ value `S.member` known
      known' = S.insert value known


-- | Enqueue a list of elements from left to right.
enqList :: (Ord p, Hashable v, Eq v) => Queue v p -> [(v, p)] -> Queue v p
enqList = foldl enq


-- | Initializes a queue from a list of (value, priority) tuples from left to right.
fromList :: (Ord p, Hashable v, Eq v) => Int -> [(v, p)] -> Queue v p
fromList limit = foldl enq (empty limit)


-- | Dequeue the element with greatest priority.
-- Throws an error if the queue is empty.
deq :: (Ord p) => Queue v p -> (Queue v p, v)
deq (LPQ _ _ _ 0 _) = error "empty q"
deq (LPQ q known _ 1 limit) = (LPQ M.empty known undefined 0 limit, fst $ removeMin q)
deq (LPQ q known least current limit) = case removeMin q of
                                             (e, q') -> (LPQ q' known least (current-1) limit, e)


-- | Returns true if the queue is empty.
null :: Queue v p -> Bool
null (LPQ _ _ _ 0 _) = True
null _ = False
