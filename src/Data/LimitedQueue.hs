module Data.LimitedQueue 
    ( Queue
    -- * constructor
    , empty
    , fromList
    -- * i/o
    , enq
    , enqList
    , deq
    -- * query
    , null
    ) where

import qualified Data.Map.Strict  as M
import qualified Data.HashSet     as S

import Data.Hashable (Hashable)
import Prelude hiding(null)

data Queue v p = LPQ (M.Map p [v]) (S.HashSet v) p Int Int

instance (Show v, Show p) => Show (Queue v p) where
  show (LPQ _ known _ 0 limit) = "Empty queue (0/" ++ show limit ++ ")\n already visited: " ++ (unlines $ map show $ S.toList known)
  show (LPQ q known least current limit) = "Queue (" ++ show current ++ "/" ++ show limit ++ "//" ++ show least ++ ") with elements: \n" 
                                            ++ show q ++ "\n already visited: " ++ (unlines $ map show $ S.toList known)


-- | Safe constructor, initializes an empty priority queue that is limited to a certain bound.
empty :: Int -> Queue v p
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


-- | Enqueue a (value, priority) tuple. 
-- If the queue reached its size limit in a previous step, the element with least priority is thrown away. 
enq :: (Ord p, Hashable v, Eq v) => Queue v p -> (v, p) -> Queue v p
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
      unknown = {-# SCC known_member #-} not $ value `S.member` known
      known' = {-# SCC known_insert #-} S.insert value known


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