-- (c) 2011 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

-- | This module is based on Section 3.1.1 of
--
-- * Chris Okasaki.
--   /Purely functional data structures./
--   1998.
--   <http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf>
module Data.Queue (
  Queue()
, null
, empty
, singleton
, enq
, deq
, enqList
, enqListWith
, toList
) where


import Prelude hiding (null)


data Queue a = Queue [a] [a] deriving (Show)


null :: Queue a -> Bool
null (Queue [] []) = True
null _             = False


empty :: Queue a
empty = Queue [] []


singleton :: a -> Queue a
singleton x = Queue [x] []


enq :: a -> Queue a -> Queue a
enq y (Queue xs ys) = Queue xs (y:ys)


deq :: Queue a -> (a, Queue a)
deq (Queue (x:xs) ys      ) = (x, Queue xs ys)
deq (Queue []     ys@(_:_)) = deq (Queue (reverse ys) [])
deq (Queue []     []      ) = error "Cannot dequeue from empty queue."


enqList :: [a] -> Queue a -> Queue a
enqList xs q = foldr enq q xs


enqListWith :: (b -> a) -> [b] -> Queue a -> Queue a
enqListWith f xs q = foldr (enq . f) q xs


toList :: Queue a -> [a]
toList (Queue xs ys) = xs ++ (reverse ys)
