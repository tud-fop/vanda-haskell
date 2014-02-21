-- (c) 2012 Matthias Büchse <Matthias.Buechse@tu-dresden.de>
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
  Queue(..)
, null
, empty
, singleton
, enq
, deq
, deqMaybe
, enqList
, enqListWith
, fromList
, toList
) where


import Prelude hiding (null)

import Data.List ( foldl' )

-- | Queue data type.
data Queue a = Queue [a] [a]


instance Eq a => Eq (Queue a) where
  q1 == q2 = toList q1 == toList q2


-- copied from Data.Set
instance Show a => Show (Queue a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (toList xs)


-- | @True@ iff 'Queue' is empty.
null :: Queue a -> Bool
null (Queue [] []) = True
null _             = False


-- | The empty 'Queue'.
empty :: Queue a
empty = Queue [] []


-- | Returns the 'Queue' containing exactly the given element.
singleton :: a -> Queue a
singleton x = Queue [x] []


-- | Add an element to a 'Queue'.
enq :: a -> Queue a -> Queue a
enq y (Queue xs ys) = Queue xs (y:ys)


-- | Remove the oldest element from the 'Queue' and return the element and
-- the 'Queue' without the element.
deq :: Queue a -> (a, Queue a)
deq (Queue (x:xs) ys      ) = (x, Queue xs ys)
deq (Queue []     ys@(_:_)) = deq (Queue (reverse ys) [])
deq (Queue []     []      ) = error "Cannot dequeue from empty queue."

-- | Remove the oldest element from the 'Queue' and return the element and
-- the 'Queue' without the element.
deqMaybe :: Queue a -> Maybe (a, Queue a)
deqMaybe (Queue (x:xs) ys      ) = Just (x, Queue xs ys)
deqMaybe (Queue []     ys@(_:_)) = deqMaybe (Queue ({-reverse-} ys) [])
deqMaybe (Queue []     []      ) = Nothing

-- | Add all elements of the given list to the 'Queue'.
-- /The order in which the list elements are added is not defined./
enqList :: [a] -> Queue a -> Queue a
enqList zs q -- @(Queue xs ys) = Queue xs (zs ++ ys)
  = foldl' (flip enq) q zs 


-- | Add all elements of the given list to the 'Queue' after applying a
-- function to them.
-- /The order in which the elements are added is not defined./
enqListWith :: (b -> a) -> [b] -> Queue a -> Queue a
enqListWith f zs (Queue xs ys) = Queue xs (foldr ((:) . f) ys zs)


-- | Convert a list to a 'Queue', such that
-- @deq (fromList (x : xs)) == (x, fromList xs)@.
-- Additionally, @toList (fromList xs) == xs@.
fromList :: [a] -> Queue a
fromList xs = Queue xs []


-- | Convert a 'Queue' to a List, such that
-- @toList q == x : xs@ iff @deq q == (x, fromList xs)@.
-- Additionally, @fromList (toList q) == q@.
toList :: Queue a -> [a]
toList (Queue xs ys) = xs ++ reverse ys
