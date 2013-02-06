-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Util
-- Copyright   :  (c) Technische Universität Dresden 2012
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Matthias.Buechse@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Functions for strictness and STRef helpers
--
-----------------------------------------------------------------------------

module Vanda.Util
  ( module Data.STRef
  , first'
  , second'
  , modifySTRef'
  , viewSTRef'
  , lookupSTRef'
  , lviewSTRef'
  , readSTRefWith
  , pairM
  , seqEither
  , seqMaybe
  ) where

import Control.Monad.ST
import Control.Seq
import Data.STRef ( STRef, newSTRef, readSTRef, writeSTRef, modifySTRef )

first' :: (a -> b) -> (a, c) -> (b, c)
first' f p = case p of
               (x, y) -> let fx = f x in fx `seq` (fx, y)

second' :: (a -> b) -> (c, a) -> (c, b)
second' f p = case p of
               (x, y) -> let fy = f y in fy `seq` (x, fy)

-- | Strict version of 'modifySTRef' 
modifySTRef' :: STRef s a -> (a -> a) -> ST s () 
modifySTRef' ref f = readSTRef ref >>= (\ x -> writeSTRef ref $! f x)


viewSTRef'
  :: STRef s a -> (a -> Maybe (b, a)) -> ST s c -> (b -> ST s c) -> ST s c
viewSTRef' ref f n j = do 
  x <- readSTRef ref 
  case f x of
    Nothing -> n
    Just (y, x') -> x' `seq` writeSTRef ref x' >> j y


lookupSTRef'
  :: STRef s a -> (a -> Maybe b) -> ST s c -> (b -> ST s c) -> ST s c
lookupSTRef' ref f n j = do 
  x <- readSTRef ref 
  case f x of
    Nothing -> n
    Just y -> j y


lviewSTRef'
  :: STRef s [a] -> ST s b -> (a -> ST s b) -> ST s b
lviewSTRef' ref n j = do 
  x <- readSTRef ref 
  case x of
    [] -> n
    y : x' -> writeSTRef ref x' >> j y


readSTRefWith :: (a -> b) -> STRef s a -> ST s b
readSTRefWith f s = readSTRef s >>= (return . f)


pairM :: Monad m => (m a, m b) -> m (a, b)
pairM (x1, x2) = do { y1 <- x1; y2 <- x2; return (y1, y2) }



seqEither :: Strategy a -> Strategy b -> Strategy (Either a b)
seqEither sa _ (Left a)  = sa a `seq` ()
seqEither _ sb (Right b) = sb b `seq` ()


seqMaybe :: Strategy a -> Strategy (Maybe a)
seqMaybe _ Nothing = ()
seqMaybe s (Just a) = s a `seq` ()

