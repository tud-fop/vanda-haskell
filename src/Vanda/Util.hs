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
  ( first'
  , second'
  , modifySTRef'
  , viewSTRef'
  , lookupSTRef'
  , lviewSTRef'
  , readSTRefWith
  , seqMaybe
  ) where

import Control.Monad.ST
import Control.Seq
import Data.STRef

first' :: (a -> b) -> (a, c) -> (b, c)
first' f p = case p of
               (x, y) -> let fx = f x in fx `seq` (fx, y)

second' :: (a -> b) -> (c, a) -> (c, b)
second' f p = case p of
               (x, y) -> let fy = f y in fy `seq` (x, fy)

-- | Strict version of 'modifySTRef' 
modifySTRef' :: STRef s a -> (a -> a) -> ST s () 
modifySTRef' ref f = do 
  x <- readSTRef ref 
  let x' = f x 
  x' `seq` writeSTRef ref x' 


viewSTRef'
  :: STRef s a -> (a -> Maybe (b, a)) -> ST s c -> (b -> ST s c) -> ST s c
viewSTRef' ref f n j = do 
  x <- readSTRef ref 
  case f x of
    Nothing -> n
    Just (y, x') -> x' `seq` writeSTRef ref x' >> j y


lookupSTRef'
  :: STRef s a -> (a -> Maybe b) -> ST s () -> (b -> ST s ()) -> ST s ()
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


seqMaybe :: Strategy a -> Strategy (Maybe a)
seqMaybe _ Nothing = ()
seqMaybe s (Just a) = s a `seq` ()

