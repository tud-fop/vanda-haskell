{-# LANGUAGE BangPatterns, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Interner
-- Copyright   :  (c) Technische Universität Dresden 2013
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Tobias.Denkinger@mailbox.tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Maps a values to 'Int's savely while remembering old mappings.
--
-----------------------------------------------------------------------------

module Data.Interner
  ( Interner (..)
  , emptyInterner
  , internerToArray
  , intern
  , internList
  , internListPreserveOrder
  , internST
  ) where

import Control.Monad.ST
import qualified Data.Array as A
import Data.Hashable ( Hashable )
import qualified Data.HashMap.Strict as HM

import Vanda.Util

data Interner t
  = Interner
    { inMap :: HM.HashMap t Int
    , inSize :: Int
    }


emptyInterner :: Interner t
emptyInterner = Interner{ inMap = HM.empty, inSize = 0 }


internerToArray :: Interner t -> A.Array Int t
internerToArray Interner{ .. }
  = A.array (0, inSize - 1) $ map swap $ HM.toList inMap

intern :: (Hashable t, Eq t) => Interner t -> t -> (Interner t, Int)
intern orig@Interner{ .. } v
  = case HM.lookup v inMap of
      Nothing -> let i = inSize
                 in ( orig{ inMap = HM.insert v i inMap
                          , inSize = i + 1
                          }
                    , i
                    )
      Just i -> (orig, i)

-- | Uses interner on a 'List' ignoring (aka reversing) its order.
internList :: (Hashable t, Eq t) => Interner t -> [t] -> (Interner t, [Int])
internList im ks
  = let f (!m, xs) k = (m', x:xs) where (m', x) = intern m k
    in  foldl f (im, []) ks

-- | Uses interner on a 'List' but preserves the 'List's order.
internListPreserveOrder :: (Hashable t, Eq t) => Interner t -> [t] -> (Interner t, [Int])
internListPreserveOrder im ks
  = (i, reverse lst) where (i, lst) = internList im ks

internST :: (Hashable t, Eq t) => STRef s (Interner t) -> t -> ST s Int
internST _in v = flip (lookupSTRef' _in (HM.lookup v . inMap)) return $ do
  i <- fmap inSize $ readSTRef _in
  modifySTRef' _in
    $ \ __in@Interner{ .. } -> __in{ inMap = HM.insert v i inMap
                                   , inSize = i + 1
                                   }
  return i
