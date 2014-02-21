{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.NTTBinary () where

import qualified Data.Binary as B
import Data.NTT

instance B.Binary NTT where
  {- get = B.getWord8 >>= \ x ->
        case x of
          0 -> NT <$> B.get
          1 -> T <$> B.get
          _ -> error "corrupt NTT data stream" -}
  get = do
          x <- B.getWord8
          y <- B.get :: B.Get Int
          return $! case x of { 0 -> nt y ; 1 -> tt y }
  put (NT x) = B.putWord8 0 >> B.put x
  put (T x) = B.putWord8 1 >> B.put x

