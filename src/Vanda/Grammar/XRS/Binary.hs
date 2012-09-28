{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vanda.Grammar.XRS.Binary where

import Control.Applicative ( (<$>), (<*>) )
import Control.DeepSeq ( NFData )
import qualified Data.Binary as B
import Data.NTT
import qualified Data.Vector as V

import Vanda.Grammar.XRS.IRTG
import Vanda.Hypergraph.IntBinary ()

instance B.Binary StrictIntPair where
  get = SIP <$> B.get <*> B.get 
  put (SIP a b) = B.put a >> B.put b

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

instance (B.Binary i, NFData i, NFData StrictIntPair)
  => B.Binary (IRTG i) where
  get = do
          rtg <- B.get
          h1 <- rtg `seq` fmap V.fromList B.get
          h2 <- h1 `seq` fmap V.fromList B.get
          h2 `seq` return $! IRTG { .. }
  put IRTG{ .. } = B.put rtg >> B.put (V.toList h1) >> B.put (V.toList h2)
