{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vanda.Grammar.XRS.Binary where

import Control.Applicative ( (<$>), (<*>) )
import Control.DeepSeq ( NFData )
import qualified Data.Binary as B
import qualified Data.Vector as V

import Vanda.Grammar.XRS.IRTG
import Vanda.Hypergraph.IntBinary ()

instance B.Binary StrictIntPair where
  get = SIP <$> B.get <*> B.get 
  put (SIP a b) = B.put a >> B.put b

instance B.Binary NTT where
  get = B.getWord8 >>= \ x ->
        case x of
          0 -> NT <$> B.get
          1 -> T <$> B.get
          _ -> error "corrupt NTT data stream"
  put (NT x) = B.putWord8 0 >> B.put x
  put (T x) = B.putWord8 1 >> B.put x

instance (B.Binary i, NFData i, NFData StrictIntPair)
  => B.Binary (IRTG i) where
  get = do
          rtg <- B.get
          h1 <- fmap V.fromList B.get
          h2 <- fmap V.fromList B.get
          return $! IRTG { .. }
  put IRTG{ .. } = B.put rtg >> B.put (V.toList h1) >> B.put (V.toList h2)
