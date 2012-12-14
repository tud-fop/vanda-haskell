{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vanda.Grammar.XRS.Binary where

import Control.Applicative ( (<$>), (<*>) )
import Control.DeepSeq ( NFData )
import qualified Data.Binary as B
import Data.NTT
import Data.NTTBinary ()
import qualified Data.Vector as V

import Vanda.Grammar.XRS.IRTG
import Vanda.Hypergraph.IntBinary ()

instance B.Binary StrictIntPair where
  get = SIP <$> B.get <*> B.get 
  put (SIP a b) = B.put a >> B.put b

instance (B.Binary i, NFData i, NFData StrictIntPair)
  => B.Binary (IRTG i) where
  get = do
          rtg <- B.get
          initial <- rtg `seq` B.get
          h1 <- initial `seq` fmap V.fromList B.get
          h2 <- h1 `seq` fmap V.fromList B.get
          h2 `seq` return $! IRTG { .. }
  put IRTG{ .. } = B.put rtg >> B.put initial >> B.put (V.toList h1) >> B.put (V.toList h2)
