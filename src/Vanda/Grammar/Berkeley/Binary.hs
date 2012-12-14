{-# LANGUAGE RecordWildCards #-}
module Vanda.Grammar.Berkeley.Binary () where

import Control.DeepSeq ( NFData )
import qualified Data.Binary as B
import Data.NTT
import Data.NTTBinary ()
import qualified Data.Vector as V

import Vanda.Grammar.Berkeley.IRTG
import Vanda.Hypergraph.IntBinary ()

instance (B.Binary i, NFData i) => B.Binary (IRTG i) where
  get = do
          rtg <- B.get
          initial <- rtg `seq` B.get
          h1 <- initial `seq` fmap V.fromList B.get
          h1 `seq` return $! IRTG { .. }
  put IRTG{ .. } = B.put rtg >> B.put initial >> B.put (V.toList h1)
