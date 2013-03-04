{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Vanda.Grammar.XRS.Binary where

import Control.Applicative ( (<$>), (<*>) )
import Control.DeepSeq ( NFData )
import qualified Data.Binary as B
import Data.NTTBinary ()
import qualified Data.Vector as V

import Vanda.Grammar.XRS.IRTG
import Vanda.Hypergraph.IntBinary ()

instance NFData StrictIntPair

instance B.Binary StrictIntPair where
  get = SIP <$> B.get <*> B.get 
  put (SIP a b) = B.put a >> B.put b

instance (B.Binary i, NFData i) => B.Binary (IRTG i) where
  get = do
          !rtg <- B.get
          !initial <- B.get
          !h1 <- fmap V.fromList B.get
          !h2 <- fmap (V.fromList . map V.fromList) B.get
          return $! IRTG { .. }
  put IRTG{ .. } = B.put rtg >> B.put initial >> B.put (V.toList h1) >> B.put (map V.toList $ V.toList h2)
