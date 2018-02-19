-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Technische Universität Dresden 2018
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Grammar.Berkeley.IRTG where

import Data.NTT
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Vanda.Hypergraph.IntHypergraph
import qualified Vanda.Hypergraph.Tree as T

data IRTG i
  = IRTG
    { rtg :: Hypergraph Int i
    , initial :: Int
    , h1 :: V.Vector (T.Tree NTT)
    }

-- TODO make substates explicit

data BerkeleyGrammar
  = BerkeleyGrammar
    { irtg :: IRTG Int
    , weights :: VU.Vector Double
    }


