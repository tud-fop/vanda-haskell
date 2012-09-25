module Vanda.Grammar.XRS.IRTG where

import qualified Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Vanda.Hypergraph.IntHypergraph


data NTT = NT !Int | T !Int deriving (Eq, Ord, Show)

data StrictIntPair = SIP !Int !Int deriving (Eq, Ord, Show)

data IRTG i
  = IRTG
    { rtg :: Hypergraph StrictIntPair i
    , h1 :: V.Vector (T.Tree NTT)
    , h2 :: V.Vector [NTT]
    }

data XRS
  = XRS
    { irtg :: IRTG Int
    , weights :: VU.Vector Double
    }

