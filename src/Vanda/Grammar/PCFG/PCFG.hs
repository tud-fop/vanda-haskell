module Vanda.Grammar.PCFG.PCFG where

import Vanda.Hypergraph
import qualified Data.Vector as V
import Data.Either

data PCFG nonterminalType terminalType = 
  PCFG 
  { productions :: EdgeList nonterminalType [Either Int terminalType] Int
  , startsymbols :: [(nonterminalType,Double)]
  , weights :: V.Vector Double
  }   
