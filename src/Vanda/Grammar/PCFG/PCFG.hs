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

partition :: Eq a => [Hyperedge a c Int] -> [(a,[Int])]
partition [] = []
partition ((he):rest) = insert (ident he) (to he) (partition rest)
  where insert :: Eq a => Int -> a -> [(a,[Int])] -> [(a,[Int])]
        insert i s [] = [(s,[i])]
        insert i s ((s',l):rest) 
          | s == s' = (s',i:l):rest
          | otherwise = (s',l):(insert i s rest)