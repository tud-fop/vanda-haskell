module Vanda.Grammar.PCFG.PCFG where

import Vanda.Hypergraph
import qualified Data.Vector as V
import Data.Either
import Data.Tree

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
          
data Deriv a b 
  = DNode a [Deriv a b] | DLeaf b deriving Show
  
root :: Deriv a a -> a
root (DLeaf x) = x
root (DNode x _) = x

derivToTree :: Deriv a a -> Tree a
derivToTree (DLeaf x) = Node x []
derivToTree (DNode x l) = Node x (map derivToTree l)

treeToDeriv :: Tree a -> Deriv a a
treeToDeriv (Node x li) 
  | length li == 0 = DLeaf x
  | otherwise      = DNode x (map treeToDeriv li)