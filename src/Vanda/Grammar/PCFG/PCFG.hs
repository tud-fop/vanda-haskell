{-|
Module:      Vanda.Grammar.PCFG.PCFG
Description: data structures of /PCFG/
Copyright:   (c) Technische Universität Dresden 2015
License:     Redistribution and use in source and binary forms, with
             or without modification, is ONLY permitted for teaching
             purposes at Technische Universität Dresden AND IN
             COORDINATION with the Chair of Foundations of Programming.
Maintainer:  markus.napierkowski@mailbox.tu-dresden.de
Stability:   unknown

This module contains data structures used for PCFGs and a 
few simple functions.
-}

module Vanda.Grammar.PCFG.PCFG where


import           Vanda.Hypergraph
import           Data.Tree
import qualified Data.Vector as V



data PCFG nonterminalType terminalType = 
  PCFG 
  { productions :: EdgeList nonterminalType [Either Int terminalType] Int
  , startsymbols :: [(nonterminalType,Double)]
  , weights :: V.Vector Double
  }   

-- | Partition the identifiers of a list of hyperedges by their 'to' node
partition :: Eq a => [Hyperedge a c Int] -> [(a,[Int])]
partition [] = []
partition ((he):rest0) = insert (ident he) (to he) (partition rest0)
  where insert :: Eq a => Int -> a -> [(a,[Int])] -> [(a,[Int])]
        insert i s [] = [(s,[i])]
        insert i s ((s',l):rest) 
          | s == s' = (s',i:l):rest
          | otherwise = (s',l):(insert i s rest)
          
data Deriv a b 
  = DNode a [Deriv a b] | DLeaf b
  
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