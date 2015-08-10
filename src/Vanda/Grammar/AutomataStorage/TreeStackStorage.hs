{-|
Module:      Vanda.Grammar.AutomataStorage.TreeStackStorage
Description: storage type for /tree stacks/
Copyright:   Ⓒ Toni Dietze and Tobias Denkinger, 2015
Maintainer:  Tobias.Denkinger@tu-dresden.de
Stability:   experimental

This module contains a data structure for the nondeterministic storage type /tree stack/ together with some predicates and functions on tree stacks.
-}
module Vanda.Grammar.AutomataStorage.TreeStackStorage
  ( TreeStack
-- * construction
  , emptyTreeStack
-- * predicates
  , checkTreeStack
  , bottomTreeStack
-- * functions
  , stayTreeStack
  , pushTreeStack
  , popTreeStack
  , upTreeStack
  , downTreeStack
  ) where


import Data.Tree (Tree (Node), subForest)


-- | 'Tree' plus stack pointer; the bottom of the stack is the root of the tree.
--   The data structure has the form [(cₙ, tₙ), …, (c₁, t₁)] where cᵢ are, intuitively,
--   contexts and tᵢ are trees. The tree stack can be obtaines from that data structure
--   as follows: the tree is cₙ . … . c₂ $ c₁ t₁ and the pointer points to the root of
--   t₁. The data structure optimizes the expensive operation of "going to a specific
--   position in the tree".
newtype TreeStack a = TreeStack [(Tree a -> Tree a, Tree a)]


-- | Creates an empty 'TreeStack', i.e. a stack tree that only has a root node.
emptyTreeStack :: a -> TreeStack a
emptyTreeStack x = TreeStack [(id, Node x [])]


-- | Checks whether the node at the stack pointer fulfills a certain predicate.
checkTreeStack :: (a -> Bool) -> TreeStack a -> Bool
checkTreeStack _ (TreeStack [])
  = error "checkTreeStack: the stack should never be empty"
checkTreeStack p (TreeStack ((_, Node x _) : _))
  = p x


-- | Checks whether the tree only has a root node.
bottomTreeStack :: TreeStack a -> Bool
bottomTreeStack (TreeStack [(f, ξ)])
  | null . subForest $ f ξ = True
  | otherwise              = False
bottomTreeStack _          = False


-- | Adds the given stack symbol above the current stack pointer.
pushTreeStack :: a -> TreeStack a -> [TreeStack a]
pushTreeStack _ (TreeStack [])
  = error "pushTreeStack: the stack should never be empty"
pushTreeStack x (TreeStack cs@((_, Node a ts) : _))
  = [ TreeStack $ (\ t' -> Node a (t' : ts), Node x []) : cs ]


-- | Removes the node of the tree currently under the stack pointer (if that tree is a
--   leaf) and moves the stack pointer to the parent of its previous position.
popTreeStack :: TreeStack a -> [TreeStack a]
popTreeStack (TreeStack [])
  = error "popTreeStack: the stack should never be empty"
popTreeStack (TreeStack ((_, Node _ []) : cs))
  = [TreeStack cs]
popTreeStack _
  = []


-- | Moves the stack pointer to the parent node of its current position.
downTreeStack :: TreeStack a -> [TreeStack a]
downTreeStack (TreeStack [])
  = error "downTreeStack: the stack should never be empty"
downTreeStack (TreeStack [_])
  = []
downTreeStack (TreeStack ((f0, t0) : (f1, _) : ts))
  = [ TreeStack $ (f1, f0 t0) : ts ]


-- | (Nondeterministically) moves the stack pointer to the child nodes.
upTreeStack :: TreeStack a -> [TreeStack a]
upTreeStack (TreeStack [])
  = error "upTreeStack: the stack should never be empty"
upTreeStack (TreeStack ((f, Node a ts) : cs))
  = [ TreeStack $ (Node a . g, t) : (f, Node a ts') : cs | (g, t, ts') <- contexts ts ]

contexts :: [a] -> [(a -> [a], a, [a])]
contexts []
  = []
contexts [x]
  = return (return, x, [])
contexts (x : xs)
  = ((: xs), x, xs) : map (\ (f, y, ys) -> ((x :) . f, y, x:ys)) (contexts xs)


-- | Applies a function to the node below the stack pointer.
stayTreeStack :: (a -> [a]) -> TreeStack a -> [TreeStack a]
stayTreeStack _ (TreeStack [])
  = error "stayTreeStack: the stack should never be empty"
stayTreeStack g (TreeStack ((f, Node a ts) : cs))
  = [ TreeStack $ (f, Node a' ts) : cs | a' <- g a ]
