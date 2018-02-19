{-|
Module:      Vanda.Grammar.AutomataStorage.SparseTreeStackStorage
Description: storage type for /sparse tree stacks/
Copyright:   Ⓒ Toni Dietze and Tobias Denkinger, 2016
License:     BSD-style
Maintainer:  Tobias.Denkinger@tu-dresden.de
Stability:   experimental

This module contains a data structure for the nondeterministic storage type
/sparse tree stack/ together with some predicates and functions on it.
-}
module Vanda.Grammar.AutomataStorage.SparseTreeStackStorage
  ( SparseTree (label, children)
  , TreeStack
-- * construction
  , emptyTreeStack
-- * conversion
  , fromTreeStack
  , fromSparseTree
-- * predicates
  , bottomTreeStack
  , checkTreeStack
-- * functions
  , stayTreeStack
  , pushTreeStack
  , popTreeStack
  , upTreeStack
  , downTreeStack
-- * pretty printing
  , prettyPrintSparseTree
  , prettyPrintTreeStack
-- * examples
  , exampleTreeStack
  , exampleSparseTree
  ) where

import Prelude hiding (null, filter, lookup)
import Data.List (intercalate)
import Data.Map (Map (), (!), assocs, elems, empty, filter, insert, keys, lookup, notMember, null, unions)
import Data.Maybe (maybeToList)
import Data.Tree (Tree (Node))


data SparseTree i a = SparseTree { label :: a, children :: Map i (SparseTree i a) }
  deriving (Eq, Show)


instance Functor (SparseTree i) where
  fmap f (SparseTree a cs) = SparseTree (f a) $ fmap (fmap f) cs


subtrees :: (Ord i) => SparseTree i a -> Map [i] (SparseTree i a)
subtrees = subtrees' []

subtrees' :: (Ord i) => [i] -> SparseTree i a -> Map [i] (SparseTree i a)
subtrees' is t = insert is t
               . unions
               . map (\ (i, t') -> subtrees' (is ++ [i]) t')
               . assocs
               $ children t


fromSparseTree :: (Ord i) => SparseTree i a -> Tree a
fromSparseTree (SparseTree x tm) = Node x . map fromSparseTree $ elems tm


newtype TreeStack i a = TreeStack [(SparseTree i a -> SparseTree i a, SparseTree i a)]


-- | Creates an empty 'TreeStack', i.e. one that only has a root node.
emptyTreeStack :: a -> TreeStack i a
emptyTreeStack x = TreeStack [(id, SparseTree x empty)]


-- | 'True' if the current node is the root,
--   'False' otherwise.
bottomTreeStack :: TreeStack i a -> Bool
bottomTreeStack (TreeStack [])
  = error "checkTreeStack: the stack backlog must not be empty"
bottomTreeStack (TreeStack [(f, ξ)])
  | null . children $ f ξ = True
  | otherwise             = False
bottomTreeStack _         = False


-- | 'True' if the current node fulfills the given predicate,
--   'False' otherwise.
checkTreeStack :: (a -> Bool) -> TreeStack i a -> Bool
checkTreeStack _ (TreeStack [])
  = error "checkTreeStack: the stack backlog must not be empty"
checkTreeStack p ts@(TreeStack ((_, SparseTree x _) : _))
  = not (bottomTreeStack ts) && p x


-- | Applies a given function to the current node label.
stayTreeStack :: (a -> [a]) -> TreeStack i a -> [TreeStack i a]
stayTreeStack _ (TreeStack [])
  = error "stayTreeStack: the stack backlog must not be empty"
stayTreeStack g (TreeStack ((f, SparseTree a tm) : cs))
  = [ TreeStack $ (f, SparseTree a' tm) : cs | a' <- g a ]


-- | If the current node has no i-th child yet, this function adds the given
--   stack symbol as the i-th child of the current node.
pushTreeStack :: Ord i => i -> a -> TreeStack i a -> [TreeStack i a]
pushTreeStack _ _ (TreeStack [])
  = error "pushTreeStack: the stack backlog must not be empty"
pushTreeStack i x (TreeStack cs@((_, SparseTree a tm) : _))
  = [ TreeStack $ (\ t' -> SparseTree a (insert i t' tm), SparseTree x empty) : cs
    | i `notMember` tm
    ]


-- | If the current node is a leaf and not the root, then this function
--   removes the subtree at the current position and moves the stack pointer
--   to the parent of current position.
popTreeStack :: TreeStack i a -> [TreeStack i a]
popTreeStack (TreeStack [])
  = error "popTreeStack: the stack backlog must not be empty"
popTreeStack (TreeStack ((_, SparseTree _ tm) : cs))
  | null tm   = [TreeStack cs]
  | otherwise = []


-- | Moves the stack pointer to the i-th child node, if it is defined.
upTreeStack :: Ord i => i -> TreeStack i a -> [TreeStack i a]
upTreeStack _ (TreeStack [])
  = error "upTreeStack: the stack backlog must not be empty"
upTreeStack i (TreeStack ((f, SparseTree a tm) : cs))
  = [ TreeStack $ (\ t' -> SparseTree a (insert i t' tm), t) : (f, SparseTree a tm) : cs
    | t <- maybeToList $ i `lookup` tm
    ]


-- | Moves the stack pointer to the parent of its current position, if possible.
downTreeStack :: TreeStack i a -> [TreeStack i a]
downTreeStack (TreeStack [])
  = error "downTreeStack: the stack backlog must not be empty"
downTreeStack (TreeStack [_])
  = []
downTreeStack (TreeStack ((f0, t0) : (f1, _) : ts))
  = [ TreeStack $ (f1, f0 t0) : ts ]


-- | An instance of 'TreeStack' 'Int' 'Int' that corresponds to the tree
--   @{[]\/-1, [5]\/3, [5,1]\/2, [42]\/5}@ with stack pointer @[5,1]@.
exampleTreeStack :: TreeStack Int Int
exampleTreeStack = head
                 (pushTreeStack 42 5 (emptyTreeStack (-1))
                 >>= downTreeStack
                 >>= pushTreeStack 5 3
                 >>= pushTreeStack 1 2 )

-- | An instance of 'SparseTree' 'Int' 'Int' that corresponds to
--   @{[]\/-1, [5]\/3, [5,1]\/2, [42]\/5}@.
exampleSparseTree :: SparseTree Int Int
exampleSparseTree = fromTreeStack exampleTreeStack


fromTreeStack :: TreeStack i a -> SparseTree i a
fromTreeStack (TreeStack [])
  = error "fromTreeStack: the stack backlog must not be empty"
fromTreeStack (TreeStack ((c, t) : xs))
  = foldr (\ (c', _) t' -> c' t') (c t) xs


-- TODO: find a way not to make this dirty hack!
prettyPrintTreeStack :: (Eq a, Ord i, Show a, Show i) => TreeStack i a -> String
prettyPrintTreeStack (TreeStack [])
  = error "prettyPrintTreeStack: the stack backlog must not be empty"
prettyPrintTreeStack ts@(TreeStack ((_, t) : _))
  = show address
  ++ " @ "
  ++ prettyPrintSparseTree (fromTreeStack ts)
  where
    address = fst . head . assocs . filter (== t) . subtrees $ fromTreeStack ts


prettyPrintSparseTree :: (Ord i, Show a, Show i) => SparseTree i a -> String
prettyPrintSparseTree t
  = "{" ++ intercalate ", " (prettyPrintSparseTree' [] t) ++ "}" where
  prettyPrintSparseTree' is (SparseTree a tm)
    = (show is ++ "/" ++ show a)
    : concat [prettyPrintSparseTree' (is ++ [i]) (tm ! i) | i <- keys tm]
