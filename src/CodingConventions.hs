-----------------------------------------------------------------------------
-- |
-- Module      :  CodingConventions
-- Copyright   :  (c) Technische Universität Dresden 2015
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- This module does not export any functionality. It merely serves for
-- documenting the coding conventions of this project. The module itself
-- follows these conventions.
--
-- A module shall consists of the following parts in exactly that order,
-- some of them are optional:
--
-- * (required) a
--   [Haddock module description](https://www.haskell.org/haddock/doc/html/ch03s03.html)
--   formatted as in this module,
-- * (required) haddock documentation for the module,
-- * (optional)
--   [file-header pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#pragmas);
--   alphabetically ordered; at most one pragma per line; as soon as you need
--   a line break, split up the pragma into several ones,
-- * (required) @module@ keyword and module name,
-- * (recommended) list of exports, putting every export on a separate line,
-- * (required) @where@,
-- * (optional) import 'Prelude', e.g. for hiding some functions,
-- * (optional) import any external modules,
-- * (optional) import any modules from the project itself,
-- * (optional) actual implementation.
--
-- Spacing and line breaking:
--
-- * Any line shall be at most 78 characters long (see “How to get short
--   lines” below).
-- * Do /not/ use tab stops. (Check the configuration of your editor.)
-- * An indentation step shall be two spaces wide.
-- * Put an empty line between the different import blocks mentioned above.
-- * Put two empty lines between top level definitions.
-- * Put spaces around infix operators.
-- * Put spaces after commas.
-- * Put spaces before opening and after closing
--   braces\/brackets\/parentheses.
-- * Put infix operators (including @=@, @,@, @|@, @..@, @::@, @->@, @=>@) on
--   the new line if you have to break the current line.
-- * Try to put matching braces\/brackets\/parentheses in the same column if
--   they are on different lines.
--
-- Other style related issues:
--
-- * Every top level function and value must be explicitly typed.
-- * Every exported function, value, and type must be documented using
--   [haddock syntax](https://www.haskell.org/haddock/doc/html/index.html).
-- * Hyperlink
--   [identifiers](https://www.haskell.org/haddock/doc/html/ch03s08.html#idm140354810770608)
--   and
--   [modules](https://www.haskell.org/haddock/doc/html/ch03s08.html#idm140354810751008)
--   whenever possible.
--
-- Other conventions:
--
-- * If you implement a comand line utility, follow the suggestions in
--   "Vanda.Main".
-- * Fix all warnings that ghc produces with activated @-Wall@ flag.
-- * Check your code with [HLint](https://hackage.haskell.org/package/hlint),
--   but do not blindly apply every suggestion.
--
-- How to get short lines:
--
-- * Recall: “A
--   [string](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6)
--   may include a ‘gap’—two backslants enclosing white characters—which is
--   ignored.”
-- * Start new lines directly after @where@, @let@, @do@, or @of@ keywords,
--   cf. the
--   [off-side rule](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-210002.7).
-- * Line breaking types: If you have to break at one “@->@”, break at every
--   (top-level) “@->@”.
--
--     @
--     insideOutside' :: (Ord v, Num w) => (w -> w -> Bool) -> v -> HyperGraph v l w -> M.Map v (w, w)
--
--     insideOutside'
--       :: (Ord v, Num w) => (w -> w -> Bool) -> v -> HyperGraph v l w -> M.Map v (w, w)
--
--     insideOutside'
--       :: (Ord v, Num w)
--       => (w -> w -> Bool) -> v -> HyperGraph v l w -> M.Map v (w, w)
--
--     insideOutside'
--       :: (Ord v, Num w)
--       => (w -> w -> Bool)
--       -> v
--       -> HyperGraph v l w
--       -> M.Map v (w, w)
--     @
--
-- * Line breaking list comprehensions: If you have to break at one “@,@”,
--   break at every “@,@”.
--
--     @
--     [(x, y, z) | x <- [0 .. 9], y <- [0 .. 9], z <- [0 .. 9]]
--
--     [ (x, y, z)
--     | x <- [0 .. 9], y <- [0 .. 9], z <- [0 .. 9]
--     ]
--
--     [ (x, y, z)
--     | x <- [0 .. 9]
--     , y <- [0 .. 9]
--     , z <- [0 .. 9]
--     ]
--     @
-----------------------------------------------------------------------------

-- Since the code is just an example, we do not export anything.
-- Therefore, we disable the warning for unused bindings.
-- Nevertheless, we want to generate the documentation.
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module CodingConventions () where


import Prelude hiding ()

import qualified Data.Tree as T

import qualified Control.Error


-- | Call 'error' with a useful message.
--
-- It is recommended to define an errorHere function in any module that would
-- otherwise call 'error' directly.
--
-- Always put this function at the top, so the module name can easily be
-- spotted and changed if needed.
errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "CodingConventions"



-- | A data type for binary trees that may be empty ('Null').
data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Null


-- * Search Trees

-- | Insert a value in a 'BinaryTree' that has the search tree property, if
-- the value is not already contained in the tree. The result also has the
-- search tree property.
--
-- If the input tree does not have the search tree property, nothing is
-- guaranteed for the result.
--
-- A 'BinaryTree' is a search tree, if every entry in the left sub-tree of a
-- node is smaller then the node entry, and every entry in the right sub-tree
-- of a node is larger than the node entry.
insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x t@(Node y t1 t2)
  = case compare x y of
      LT -> Node y (insert x t1) t2
      EQ -> t
      GT -> Node y t1 (insert x t2)
insert x Null = Node x Null Null


-- * Conversion

-- | Convert a 'BinaryTree' to a 'T.Tree'.
--
-- Note that the information if a sub-tree was the left or the right child of
-- its parent is lost if it is the only child.
--
-- The function calls 'error' for input 'Null'.
toTree :: BinaryTree a -> T.Tree a
toTree (Node x Null Null) = T.Node x []
toTree (Node x t    Null) = T.Node x [toTree t]
toTree (Node x Null t   ) = T.Node x [toTree t]
toTree (Node x t1   t2  ) = T.Node x [toTree t1, toTree t2]
toTree Null = errorHere "totree" "Cannot convert Null."


-- | Convert a binary 'T.Tree' to a 'BinaryTree'.
--
-- If a sub-tree is the only child of its parent, it gets the left child.
--
-- The function calls 'error' at 'T.Node's with more than two sub-trees.
fromTree :: T.Tree a -> BinaryTree a
fromTree (T.Node x [      ]) = Node x Null          Null
fromTree (T.Node x [t     ]) = Node x (fromTree t ) Null
fromTree (T.Node x [t1, t2]) = Node x (fromTree t1) (fromTree t2)
fromTree _ = errorHere "fromTree" "Tree has Node with more than two children."
