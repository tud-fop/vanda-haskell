-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Util.Tree
-- Copyright   :  (c) Technische Universität Dresden 2014
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Some useful functions for 'Tree's in addition to those from "Data.Tree".
--
-----------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Vanda.Util.Tree
( -- * Two-dimensional drawing
  Drawstyle(..)
, drawstyleCompact1
, drawstyleCompact2
, drawTree'
, drawForest'
, -- * Extraction
  flattenRanked
, subTrees
, yield
, -- * Manipulation
  defoliate
, -- * Map
  mapLeafs, mapInners, mapInnersAndLeafs, mapAccumLLeafs, zipLeafsWith
) where


import Control.Arrow (second)
import Data.Functor
import Data.List (mapAccumL)
import Data.Tree


errorModule :: String -> a
errorModule = error . ("Vanda.Util.Tree." ++)


-- Two-dimensional drawing ---------------------------------------------------

-- | Defines how the branching structure of 'drawTree'' and 'drawForest''
-- looks like.
data Drawstyle = Drawstyle
  { br :: String  -- ^ branch at root
  , bi :: String  -- ^ branch at inner sibling
  , bl :: String  -- ^ branch at last sibling
  , lr :: String  -- ^ leaf at root
  , li :: String  -- ^ leaf at inner sibling
  , ll :: String  -- ^ leaf as last sibling
  , fi :: String  -- ^ filler for children of inner sibling
  , fl :: String  -- ^ filler for children of last sibling
  }


-- | Compact 'Drawstyle' similar to style of 'drawTree'.
drawstyleCompact1
  :: String     -- ^ separates the node label from the tree strtucture
  -> Drawstyle
drawstyleCompact1 sep = Drawstyle
  { br = ""
  , bi = "├" ++ sep
  , bl = "├" ++ sep
  , lr = ""
  , li = "└" ++ sep
  , ll = "└" ++ sep
  , fi = "│" ++ space
  , fl = " " ++ space
  }
  where space = ' ' <$ sep


-- | Compact 'Drawstyle' where the lines of the tree structure are completely
-- connected.
drawstyleCompact2
  :: Int        -- ^ width of the branches
  -> String     -- ^ separates the node label from the tree strtucture
  -> Drawstyle
drawstyleCompact2 width sep = Drawstyle
  { br = "┌" ++ sep
  , bi = "├" ++ line ++ "┬" ++ sep
  , bl = "├" ++ line ++ "─" ++ sep
  , lr = "╶" ++ sep
  , li = "└" ++ line ++ "┬" ++ sep
  , ll = "└" ++ line ++ "─" ++ sep
  , fi = "│" ++ space
  , fl = " " ++ space
  }
  where line  = replicate width '─'
        space = replicate width ' '



-- | Neat 2-dimensional drawing of a tree based on a 'Drawstyle'.
drawTree' :: Drawstyle -> Tree String -> String
drawTree'  = (unlines .) . draw

-- | Neat 2-dimensional drawing of a forest on a 'Drawstyle'.
drawForest' :: Drawstyle -> Forest String -> String
drawForest' drawstyle = unlines . map (drawTree' drawstyle)

draw :: Drawstyle -> Tree String -> [String]
draw Drawstyle{ .. } (Node root ts0)
  = ((if null ts0 then lr else br) ++ root) : drawSubTrees ts0
  where
    draw' (Node x ts) = x : drawSubTrees ts

    drawSubTrees []
      = []
    drawSubTrees [t]
      = shift (if null (subForest t) then ll else li) fl (draw' t)
    drawSubTrees (t : ts)
      = shift (if null (subForest t) then bl else bi) fi (draw' t)
      ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)


-- Extraction ----------------------------------------------------------------

-- | The elements of a 'Tree' and the 'length' of their 'subForest's,
-- respectively, in pre-order.
flattenRanked :: Tree a -> [(a, Int)]
flattenRanked (Node x ts) = (x, length ts) : concatMap flattenRanked ts


-- | List of all subtrees in pre-order.
subTrees :: Tree a -> Forest a
subTrees t = t : concatMap subTrees (subForest t)


-- | List of leaves from left to right.
yield :: Tree a -> [a]
yield (Node x []) = [x]
yield (Node _ xs) = concatMap yield xs


-- Manipulation --------------------------------------------------------------

-- | Removes the leaves of a 'T.Tree'.
defoliate :: Tree a -> Tree a
defoliate (Node _ []) = errorModule "defoliate: Tree has only one leaf."
defoliate (Node x xs)
  = Node x $ map defoliate $ filter (not . null . subForest) xs


-- Map -----------------------------------------------------------------------

-- | Apply a funtion to all leaves.
mapLeafs :: (a -> a) -> Tree a -> Tree a
mapLeafs g = mapInnersAndLeafs id g


-- | Apply a funtion to all inner nodes, i.e. nodes which are not leaves.
mapInners :: (a -> a) -> Tree a -> Tree a
mapInners f = mapInnersAndLeafs f id


-- | Apply different functions to inner nodes and leaves.
mapInnersAndLeafs
  :: (a -> b)  -- ^ function applied to inner nodes
  -> (a -> b)  -- ^ function applied to leaves
  -> Tree a
  -> Tree b
mapInnersAndLeafs f g = go
  where go (Node x ts) = Node (if null ts then g x else f x) (map go ts)


-- | Like 'mapAccumL', but on the leaves of a tree.
mapAccumLLeafs :: (a -> b -> (a, b)) -> a -> Tree b -> (a, Tree b)
mapAccumLLeafs f = go
  where
    go a (Node x []) = second (flip Node []) (f a x)
    go a (Node x ts) = second (Node x) (mapAccumL go a ts)


-- | Like 'zipWith', but on the leaves of a tree. If the list has less
-- elements than the tree has leaves, the last leaves stay unchanged. If the
-- list has more elements than the tree has leaves, the overhang of the list
-- is discarded.
zipLeafsWith :: (a -> b -> b) -> [a] -> Tree b -> Tree b
zipLeafsWith f = (snd .) . go
  where
    go [] t = ([], t)
    go (x : xs) (Node y []) = (xs, Node (f x y) [])
    go      xs  (Node y ts) = second (Node y) (mapAccumL go xs ts)
