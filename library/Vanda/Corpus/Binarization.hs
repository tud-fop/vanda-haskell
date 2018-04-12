-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Corpus.Binarization
-- Description :  different binarization strategies
-- Copyright   :  (c) Technische Universität Dresden 2016
-- License     :  BSD-style
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Encode trees by binary trees.
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Corpus.Binarization
( Nodetype(..)
, nodetypeById
, -- * First-child-next-sibling encoding
  encodeFcns
, decodeFcns
, nodetypeByFcns
, -- * Left-branching binarization
  encodeLeftbranching0
, encodeLeftbranching1
, decodeLeftbranching
, nodetypeByLeftbranching0
, nodetypeByLeftbranching1
, -- * Miscellaneous
  yieldBy
)
where


import Data.Tree

import qualified Control.Error
import Vanda.Util.Tree (filterTree)


errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.Corpus.Binarization"


-- | We distinguish three types of nodes in an encoded tree.
data Nodetype
  = Inner  -- ^ the node was an inner node before encoding
  | Leaf   -- ^ the node was a leaf before encoding
  | Meta   -- ^ the node was introduced by the encoding
  deriving (Eq, Show)


-- | Get the 'Nodetype' of the root of an unencoded tree.
--
-- This function never returns 'Meta'.
nodetypeById :: Tree a -> Nodetype
nodetypeById (Node _ []) = Leaf
nodetypeById _           = Inner


-- | First-child-next-sibling encoding.
--
-- > encodeFcns "ε" id (x(x₁(…), x₂(…), x₃(…)))
-- >   = x(x₁(…, x₂(…, x₃(…, ε))), ε)
encodeFcns
  :: forall a b
  .  b         -- ^ label for leaf nodes denoting empty tree
  -> (a -> b)  -- ^ encoding of former labels
  -> Tree a
  -> Tree b
encodeFcns nil enc t = go [t]
  where
    go :: Forest a -> Tree b
    go (Node x cs : ss) = Node (enc x) [go cs, go ss]
    go [] = Node nil []


-- | Inverse of 'encodeFcns'.
--
-- Note: Nothing is guaranteed for inputs that are not in the image of one of
-- 'encodeFcns'.
decodeFcns :: forall a b. (b -> a) -> Tree b -> Tree a
decodeFcns dec (Node x [t, _]) = Node (dec x) (go t)
  where
    go :: Tree b -> Forest a
    go (Node y [t1, t2]) = Node (dec y) (go t1) : go t2
    go (Node _ []      ) = []
    go _ = errorHere "decodeFcns.go" "malformed input"
decodeFcns _ _ = errorHere "decodeFcns" "malformed input"


-- | Get the 'Nodetype' of the root of a subtree of a tree encoded by
-- 'encodeFcns'.
nodetypeByFcns :: Tree a -> Nodetype
nodetypeByFcns (Node _ [])             = Meta
nodetypeByFcns (Node _ [Node _ [], _]) = Leaf
nodetypeByFcns _                       = Inner


-- | Encode the children of a node in a chain growing rightwards. The final
-- chain node is 0-ary.
--
-- > encodeLeftbranching0 "ε" ":" id (x(x₁(…), x₂(…), x₃(…)))
-- >   = x(:(x₁(…), :(x₂(…), :(x₃(…), ε))))
encodeLeftbranching0
  :: forall a b
  .  b         -- ^ label for leaf nodes denoting empty list of children
  -> b         -- ^ label for nodes that add another child tree
  -> (a -> b)  -- ^ encoding of former labels
  -> Tree a
  -> Tree b
encodeLeftbranching0 nil cons box = goT
  where
    goT :: Tree a -> Tree b
    goT (Node x ts) = Node (box x) [goF ts]

    goF :: Forest a -> Tree b
    goF (t : ts) = Node cons [goT t, goF ts]
    goF []       = Node nil  []


-- | Encode the children of a node in a chain growing rightwards. The final
-- chain node is 1-ary.
--
-- Note that in contrast to 'encodeLeftbranching0' the property of being a
-- leaf node is preserved.
--
-- > encodeLeftbranching1 "." ":" id (x(x₁(…), x₂(…), x₃(…)))
-- >   = x(:(x₁(…), :(x₂(…), .(x₃(…)))))
encodeLeftbranching1
  :: forall a b
  .  b         -- ^ label for nodes that are a parent of a last child tree
  -> b         -- ^ label for nodes that add another child tree
  -> (a -> b)  -- ^ encoding of former labels
  -> Tree a
  -> Tree b
encodeLeftbranching1 single cons box = goT
  where
    goT :: Tree a -> Tree b
    goT (Node x []) = Node (box x) []
    goT (Node x ts) = Node (box x) [goF ts]

    goF :: Forest a -> Tree b
    goF [t]      = Node single [goT t]
    goF (t : ts) = Node cons   [goT t, goF ts]
    goF []       = errorHere "encodeLeftbranching1" "You found a bug."


-- | Inverse of 'encodeLeftbranching0' and 'encodeLeftbranching1'.
--
-- Note: Nothing is guaranteed for inputs that are not in the image of one of
-- 'encodeLeftbranching0' or 'encodeLeftbranching1'.
decodeLeftbranching :: forall a b. (b -> a) -> Tree b -> Tree a
decodeLeftbranching dec = goT
  where
    -- goT is a bit more general than necessary.
    goT :: Tree b -> Tree a
    goT (Node x ts) = Node (dec x) (concatMap goF ts)

    goF :: Tree b -> Forest a
    goF (Node _ [t1, t2]) = goT t1 : goF t2
    goF (Node _ [t     ]) = [goT t]
    goF (Node _ [      ]) = []
    goF _ = errorHere "decodeLeftbranching.goF" "malformed input"


-- | Get the 'Nodetype' of the root of a subtree of a tree encoded by
-- 'encodeLeftbranching0'.
nodetypeByLeftbranching0 :: Tree a -> Nodetype
nodetypeByLeftbranching0 (Node _ [Node _ []]) = Leaf
nodetypeByLeftbranching0 (Node _ [_        ]) = Inner
nodetypeByLeftbranching0 _                    = Meta


-- | Get the 'Nodetype' of the root of a subtree of a tree encoded by
-- 'encodeLeftbranching1'.
nodetypeByLeftbranching1 :: (a -> Bool) -> Tree a -> Nodetype
nodetypeByLeftbranching1 _ (Node _ [])     = Leaf
nodetypeByLeftbranching1 p (Node x [_])    = if p x then Meta else Inner
nodetypeByLeftbranching1 _ _               = Meta


-- | Get list of 'Leaf' nodes in preorder.
yieldBy :: (Tree a -> Nodetype) -> Tree a -> [a]
yieldBy f = filterTree ((Leaf ==) . f)
