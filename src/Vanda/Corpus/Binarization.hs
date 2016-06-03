-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Corpus.Binarization
-- Description :  different binarization strategies
-- Copyright   :  (c) Technische Universität Dresden 2016
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Encode trees by binary trees.
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Corpus.Binarization where


import Data.Tree


-- | First-child-next-sibling encoding.
--
-- > fcns "ε" id (x(x₁(…), x₂(…), x₃(…)))
-- >   = x(x₁(…, x₂(…, x₃(…, ε))), ε)
fcns
  :: forall a b
  .  b         -- ^ label for leaf nodes denoting empty tree
  -> (a -> b)  -- ^ encoding of former labels
  -> Tree a
  -> Tree b
fcns nil enc t = go [t]
  where
    go :: Forest a -> Tree b
    go (Node x cs : ss) = Node (enc x) [go cs, go ss]
    go [] = Node nil []


-- | > leftbranching "ε" ":" id (x(x₁(…), x₂(…), x₃(…)))
--   >   = x(:(x₁(…), :(x₂(…), :(x₃(…), ε))))
leftbranching
  :: forall a b
  .  b         -- ^ label for leaf nodes denoting empty list of children
  -> b         -- ^ label for nodes that add another child tree
  -> (a -> b)  -- ^ encoding of former labels
  -> Tree a
  -> Tree b
leftbranching nil cons box = goT
  where
    goT :: Tree a -> Tree b
    goT (Node x ts) = Node (box x) [goF ts]

    goF :: Forest a -> Tree b
    goF (t : ts) = Node cons [goT t, goF ts]
    goF []       = Node nil  []
