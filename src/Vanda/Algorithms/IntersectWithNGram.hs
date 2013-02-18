-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Algorithms.IntersectWithNGram
-- Copyright   :  (c) Technische Universität Dresden 2013
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Tobias.Denkinger@mailbox.tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Intersects a given IRTG with an Automaton representing an n-gram model.
--
-----------------------------------------------------------------------------


module Vanda.Algorithms.IntersectWithNGram where

import Data.NTT
import Vanda.Grammar.NGrams.WTA
import import Vanda.Hypergraph.IntHypergraph

type Item i = ((i, NState i), Double, NTT)

-- | Intersects IRTG and n-gram model.
intersect
  :: Hypergraph l i                -- ^ RTG hypergraph \
  -> Int                           -- ^ target symbol   RTG
  -> (Hyperedge l i -> Double)     -- ^ weights of RTG /
  -> (Hyperedge l i -> [NTT])      -- ^ tree to string homomorphism
  -> KenLM                         -- ^ n-gram language model
  -> [Item Int]                    -- ^ resulting list of 'Items'
intersect hg q0 w h2 lm
  = undefined

-- | Emits an initial 'Item'.
initRule
  :: (Hyperedge l i -> Double)     -- ^ weights
  -> (Hyperedge l i -> [NNT])      -- ^ tree to string homomorphism
  -> KenLM                         -- ^ language model
  -> Hyperedge l i                 -- ^ rule
  -> Item Int                      -- ^ resulting 'Item'
initRule w h2 lm he
  = ((to he, initState . h2 $ he), w he, he)

-- | Combines 'Item's by a rule. The 'Item's and the rule must
--   match (not checked).
blowRule
  :: (Hyperedge l i -> Double)     -- ^ weights
  -> (Hyperedge l i -> [NNT])      -- ^ tree to string homomorphism
  -> KenLM                         -- ^ language model
  -> Hyperedge l i                 -- ^ rule
  -> [Item Int]                    -- ^ 'Item's
  -> Item Int                      -- ^ resulting 'Item'
blowRule w h2 lm he is
  = let ss  = blowHelper (h2 he) is
        mu' = (w he) * (getWeight lm ss)
        s'  = collapseStates (order lm) ss
    in  ((to he, s'), mu', he)

blowHelper
  :: [NTT]
  -> [Item Int]
  -> [NStateString]
blowHelper [] _
  = []
blowHelper ((T x):xs) ys
  = [x] ++ blowHelper xs ys
blowHelper ((NT _):xs) (((_, y), _, _):ys)
  = [y] ++ blowHelper xs ys

