{-# LANGUAGE RecordWildCards #-}
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


module Vanda.Algorithms.IntersectWithNGram
  ( relabel
  , intersect
  ) where

import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Map as M

import Data.NTT
import Vanda.Token
import Vanda.Grammar.NGrams.KenLM
import Vanda.Grammar.NGrams.WTA
import qualified Vanda.Hypergraph.IntHypergraph as HI
import qualified Vanda.Hypergraph.Basic as HB
import qualified Vanda.Grammar.XRS.IRTG as I

import Debug.Trace

type Item i l w = ((i, NState i), w, [(i, NState i)], l)
data IntMap v = IntMap _mapping :: M.Map v Int
                       len      :: Int

empty
  :: IntMap v
empty
  = IntMap M.empty 0

getInt
  :: IntMap v
  -> v
  -> (IntMap v, Int)
getInt im@(IntMap m l) k
  = case M.lookup k m of
         Just v -> (im, v)
         _      -> (im{ _mapping = insert k l m, len = l + 1 }, l)

getInts
  :: IntMap v
  -> [v]
  -> (IntMap v, [Int])
getInts im ks
  = let f (m, xs) k = (m', x:xs) where (m', x) = getInt m k
    in  foldl f ks

-- | Relabels the terminals in 'h2' according to the String-to-Int mapping
--   in the language model.
relabel
  :: KenLM
  -> TokenArray
  -> I.XRS
  -> I.XRS
relabel lm ta xrs@I.XRS{ .. }
  = let r = dictIndex lm . getString ta
    in  xrs{ I.irtg = irtg{ I.h2 = relabel' r . I.h2 $ irtg } }

relabel'
  :: (Int -> Int)                 -- ^ relabeling
  -> V.Vector [NTT]               -- ^ original homomorphism
  -> V.Vector [NTT]               -- ^ new homomorphism
relabel' r h2
  = let f []          = []
        f ((T x):xs)  = (T (r x)):(f xs)
        f ((NT x):xs) = (NT x):(f xs)
    in  V.map f h2

-- | Intersects IRTG and n-gram model.
intersect
  :: KenLM                          -- ^ language model
  -> I.XRS                          -- ^ translation model
  -> I.XRS                          -- ^ product translation model
intersect lm tm@I.XRS{ .. }
  = let I.IRTG{ .. }
            = irtg
        hom = (V.!) h2 . I._snd . HI.label
        mu  = (VU.!) weights . HI.ident
        its = intersect' lm mu hom rtg
    in  trace (unlines . map show $ its)
      $ tm

-- | Intersects IRTG and n-gram model, emits 'Item's.
intersect'
  :: KenLM                          -- ^ language model
  -> (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> HI.Hypergraph l i1             -- ^ RTG hypergraph
  -> [Item Int l Double]            -- ^ resulting list of 'Items'
intersect' lm w h2 hg
  = let es = filter ((==) 0 . HI.arity) . HI.edges $ hg
    in  map (initRule w h2 lm) es

-- | Converts an 'Item' to a Hyperedge.
itemToHyperedge
  :: Item i l w
  -> i1
  -> (HB.Hyperedge (i, NState i) l i1, w)
itemToHyperedge (h, mu, t, lbl) idx
  = (HB.mkHyperedge h t lbl idx, mu)

-- | Emits an initial 'Item'.
initRule
  :: (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> KenLM                          -- ^ language model
  -> HI.Hyperedge l i1              -- ^ rule
  -> Item Int l Double              -- ^ resulting 'Item'
initRule w h2 lm he
  = ((HI.to he, initState (order lm) . h2 $ he), w he, [], HI.label he)

-- | Combines 'Item's by a rule. The 'Item's and the rule must
--   match (not checked).
blowRule
  :: (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> KenLM                          -- ^ language model
  -> HI.Hyperedge l i1              -- ^ rule
  -> [Item i l w]                   -- ^ 'Item's
  -> Item i l w                     -- ^ resulting 'Item'
blowRule w h2 lm he is
  = undefined {-let ss  = blowHelper (h2 he) is
        mu' = (w he) * (getWeight lm ss)
        s'  = collapseStates (order lm) ss
        in  ((to he, s'), mu', he)-}

-- | Combines a String of terminals and non-terminals with a String of 'Item's
--   to a String of terminals and states
blowHelper
  :: [NTT]
  -> [Item i l w]
  -> [NStateString Int]
blowHelper [] _
  = undefined --[]
blowHelper ((T x):xs) ys
  = undefined --[x] ++ blowHelper xs ys
blowHelper ((NT _):xs) (((_, y), _, _, _):ys)
  = undefined --[y] ++ blowHelper xs ys

integerize
  :: HB.Hypergraph v l i
  -> HI.Hypergraph l i
integerize hg
  = let m  = empty
        e1 = HB.edges hg
        
