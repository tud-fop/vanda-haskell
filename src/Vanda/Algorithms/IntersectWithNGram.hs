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

import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Vector.Unboxed as VU

import Data.NTT
import qualified Data.IntTokenMap as IM
import Vanda.Token
import Vanda.Grammar.NGrams.KenLM
import Vanda.Grammar.NGrams.WTA
import qualified Vanda.Hypergraph.IntHypergraph as HI
import qualified Vanda.Hypergraph.Basic as HB
import qualified Vanda.Hypergraph.Tree as T
import qualified Vanda.Grammar.XRS.IRTG as I

import Debug.Trace

type Item i l w = ((i, NState i), w, [(i, NState i)], l)

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
intersect lm I.XRS{ .. }
  = let I.IRTG{ .. }
              = irtg
        hom   = (V.!) h2 . I._snd . HI.label          -- prepare h2
        mu    = (VU.!) weights . HI.ident             -- prepare weights
        its   = intersect' lm mu hom rtg              -- generate items
        lbl'  = I.SIP                                 -- generate homomorphism
                  ((+) 1 . L.maximumBy compare        --  pointers
                         . map (I._fst . HI.label)
                         . HI.edges
                         $ rtg
                  )
                  ((+) 1 . L.maximumBy compare 
                         . map (I._snd . HI.label)
                         . HI.edges
                         $ rtg
                  )
        h1'   = V.fromList                            -- update h1
              . flip (++) [T.Unary (T 0) (T.Nullary (NT 0))]
              . V.toList
              $ h1
        h2'   = V.fromList                            -- update h2
              . flip (++) [[NT 0]]
              . V.toList
              $ h2
        its'  = makeSingleEndState
                  (initial ==)
                  (0, emptyNState)
                  lbl'
                  its
        wt_es = groupByWeight its'
        mu'   = VU.fromList . fst $ wt_es
        es    = map (uncurry itemToHyperedge)
              . concat
              . map (\(ix, arr) -> zip arr (repeat ix))
              . zip [0 ..]
              . snd
              $ wt_es
        (es'', vtx)                                   -- integerize Hypergraph
              = integerize (0, emptyNState) es
        irtg' = I.IRTG                                -- build IRTG
                 (HI.mkHypergraph es'') vtx h1' h2'
        xrs'  = I.XRS irtg' mu'                       -- build XRS
    in  trace (unlines . map show $ its)
      $ xrs'

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
  :: ((i, NState i), [(i, NState i)], l)
  -> i1
  -> HB.Hyperedge (i, NState i) l i1
itemToHyperedge (h, t, lbl) idx
  = HB.mkHyperedge h t lbl idx

-- | Emits an initial 'Item'.
initRule
  :: (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> KenLM                          -- ^ language model
  -> HI.Hyperedge l i1              -- ^ rule
  -> Item Int l Double              -- ^ resulting 'Item'
initRule mu h2 lm he
  = let f (T x) = x
        (st, w) = mkNState lm . map f . h2 $ he
    in  ( (HI.to he, st)
        , w * (mu he)
        , []
        , HI.label he
        )

-- | Combines 'Item's by a rule. The 'Item's and the rule must
--   match (not checked).
-- blowRule
--   :: (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
--   -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
--   -> KenLM                          -- ^ language model
--   -> HI.Hyperedge l i1              -- ^ rule
--   -> [Item i l w]                   -- ^ 'Item's
--   -> Item i l w                     -- ^ resulting 'Item'
-- blowRule w h2 lm he is
--   = let ss  = blowHelper (h2 he) is
--         mu' = (w he) * (getWeight lm ss)
--         s'  = collapseStates (order lm) ss
--     in  ((to he, s'), mu', he)-}

-- | Combines a String of terminals and non-terminals with a String of 'Item's
--   to a String of terminals and states
-- blowHelper
--   :: [NTT]
--   -> [Item i l w]
--   -> [NStateString Int]
-- blowHelper [] _
--   = []
-- blowHelper ((T x):xs) ys
--   = [x]:(blowHelper xs ys)
-- blowHelper ((NT _):xs) (((_, y), _, _, _):ys)
--   = y:(blowHelper xs ys)
-- blowHelper _ _
--   = []

-- | Takes 'Hyperedge's with arbitrary vertex type and returns 'Hyperedges'
--   with vertex type 'Int'.
integerize
  :: Ord v
  => v
  -> [HB.Hyperedge v l i]
  -> ([HI.Hyperedge l i], Int)
integerize vtx es
  = let mi = IM.empty
        f (m, xs) e
           = let (m1, t') = IM.getInt m (HB.to e)
                 (m2, f') = IM.getInts m1 (HB.from e)
             in  (m2, (HI.mkHyperedge t' f' (HB.label e) (HB.ident e)):xs)
        (mi', es')
           = foldl f (mi, []) es
    in  (es', snd . IM.getInt mi' $ vtx)

-- | Adds some 'Item's such that 'Item's produced from the former final state
--   are connected to the new final state.
makeSingleEndState
  :: (Eq i, Fractional w)
  => (i -> Bool)                       -- ^ is a end state
  -> (i, NState i)                     -- ^ new end state
  -> l                                 -- ^ label of new rules
  -> [Item i l w]                      -- ^ old 'Item's
  -> [Item i l w]                      -- ^ new 'Item's
makeSingleEndState p vInit lbl es
  = (++) es
  . map (\x -> (vInit, 1.0, [x], lbl))
  . L.nub
  . filter (p . fst)
  . map (\(x, _, _, _) -> x)
  $ es

-- | Groups a 'List' of 'Item's by the weight. Unzips it.
groupByWeight
  :: Ord w
  => [Item i l w]
  -> ([w], [[((i, NState i),[(i, NState i)], l)]])
groupByWeight
  = unzip
  . M.toList
  . M.fromListWith (++)
  . map (\(a, b, c, d) -> (b, [(a, c, d)]))