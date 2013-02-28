{-# LANGUAGE RecordWildCards, BangPatterns #-}
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
import qualified Vanda.Hypergraph.Tree as T
import qualified Vanda.Grammar.XRS.IRTG as I

import Debug.Trace

data CState i
  = CState { _fst :: i
           , _snd :: NState i
  } deriving (Eq, Ord)

instance Show i => Show (CState i) where
  show (CState f s)
    = "{" ++ (show f) ++ "," ++ (show s) ++ "}"

data Item s l w
  = Item { _to    :: s
         , _wt    :: w
         , _from  :: [s]
         , _lbl   :: l
  } deriving (Eq, Ord)

instance (Show i, Show w) => Show (Item i l w) where
  show (Item to wt from _)
    = (show to) ++ " <-- " ++ (show from) ++ " # " ++ (show wt) 

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
        mu x  = (VU.!) weights . HI.ident $ x         -- prepare weights
        its   = intersect' lm mu hom rtg              -- generate items
        (h1', l1)
              = addToHomomorphism h1 (T.Nullary (NT 0))
        (h2', l2)
              = addToHomomorphism h2 [NT 0]
        its'  = makeSingleEndState
                  ((==) initial . _fst)
                  (CState 0 emptyNState)
                  (I.SIP l1 l2)
                  its
        (its'', vtx)                                   -- integerize Hypergraph
              = integerize' (CState 0 emptyNState) its'
        (hg, mu')
              = itemsToHypergraph its''
        irtg' = I.IRTG hg vtx h1' h2'
        xrs'  = I.XRS irtg' mu'                       -- build XRS
    in  xrs'

addToHomomorphism
  :: V.Vector t
  -> t
  -> (V.Vector t, Int)
addToHomomorphism h e
  = (V.fromList . flip (++) [e] . V.toList $ h, V.length h)

-- | Intersects IRTG and n-gram model, emits 'Item's.
intersect'
  :: (Ord l, Show l, Show i1)
  => KenLM                          -- ^ language model
  -> (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> HI.Hypergraph l i1             -- ^ RTG hypergraph
  -> [Item (CState Int) l Double]            -- ^ resulting list of 'Items'
intersect' lm mu h2 hg
  = let es0 = filter ((==) 0 . HI.arity) . HI.edges $ hg
        is0 = M.fromListWith (++)
            . map (\x -> (HI.to x, [initRule mu h2 lm x]))
            $ es0
        es  = trace (unlines . map show . concat . map snd . M.toList $ is0) $
              filter ((/=) 0 . HI.arity) . HI.edges $ hg
        go !its
          = let l = [ ( HI.to e, lst )
                    | e  <- es
                    , let lst = [ trace (show r) $
                                  r
                                | let ss = sequence
                                         $ [ M.findWithDefault [] t1 its
                                           | t1 <- HI.from e
                                           ]
                                , not . L.null $ ss
                                , s <- ss
                                , let r = blowRule lm mu h2 e s
                                , not . elem r 
                                      . flip (M.findWithDefault []) its
                                      . HI.to
                                      $ e
                                ]
                    , not . L.null
                          $ lst
                    ]
            in  if   L.null l
                then concat . map snd
                            . M.toAscList
                            $ its
                else go 
                   . foldl (\ m (k, v) -> M.insertWith (++) k v m) its
                   $ l
    in  go is0

-- | Converts an 'Item' to a Hyperedge.
itemsToHypergraph
  :: [Item Int l Double]
  -> (HI.Hypergraph l Int, VU.Vector Double)
itemsToHypergraph xs
  = let (wts, xs')
              = groupByWeight xs
        mu    = VU.fromList wts
        es    = map (uncurry (\ (a, b, c) d -> HI.mkHyperedge a b c d))
              . concat
              . map (\(ix, arr) -> zip arr (repeat ix))
              . zip [0 ..]
              $ xs'
    in  (HI.mkHypergraph es, mu)

-- | Emits an initial 'Item'.
initRule
  :: (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> KenLM                          -- ^ language model
  -> HI.Hyperedge l i1              -- ^ rule
  -> Item (CState Int) l Double     -- ^ resulting 'Item'
initRule mu h2 lm he
  = let f (T x)  = x
        f (NT _) = 0
        (st, w1) = mkNState lm . map f . h2 $ he
    in  Item 
          (CState (HI.to he) st)
          (w1 * (mu he))
          []
          (HI.label he)

-- | Combines 'Item's by a rule. The 'Item's and the rule must
--   match (not checked).
blowRule
  :: KenLM                          -- ^ language model
  -> (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> HI.Hyperedge l i1              -- ^ rule
  -> [Item (CState Int) l Double]   -- ^ 'Item's
  -> Item (CState Int) l Double     -- ^ resulting 'Item'
blowRule lm mu h2 he is
  = let xs      = map _to is
        (x, w1) = toNState lm (map _snd xs) (h2 he)
    in  Item (CState (HI.to he) x) (mu he * w1) xs (HI.label he)

toNState
  :: KenLM
  -> [NState Int]
  -> [NTT]
  -> (NState Int, Double)
toNState lm m xs
  = let f (T i)  = mkNState lm [i]
        f (NT i) = (m !! i, 1)
    in  (\ ((x, w1), w2) -> (x, w1 * w2))
      . (\ (xs', w) -> (mergeNStates lm xs', product w))
      . unzip
      . map f
      $ xs

-- | Takes 'Hyperedge's with arbitrary vertex type and returns 'Hyperedges'
--   with vertex type 'Int'.
integerize'
  :: Ord v
  => v
  -> [Item v l d]
  -> ([Item Int l d], Int)
integerize' vtx is
  = let mi = IM.empty
        f (m, xs) e
           = let (m1, t') = IM.getInt m (_to e)
                 (m2, f') = IM.getInts m1 (_from e)
             in  (m2, (Item t' (_wt e) f' (_lbl e)):xs)
        (mi', is')
           = foldl f (mi, []) is
    in  (is', snd . IM.getInt mi' $ vtx)

-- | Adds some 'Item's such that 'Item's produced from the former final state
--   are connected to the new final state.
makeSingleEndState
  :: (Eq i, Fractional w)
  => (i -> Bool)                       -- ^ is a end state
  -> i                                 -- ^ new end state
  -> l                                 -- ^ label of new rules
  -> [Item i l w]                      -- ^ old 'Item's
  -> [Item i l w]                      -- ^ new 'Item's
makeSingleEndState p vInit lbl es
  = (++) es
  . map (\x -> Item vInit 1.0 [x] lbl)
  . L.nub
  . filter p
  . map _to
  $ es

-- | Groups a 'List' of 'Item's by the weight. Unzips it.
groupByWeight
  :: Ord w
  => [Item i l w]
  -> ([w], [[(i,[i], l)]])
groupByWeight
  = unzip
  . M.toList
  . M.fromListWith (++)
  . map (\(Item a b c d) -> (b, [(a, c, d)]))