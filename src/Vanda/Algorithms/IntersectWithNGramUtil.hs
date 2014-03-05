{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Algorithms.IntersectWithNGramUtil
-- Copyright   :  (c) Technische Universität Dresden 2013
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Tobias.Denkinger@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Intersects a given IRTG with an Automaton representing an n-gram model.
--
-----------------------------------------------------------------------------


module Vanda.Algorithms.IntersectWithNGramUtil
  ( relabel
  , mapState
  , State (Nullary, Binary, _fst, _snd)
  , Item (Item, _to, _from, _wt)
  , intersect
  , doReordering
  , itemsToHypergraph
  , integerize'
  , makeSingleEndState
  , groupByWeight
  ) where

import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.WTA as WTA
import qualified Data.Interner as In
import qualified Vanda.Grammar.XRS.IRTG as I
import qualified Vanda.Hypergraph.IntHypergraph as HI
import qualified Vanda.Hypergraph.Tree as T

import Data.Hashable
import Vanda.Grammar.LM
import Data.NTT

data State s i
  = Nullary
  | Binary { _fst :: i
           , _snd :: s
           } deriving (Eq, Ord)

instance (Show i, Show s) => Show (State s i) where
  show Nullary
    = show "*"
  show (Binary a b)
    = show a ++ "@" ++ show b

instance (Hashable i, Hashable s) => Hashable (State s i) where
  hashWithSalt s Nullary = s
  hashWithSalt s (Binary a b) = s `hashWithSalt` a `hashWithSalt` b

data Item s l w
  = Item { _to    :: s
         , _wt    :: w
         , _from  :: [s]
         , _lbl   :: l
  } deriving (Eq, Ord, Show)

-- | Relabels the terminals in 'h2' according to the String-to-Int mapping
--   in the language model.
relabel
  :: (Int -> Int)
  -> I.XRS
  -> I.XRS
relabel f1 xrs@I.XRS{ .. }
  = xrs{ I.irtg = irtg{ I.h1 = relabel' f1 $ I.h1 irtg } }

mapState
  :: WTA.State s
  => (i -> j)
  -> (i' -> j')
  -> State (s i') i
  -> State (s j') j
mapState _ _ Nullary
  = Nullary
mapState f1 f2 (Binary a b)
  = Binary (f1 a) (WTA.mapState f2 b)

relabel'
  :: (Int -> Int)                 -- ^ relabeling
  -> V.Vector (T.Tree NTT)      -- ^ original homomorphism
  -> V.Vector (T.Tree NTT)      -- ^ new homomorphism
relabel' r h1
  = let h (T.Nullary (T x)) = T.Nullary . T $ r x
        h (T.Nullary (NT x)) = T.Nullary $ NT x
        h (T.Unary a t) = T.Unary a $ h t
        h (T.Binary a t1 t2) = T.Binary a (h t1) (h t2)
        h (T.Node a ts) = T.Node a $ map h ts
    in  V.map h h1

-- | Intersects IRTG and n-gram model.
intersect
  :: (LM a, WTA.State s, Eq (s Int), Hashable (s Int))
  => (a -> (Int, I.StrictIntPair)
        -> (HI.Hyperedge I.StrictIntPair Int -> Double)
        -> (HI.Hyperedge I.StrictIntPair Int -> [NTT])
        -> HI.Hypergraph I.StrictIntPair Int
        -> [Item (State (s Int) Int) I.StrictIntPair Double]
     )                              -- ^ intersection function
  -> (Int -> Int)                   -- ^ relabeling due to lm
  -> a                              -- ^ language model
  -> I.XRS                          -- ^ translation model
  -> (I.XRS, V.Vector (State (s Int) Int)) -- ^ product translation model, new states
intersect intersect' rel lm I.XRS{ .. }
  = (xrs', states) where
      I.IRTG{ .. } = irtg
      relab (T x)  = T $ rel x
      relab (NT x) = NT x
      hom          = map relab . yield . (V.!) h1 . I._fst . HI.label -- prepare h1
      mu           = log . (VU.!) weights . HI.ident         -- prepare weights
      h1'          = V.snoc h1 . T.Nullary $ NT 0
      h2'          = V.snoc h2 $ V.fromList [NT 0]
      its          = intersect'
                       lm
                       (initial, I.SIP (V.length h1' - 1) (V.length h2' - 1))
                       mu
                       hom
                       rtg                                  -- generate items
      (its', vtx, states)                                   -- integerize Hypergraph
                   = integerize' Nullary its
      (hg, mu')    = itemsToHypergraph its'
      irtg'        = I.IRTG hg vtx h1' h2'
      xrs'         = I.XRS irtg' mu'                         -- build XRS

-- | Converts an 'Item' to a Hyperedge.
itemsToHypergraph
  :: [Item Int l Double]
  -> (HI.Hypergraph l Int, VU.Vector Double)
itemsToHypergraph xs
  = let (wts, xs')
              = groupByWeight xs
        mu    = VU.fromList $ map exp wts
        es    = map (uncurry (\ (a, b, c) d -> HI.mkHyperedge a b c d))
              . concatMap (\(ix, arr) -> zip arr $ repeat ix)
              $ zip [0 ..] xs'
    in  (HI.mkHypergraph es, mu)

-- | reorders/inserts the given 'NState's according to the given reordering/insertion
doReordering
  :: WTA.State s
  => WTA.WTA Int (s Int)            -- ^ language model
  -> [NTT]                          -- ^ reordering/insertion
  -> [s Int]                        -- ^ original states
  -> [([s Int], Double)]            -- ^ processed states
doReordering wta ntts xs
  = let h (T i)  = WTA.delta wta [] [i]
        h (NT i) = [(xs !! i, 0)]
    in  map (L.foldl' (\(qs, ys) (q, y) -> (qs ++ [q], ys + y)) ([], 0)) . sequence $ map h ntts

-- | Takes 'Hyperedge's with arbitrary vertex type and returns 'Hyperedges'
--   with vertex type 'Int'.
integerize'
  :: (Hashable v, Eq v)
  => v
  -> [Item v l d]
  -> ([Item Int l d], Int, V.Vector v)
integerize' vtx is
  = let mi = In.emptyInterner
        h (m, xs) e
           = let (m1, t') = In.intern m (_to e)
                 (m2, f') = In.internListPreserveOrder m1 (_from e)
             in  (m2, Item t' (_wt e) f' (_lbl e):xs)
        (mi', is')
           = foldl h (mi, []) is
             in  (is', snd $ In.intern mi' vtx, V.fromList . reverse . A.elems $ In.internerToArray mi' )

-- | Adds some 'Item's such that 'Item's produced from the former final state
--   are connected to the new final state.
makeSingleEndState
  :: WTA.State s
  => WTA.WTA i (s i)                   -- ^ language model
  -> (State (s i) i -> Bool)           -- ^ is a end state
  -> State (s i) i                     -- ^ new end state
  -> l                                 -- ^ label of new rules
  -> [Item (State (s i) i) l Double]   -- ^ old 'Item's
  -> [Item (State (s i) i) l Double]   -- ^ new 'Item's
makeSingleEndState lm p vInit lbl es
  = (++) es
  . map (\x -> Item vInit (WTA.nu lm $ _snd x) [x] lbl)
  . filter p
  $ map _to es

-- | Groups a 'List' of 'Item's by the weight. Unzips it.
groupByWeight
  :: Ord w
  => [Item i l w]
  -> ([w], [[(i, [i], l)]])
groupByWeight
  = unzip
  . M.toList
  . M.fromListWith (++)
  . map (\(Item a b c d) -> (b, [(a, c, d)]))

yield :: T.Tree a -> [a]
yield (T.Nullary x) = [x]
yield (T.Unary _ t1) = yield t1
yield (T.Binary _ t1 t2) = yield t1 ++ yield t2
yield (T.Node _ ts) = concatMap yield ts
