{-# LANGUAGE RecordWildCards, BangPatterns #-}
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
  , State (Unary, Binary, _fst, _snd)
  , Item (Item, _to, _from, _wt)
  , intersect
  , doReordering
  , itemsToHypergraph
  , integerize'
  , makeSingleEndState
  , groupByWeight
  ) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Data.NTT
import Data.Hashable
import qualified Data.Interner as In
import Vanda.Grammar.LM
import qualified Vanda.Grammar.NGrams.WTA as WTA
import qualified Vanda.Hypergraph.IntHypergraph as HI
import qualified Vanda.Hypergraph.Tree as T
import qualified Vanda.Grammar.XRS.IRTG as I

data State i
  = Unary i
  | Binary { _fst :: i
           , _snd :: WTA.State i
           } deriving (Eq, Ord)

instance Show i => Show (State i) where
  show (Unary a)
    = show a
  show (Binary a b)
    = show a ++ "@" ++ show b

instance Hashable i => Hashable (State i) where
  hashWithSalt s (Unary a) = s `hashWithSalt` a
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
  = xrs{ I.irtg = irtg{ I.h2 = relabel' f1 $ I.h2 irtg } }

mapState
  :: (i -> j)
  -> (i -> j)
  -> State i
  -> State j
mapState f1 _ (Unary a)
  = Unary $ f1 a
mapState f1 f2 (Binary a b)
  = Binary (f1 a) (WTA.mapState f2 b)

relabel'
  :: (Int -> Int)                 -- ^ relabeling
  -> V.Vector (V.Vector NTT)      -- ^ original homomorphism
  -> V.Vector (V.Vector NTT)      -- ^ new homomorphism
relabel' r h2
  = let h []          = []
        h ((T x):xs)  = (T (r x)):(h xs)
        h ((NT x):xs) = (NT x):(h xs)
    in  flip V.map h2 $ V.fromList . h . V.toList

-- | Intersects IRTG and n-gram model.
intersect
  :: LM a
  => (a -> (HI.Hyperedge I.StrictIntPair Int -> Double)
        -> (HI.Hyperedge I.StrictIntPair Int -> [NTT])
        -> HI.Hypergraph I.StrictIntPair Int
        -> [Item (State Int) I.StrictIntPair Double]
     )                              -- ^ intersection function
  -> a                              -- ^ language model
  -> I.XRS                          -- ^ translation model
  -> (I.XRS, V.Vector (State Int)) -- ^ product translation model, new states
intersect intersect' lm I.XRS{ .. }
  = (xrs', states) where
      I.IRTG{ .. } = irtg
      hom          = V.toList . (V.!) h2 . I._snd . HI.label -- prepare h2
      mu           = log . (VU.!) weights . HI.ident         -- prepare weights
      its          = intersect' lm mu hom rtg                -- generate items
      h1'          = V.snoc h1 . T.Nullary $ NT 0
      h2'          = V.snoc h2 $ V.fromList [NT 0]
      its'         = makeSingleEndState
                       ((==) initial . _fst)
                       (Unary 0)
                       (I.SIP (V.length h1' - 1) (V.length h2' - 1))
                       its
      (its'', vtx, states)                                   -- integerize Hypergraph
                   = integerize' (Unary 0) its'
      (hg, mu')    = itemsToHypergraph its''
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
  :: WTA.WTA Int Int                -- ^ language model
  -> [NTT]                          -- ^ reordering/insertion
  -> [WTA.State Int]                -- ^ original states
  -> [([WTA.State Int], Double)]    -- ^ processed states
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
             in  (m2, (Item t' (_wt e) f' (_lbl e)):xs)
        (mi', is')
           = foldl h (mi, []) is
    in  (is', snd $ In.intern mi' vtx, V.fromList . A.elems $ In.internerToArray mi' )

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
  . map (\x -> Item vInit 0 [x] lbl)
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
