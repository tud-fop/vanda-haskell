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
  , mapCState
  , CState (CState)
  , intersect
  ) where

import qualified Data.Vector as V
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Vector.Unboxed as VU

import Data.NTT
import Data.Hashable
import qualified Data.Interner as In
import Vanda.Grammar.LM
import Vanda.Grammar.NGrams.WTA
import qualified Vanda.Hypergraph.IntHypergraph as HI
import qualified Vanda.Hypergraph.Tree as T
import qualified Vanda.Grammar.XRS.IRTG as I

data CState i
  = CState { _fst :: i
           , _snd :: NState i
  } deriving (Eq, Ord)

instance Show i => Show (CState i) where
  show (CState a b)
    = "(" ++ (show a) ++ "," ++ (show b) ++ ")"

instance Hashable i => Hashable (CState i) where
  hashWithSalt s (CState a b) = s `hashWithSalt` a `hashWithSalt` b

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
relabel f xrs@I.XRS{ .. }
  = xrs{ I.irtg = irtg{ I.h2 = relabel' f . I.h2 $ irtg } }
  
mapCState
  :: (i -> j)
  -> (i -> j)
  -> CState i
  -> CState j
mapCState f1 f2 (CState a (Unary b))
  = CState (f1 a) (Unary (map f2 b))
mapCState f1 f2 (CState a (Binary b1 b2))
  = CState (f1 a) (Binary (map f2 b1) (map f2 b2))

relabel'
  :: (Int -> Int)                 -- ^ relabeling
  -> V.Vector (V.Vector NTT)      -- ^ original homomorphism
  -> V.Vector (V.Vector NTT)      -- ^ new homomorphism
relabel' r h2
  = let f []          = []
        f ((T x):xs)  = (T (r x)):(f xs)
        f ((NT x):xs) = (NT x):(f xs)
    in  V.map (V.fromList . f . V.toList) h2

-- | Intersects IRTG and n-gram model.
intersect
  :: LM a
  => a                              -- ^ language model
  -> I.XRS                          -- ^ translation model
  -> (I.XRS, V.Vector (CState Int)) -- ^ product translation model, new states
intersect lm I.XRS{ .. }
  = let I.IRTG{ .. }
              = irtg
        hom   = V.toList . (V.!) h2 . I._snd . HI.label
                                                      -- prepare h2
        mu    = log . (VU.!) weights . HI.ident       -- prepare weights
        its   = intersect' lm mu hom rtg              -- generate items
        (h1', l1)
              = addToHomomorphism h1 (T.Nullary (NT 0))
        (h2', l2)
              = addToHomomorphism h2 (V.fromList [NT 0])
        its'  = makeSingleEndState
                  ((==) initial . _fst)
                  (CState 0 emptyNState)
                  (I.SIP l1 l2)
                  its
        (its'', vtx, states)                          -- integerize Hypergraph
              = integerize' (CState 0 emptyNState) its'
        (hg, mu')
              = itemsToHypergraph its''
        irtg' = I.IRTG hg vtx h1' h2'
        xrs'  = I.XRS irtg' mu'                       -- build XRS
    in  (xrs', states)

addToHomomorphism
  :: V.Vector t
  -> t
  -> (V.Vector t, Int)
addToHomomorphism h e
  = (V.fromList . flip (++) [e] . V.toList $ h, V.length h)

-- | Intersects IRTG and n-gram model, emits 'Item's.
intersect'
  :: (Ord l, Show l, Show i1, LM a)
  => a                              -- ^ language model
  -> (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> HI.Hypergraph l i1             -- ^ RTG hypergraph
  -> [Item (CState Int) l Double]            -- ^ resulting list of 'Items'
intersect' lm mu h2 hg
  = let es0 = filter ((==) 0 . HI.arity) . HI.edges $ hg
        is0 = M.fromListWith (++)
            . map (\x -> (HI.to x, [initRule mu h2 lm x]))
            $ es0
        es  = filter ((/=) 0 . HI.arity) . HI.edges $ hg
        go !its
          = let l = [ ( HI.to e, lst )
                    | e  <- es
                    , let lst = L.nub
                              $ [ r
                                | let ss = sequence
                                         $ [ M.findWithDefault [] t1 its
                                           | t1 <- HI.from e
                                           ]
                                , not . L.null $ ss
                                , s <- ss
                                , let r = blowRule mu h2 lm e s
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
        mu    = VU.fromList . map exp $ wts
        es    = map (uncurry (\ (a, b, c) d -> HI.mkHyperedge a b c d))
              . concat
              . map (\(ix, arr) -> zip arr (repeat ix))
              . zip [0 ..]
              $ xs'
    in  (HI.mkHypergraph es, mu)

-- | Emits an initial 'Item'.
initRule
  :: LM a
  => (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> a                              -- ^ language model
  -> HI.Hyperedge l i1              -- ^ rule
  -> Item (CState Int) l Double     -- ^ resulting 'Item'
initRule mu h2 lm he
  = let f (T x)  = x
        f (NT _) = 0
        (st, w1) = mkNState lm . map f . h2 $ he
    in  Item 
          (CState (HI.to he) st)
          (w1 + (mu he))
          []
          (HI.label he)

-- | Combines 'Item's by a rule. The 'Item's and the rule must
--   match (not checked).
blowRule
  :: LM a
  => (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> a                              -- ^ language model
  -> HI.Hyperedge l i1              -- ^ rule
  -> [Item (CState Int) l Double]   -- ^ 'Item's
  -> Item (CState Int) l Double     -- ^ resulting 'Item'
blowRule mu h2 lm he is
  = let xs      = map _to is
        (x, w1) = toNState lm (map _snd xs) (h2 he)
    in  Item (CState (HI.to he) x) (mu he + w1) xs (HI.label he)

toNState
  :: LM a
  => a
  -> [NState Int]
  -> [NTT]
  -> (NState Int, Double)
toNState lm m xs
  = let f (T i)  = mkNState lm [i]
        f (NT i) = (m !! i, 0)
    in  (\ ((x, w1), w2) -> (x, w1 + w2))
      . (\ (xs', w) -> (mergeNStates lm xs', sum w))
      . unzip
      . map f
      $ xs

-- | Takes 'Hyperedge's with arbitrary vertex type and returns 'Hyperedges'
--   with vertex type 'Int'.
integerize'
  :: (Hashable v, Eq v)
  => v
  -> [Item v l d]
  -> ([Item Int l d], Int, V.Vector v)
integerize' vtx is
  = let mi = In.emptyInterner
        f (m, xs) e
           = let (m1, t') = In.intern m (_to e)
                 (m2, f') = In.internList m1 (_from e)
             in  (m2, (Item t' (_wt e) f' (_lbl e)):xs)
        (mi', is')
           = foldl f (mi, []) is
    in  (is', snd . In.intern mi' $ vtx, V.fromList . A.elems . In.internerToArray $ mi' )

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