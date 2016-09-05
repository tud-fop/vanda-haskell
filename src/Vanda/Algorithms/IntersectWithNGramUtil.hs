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
  ( mapState
  , State (Nullary, Binary, _fst, _snd)
  , IOPState (Initial, Trinary, _inp, _cen, _out)
  , Item (Item, _to, _from, _wt)
  , intersect
  , ioProduct
  , doReordering
  , itemsToHypergraph
  , integerize'
  , makeSingleEndState
  , makeSingleEndState'
  , groupByWeight
  , awesomeSequence
  , concatenateRanges
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


data IOPState s1 i s2
  = Initial
  | Trinary { _inp :: s1
            , _cen :: i
            , _out :: s2
            } deriving (Eq, Ord, Show)

instance (Hashable i, Hashable s1, Hashable s2) => Hashable (IOPState s1 i s2) where
  hashWithSalt s Initial = s
  hashWithSalt s (Trinary a b c) = s `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c


data Item s l w
  = Item { _to    :: s
         , _wt    :: w
         , _from  :: [s]
         , _lbl   :: l
  } deriving (Eq, Ord, Show)


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

-- | Intersects IRTG and n-gram model.
intersect
  :: (Eq (s Int), Hashable (s Int))
  => (a -> (Int, I.StrictIntPair)
        -> (HI.Hyperedge I.StrictIntPair Int -> Double)
        -> (HI.Hyperedge I.StrictIntPair Int -> [NTT])
        -> HI.Hypergraph I.StrictIntPair Int
        -> [Item (State (s Int) Int) I.StrictIntPair Double]
     )                                              -- ^ intersection function
  -> a                                                     -- ^ language model
  -> I.XRS                                              -- ^ translation model
  -> (I.XRS, V.Vector (State (s Int) Int))
                                    -- ^ product translation model, new states
intersect intersect' lm I.XRS{ .. }
  = (xrs', states) where
      I.IRTG{ .. } = irtg
      hom          = (V.!) (V.map yield h1) . I._fst . HI.label  -- prepare h1
      mu           = log . (VU.!) weights . HI.ident        -- prepare weights
      h1'          = V.snoc h1 . T.Nullary $ NT 0
      h2'          = V.snoc h2 $ V.fromList [NT 0]
      its          = intersect'
                       lm
                       (initial, I.SIP (V.length h1' - 1) (V.length h2' - 1))
                       mu
                       hom
                       rtg                                   -- generate items
      (its', vtx, states)                             -- integerize Hypergraph
                   = integerize' Nullary its
      (hg, mu')    = itemsToHypergraph its'
      irtg'        = I.IRTG hg vtx h1' h2'
      xrs'         = I.XRS irtg' mu'                              -- build XRS

ioProduct
  :: (Eq (s Int), Hashable (s Int))
  => ( a -> (Int, I.StrictIntPair)
         -> [Int]
         -> (HI.Hyperedge I.StrictIntPair Int -> Double)
         -> (HI.Hyperedge I.StrictIntPair Int -> [NTT])
         -> (HI.Hyperedge I.StrictIntPair Int -> [NTT])
         -> HI.Hypergraph I.StrictIntPair Int
         -> [Item (IOPState (Int, Int) Int (s Int)) I.StrictIntPair Double]
     )                                              -- ^ intersection function
  -> a                                                     -- ^ language model
  -> [Int]                                                           -- ^ word
  -> I.XRS                                              -- ^ translation model
  -> (I.XRS, V.Vector (IOPState (Int, Int) Int (s Int)))
                                    -- ^ product translation model, new states
ioProduct ioProduct' lm word I.XRS{ .. }
  = (xrs', states) where
      I.IRTG{ .. } = irtg
      hom1  = (V.!) (V.map yield h1) . I._fst . HI.label         -- prepare h1
      hom2  = V.toList . (V.!) h2 . I._snd . HI.label            -- prepare h2
      mu    = log . (VU.!) weights . HI.ident               -- prepare weights
      h1'   = V.snoc h1 . T.Nullary $ NT 0
      h2'   = V.snoc h2 $ V.fromList [NT 0]
      its   = ioProduct'
                lm
                (initial, I.SIP (V.length h1' - 1) (V.length h2' - 1))
                word
                mu
                hom1
                hom2
                rtg
      (its', vtx, states)                             -- integerize Hypergraph
            = integerize' Initial its
      (hg, mu')
            = itemsToHypergraph its'
      irtg' = I.IRTG hg vtx h1' h2'
      xrs'  = I.XRS irtg' mu'                                     -- build XRS

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
  :: WTA.WTA Int (s Int)            -- ^ language model
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
  :: WTA.WTA i (s i)                   -- ^ language model
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

-- | Adds some 'Item's such that 'Item's produced from the former final state
--   are connected to the new final state.
makeSingleEndState'
  :: WTA.WTA i (s i)                       -- ^ language model
  -> (IOPState s1 i (s i) -> Bool)         -- ^ is a end state
  -> IOPState s1 i (s i)                   -- ^ new end state
  -> l                                     -- ^ label of new rules
  -> [Item (IOPState s1 i (s i)) l Double] -- ^ old 'Item's
  -> [Item (IOPState s1 i (s i)) l Double] -- ^ new 'Item's
makeSingleEndState' lm p vInit lbl es
  = (++) es
  . map (\x -> Item vInit (WTA.nu lm $ _out x) [x] lbl)
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


-- | For a given list l of tuples of lists ai and bi of symbols,
--   generates all sequences s of length |l| such that for every in
--   in {1, ..., |l|} the i-th symbol of s is either in ai or in bi,
--   and there is at least one j in {1, ..., |l|} such that the j-th
--   element of s is in aj.
awesomeSequence :: [([a], [a])] -> ([[a]], [[a]])
awesomeSequence [] = ([], [[]])
awesomeSequence ((ns, os) : xs)
  = {-# SCC "awesomeSequence" #-}
    ( [ n:as  | n <- ns, as <- nss ++ oss ]
      ++ [ o:ns' | o <- os, ns' <- nss ]
    , [ o:os' | o <- os, os' <- oss ]
    )
  where
    (nss, oss) = awesomeSequence xs

concatenateRanges
  :: Eq a
  => [[(a, a)]]
  -> [(a, a)]
concatenateRanges []
  = []
concatenateRanges (s:ss)
  = let con x [] = [x]
        con (a, b) (x:xs) = concat [ con (a, d) xs | (c, d) <- x, b == c ]
    in  concatMap (flip con ss) s
