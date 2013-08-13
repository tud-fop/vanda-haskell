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


module Vanda.Algorithms.IntersectWithNGram where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Data.NTT
import Vanda.Grammar.LM
import qualified Vanda.Hypergraph.IntHypergraph as HI
import Vanda.Algorithms.IntersectWithNGramUtil
import Vanda.Grammar.NGrams.WTA

-- | Intersects IRTG and n-gram model, emits 'Item's.
intersect
  :: (Ord l, Show l, Show i1, LM a)
  => a                              -- ^ language model
  -> (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> HI.Hypergraph l i1             -- ^ RTG hypergraph
  -> [Item (CState Int) l Double]   -- ^ resulting list of 'Items'
intersect lm mu h2 hg
  = let es0 = filter ((==) 0 . HI.arity) . HI.edges $ hg
        is0 = M.fromListWith (++)
            . map (\x -> (HI.to x, [initRule mu h2 lm x]))
            $ es0
        es  = filter ((/=) 0 . HI.arity) . HI.edges $ hg
        go !ts !its
          = let l = [ ( HI.to e, lst )
                    | e  <- es
                    , let lst = L.nub $
                                [ r
                                | let ss = sequence
                                         $ [ M.findWithDefault [] t1 its
                                           | t1 <- HI.from e
                                           ]
                                , not . L.null $ ss
                                , s <- ss
                                , S.notMember (map _to s) ts
                                , let r = blowRule mu h2 lm e s
                                ]
                    , not . L.null
                          $ lst
                    ]
            in  if   L.null l
                then concat . map snd
                            . M.toAscList
                            $ its
                else go (S.union (S.fromList . map _from . concatMap snd $ l) ts)
                   . foldl (\ m (k, v) -> M.insertWith (++) k v m) its
                   $ l
    in  go S.empty is0

-- | Emits an initial 'Item'.
initRule
  :: LM a
  => (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> a                              -- ^ language model
  -> HI.Hyperedge l i1              -- ^ rule
  -> Item (CState Int) l Double     -- ^ resulting 'Item'
initRule mu h2 lm he
  = let h (T x)  = x
        h (NT x) = x
        st = deltaS lm [] . map h . h2 $ he
        w1 = deltaW lm [] . map h . h2 $ he
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
        xr      = doReordering (h2 he) . map _snd $ xs
        x       = deltaS lm xr []
        w1      = deltaW lm xr []
    in  Item (CState (HI.to he) x) (mu he + w1) xs (HI.label he)
