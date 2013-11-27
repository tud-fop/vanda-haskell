-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Algorithms.IntersectWithNGram
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


module Vanda.Algorithms.IntersectWithNGram where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.NTT
import Vanda.Grammar.LM
import qualified Vanda.Hypergraph.IntHypergraph as HI
import Vanda.Algorithms.IntersectWithNGramUtil
import qualified Vanda.Grammar.NGrams.WTA as WTA
import qualified Vanda.Grammar.NGrams.WTA_BHPS as WTABHPS
import qualified Data.WTA as WTA

-- | Intersects IRTG and n-gram model, emits 'Item's.
intersectBHPS
  :: (Ord l, LM a)
  => a                              -- ^ language model
  -> (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> HI.Hypergraph l i1             -- ^ RTG hypergraph
  -> [Item (State (WTABHPS.State' Int) Int) l Double]    -- ^ resulting list of 'Items'
intersectBHPS lm mu h2 hg
  = let wta = WTABHPS.makeWTA lm
            . L.nub
            . concatMap (map (\(T x) -> x))
            . map h2 $ filter ((==) 0 . HI.arity)
            $ HI.edges hg
    in  intersect' wta mu h2 hg

-- | Intersects IRTG and n-gram model, emits 'Item's.
intersectSmoothed
  :: (Ord l, LM a)
  => a                              -- ^ language model
  -> (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> HI.Hypergraph l i1             -- ^ RTG hypergraph
  -> [Item (State (WTA.State' Int) Int) l Double]    -- ^ resulting list of 'Items'
intersectSmoothed lm mu h2 hg
  = let wta = WTA.smoothedWTA lm
    in  intersect' wta mu h2 hg

-- | Intersects IRTG and n-gram model, emits 'Item's.
intersectUnsmoothed
  :: (Ord l, LM a)
  => a                              -- ^ language model
  -> (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> HI.Hypergraph l i1             -- ^ RTG hypergraph
  -> [Item (State (WTA.State' Int) Int) l Double]    -- ^ resulting list of 'Items'
intersectUnsmoothed lm mu h2 hg
  = let wta = WTA.unsmoothedWTA lm
    in  intersect' wta mu h2 hg

intersect'
  :: (Ord l, Ord (s Int), WTA.State s)
  => WTA.WTA Int (s Int)            -- ^ language model
  -> (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> HI.Hypergraph l i1             -- ^ RTG hypergraph
  -> [Item (State (s Int) Int) l Double]  -- ^ resulting list of 'Items'
intersect' wta mu h2 hg
  = let es  = filter ((/=) 0 . HI.arity) $ HI.edges hg
        es0 = filter ((==) 0 . HI.arity) $ HI.edges hg
        is0 = flip concatMap es0 $ initRule mu h2 wta
        ns0 = M.map S.fromList . M.fromListWith (++) . map (\x -> (_fst x, [x])) $ map _to is0
        go cs os ns its
          = let is = [ (HI.to e, concat lst)
                     | e <- es
                     , let lst = [ blowRule mu h2 wta e s
                                 | s <- fst
                                      $ awesomeSequence
                                        [ (nLst, oLst)
                                        | q <- HI.from e
                                        , let nLst = S.toList $ M.findWithDefault S.empty q ns
                                        , let oLst = S.toList $ M.findWithDefault S.empty q os
                                        ]
                                 , S.notMember s cs
                                 ]
                     , not $ null lst
                     ]
                cs' = flip S.union cs
                    . S.fromList
                    . map _from
                    $ concatMap snd is
                os' = M.unionWith S.union os ns
                ns' = M.mapWithKey (\ k x -> S.difference x $ M.findWithDefault S.empty k os')
                    . M.map (S.fromList . map _to)
                    $ M.fromListWith (++) is
            in  if   null is
                then its
                else go cs' os' ns' $ concatMap snd is ++ its
    in  go S.empty M.empty ns0 is0


-- | Emits an initial 'Item'.
initRule
  :: WTA.State s
  => (HI.Hyperedge l i1 -> Double)        -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])         -- ^ tree to string homomorphism
  -> WTA.WTA Int (s Int)                  -- ^ language model
  -> HI.Hyperedge l i1                    -- ^ rule
  -> [Item (State (s Int) Int) l Double]  -- ^ resulting 'Item'
initRule mu h2 lm he
  = let h (T x)  = x
        h (NT x) = x
        sts      = map h $ h2 he
        xs       = WTA.delta lm [] sts
    in  map (\ (st, w1)
            -> Item (Binary (HI.to he) st)
                (mu he + w1)
                []
                (HI.label he))
            xs

-- | Combines 'Item's by a rule. The 'Item's and the rule must
--   match (not checked).
blowRule
  :: WTA.State s
  => (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> WTA.WTA Int (s Int)            -- ^ language model
  -> HI.Hyperedge l i1              -- ^ rule
  -> [State (s Int) Int]                  -- ^ base states
  -> [Item (State (s Int) Int) l Double]  -- ^ resulting 'Item'
blowRule mu h2 wta he xs
  = let xr  = doReordering wta (h2 he) $ map _snd xs
        qss = concatMap (\ (qs, d) -> map (\ (x, y) -> (x, y + d)) $ WTA.delta wta qs []) xr
    in  map (\ (q, w) -> Item (Binary (HI.to he) q) (w + mu he) xs $ HI.label he) qss

-- | For a given list l of tuples of lists ai and bi of symbols,
--   generates all sequences s of length |l| such that for every in
--   in {1, ..., |l|} the i-th symbol of s is either in ai or in bi,
--   and there is at least one j in {1, ..., |l|} such that the j-th
--   element of s is in aj.
awesomeSequence :: [([a], [a])] -> ([[a]], [[a]])
awesomeSequence [] = ([], [[]])
awesomeSequence ((ns, os) : xs)
  = ( [ n:as  | n <- ns, as <- nss ++ oss ]
      ++ [ o:ns' | o <- os, ns' <- nss ]
    , [ o:os' | o <- os, os' <- oss ]
    )
  where
    (nss, oss) = awesomeSequence xs
