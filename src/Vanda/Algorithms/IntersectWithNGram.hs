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

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.WTA as WTA
import qualified Vanda.Grammar.NGrams.WTA as WTA
import qualified Vanda.Grammar.NGrams.WTA_BHPS as WTABHPS
import qualified Vanda.Hypergraph.IntHypergraph as HI

import Vanda.Algorithms.IntersectWithNGramUtil
import Vanda.Grammar.LM
import Data.NTT

-- | Intersects IRTG and n-gram model, emits 'Item's.
intersectBHPS
  :: LM a
  => a                            -- ^ language model
  -> (Int, l)                     -- ^ initial state
  -> (HI.Hyperedge l i -> Double) -- ^ rule weights
  -> (HI.Hyperedge l i -> [NTT])  -- ^ tree to string homomorphism
  -> HI.Hypergraph l i            -- ^ RTG hypergraph
  -> [Item (State (WTABHPS.State' Int) Int) l Double]
                                  -- ^ resulting list of 'Items'
intersectBHPS lm lbl mu h2 hg
  = let wta = WTABHPS.makeWTA lm
            . L.nub
            . concatMap (map (\(T x) -> x))
            . map h2 $ filter ((==) 0 . HI.arity)
            $ HI.edges hg
    in  intersect' id wta lbl mu h2 hg

-- | Intersects IRTG and n-gram model, emits 'Item's.
intersectSmoothed
  :: LM a
  => a                             -- ^ language model
  -> (Int, l)                      -- ^ initial state
  -> (HI.Hyperedge l i1 -> Double) -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])  -- ^ tree to string homomorphism
  -> HI.Hypergraph l i1            -- ^ RTG hypergraph
  -> [Item (State (WTA.State' Int) Int) l Double]
                                   -- ^ resulting list of 'Items'
intersectSmoothed lm mu h2 hg
  = let wta = WTA.smoothedWTA lm
    in  intersect' id wta mu h2 hg

intersectPruning
  :: LM a
  => Int                            -- ^ pruning length
  -> a                              -- ^ language model
  -> (Int, l)                       -- ^ initial state
  -> (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to string homomorphism
  -> HI.Hypergraph l i1             -- ^ RTG hypergraph
  -> [Item (State (WTA.State' Int) Int) l Double]
                                    -- ^ resulting list of 'Items'
intersectPruning c lm mu h2 hg
  = let wta = WTA.smoothedWTA lm
    in  intersect' (take c . L.sortBy (\x y -> compare (_wt x) (_wt y))) wta mu h2 hg

-- | Intersects IRTG and n-gram model, emits 'Item's.
intersectUnsmoothed
  :: LM a
  => a                             -- ^ language model
  -> (Int, l)                      -- ^ initial state
  -> (HI.Hyperedge l i1 -> Double) -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])  -- ^ tree to string homomorphism
  -> HI.Hypergraph l i1            -- ^ RTG hypergraph
  -> [Item (State (WTA.State' Int) Int) l Double]
                                   -- ^ resulting list of 'Items'
intersectUnsmoothed lm mu h2 hg
  = let wta = WTA.unsmoothedWTA lm
    in  intersect' id wta mu h2 hg

intersect'
  :: (Ord (s Int), WTA.State s)
  => ([Item (State (s Int) Int) l Double] -> [Item (State (s Int) Int) l Double])
  -> WTA.WTA Int (s Int)           -- ^ language model
  -> (Int, l)                      -- ^ initial state
  -> (HI.Hyperedge l i1 -> Double) -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])  -- ^ tree to string homomorphism
  -> HI.Hypergraph l i1            -- ^ RTG hypergraph
  -> [Item (State (s Int) Int) l Double]
                                   -- ^ resulting list of 'Items'
intersect' sel wta (oi, lbl) mu h2 hg
  = let es  = filter ((/=) 0 . HI.arity) $ HI.edges hg
        es0 = filter ((==) 0 . HI.arity) $ HI.edges hg
        is0 = M.map (concatMap (initRule mu h2 wta))
            . M.fromListWith (++)
            $ map (\ e -> (HI.to e, [e])) es0
        ns0 = M.map (S.fromList . map _to) is0
        go os ns its
          = let is = M.fromListWith (++)
                   $ [ (HI.to e, concat lst)
                     | e <- es
                     , let lst = [ blowRule mu h2 wta e s
                                 | s <- fst
                                      $ awesomeSequence
                                        [ (nLst, oLst)
                                        | q <- HI.from e
                                        , let
                                            nLst
                                              = S.toList
                                              $ M.findWithDefault S.empty q ns
                                        , let
                                            oLst
                                              = S.toList
                                              $ M.findWithDefault S.empty q os
                                        ]
                                 ]
                     , not $ null lst
                     ]
                os' = M.unionWith S.union os ns
                is' = M.map sel $ M.unionWith (++) is its
                ss' = M.map (S.fromList . map _to) is'
                ns' = M.unionWith S.difference ss' os'
            in  if   M.null is
                then concat $ M.elems its
                else go os' ns' is'
    in  makeSingleEndState wta ((==) oi . _fst) Nullary lbl
      $ go M.empty ns0 is0


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
        qss = concatMap (\(qs, d) -> map (\(x, y) -> (x, y + d))
                                   $ WTA.delta wta qs []) xr
    in  map (\(q, w) -> Item (Binary (HI.to he) q)
                             (w + mu he)
                             xs
                             (HI.label he)
            ) qss

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
