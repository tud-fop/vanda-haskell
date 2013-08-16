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
  = let es  = filter ((/=) 0 . HI.arity) . HI.edges $ hg
        es0 = filter ((==) 0 . HI.arity) . HI.edges $ hg
        is0 = flip map es0 $ initRule mu h2 lm
        ns0 = M.map S.fromList . M.fromListWith (++) . map (\x -> (_fst x, [x])) . map _to $ is0
        go cs os ns its
          = let is = [ (HI.to e, lst)
                     | e <- es
                     , let lst = [ blowRule mu h2 lm e s
                                 | s <- fst
                                      . awesomeSequence
                                      $ [ (nLst, oLst)
                                        | q <- HI.from e
                                        , let nLst = S.toList $ M.findWithDefault S.empty q ns
                                        , let oLst = S.toList $ M.findWithDefault S.empty q os
                                        ]
                                 , S.notMember s cs
                                 ]
                     , not $ null lst
                     ]
                cs' = flip S.union cs
                    . S.fromList . map _from
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
    in  Item (CState (HI.to he) st)
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
  -> [CState Int]                   -- ^ base states
  -> Item (CState Int) l Double     -- ^ resulting 'Item'
blowRule mu h2 lm he xs
  = let xr      = doReordering (h2 he) $ map _snd xs
        x       = deltaS lm xr []
        w1      = deltaW lm xr [] + mu he
    in  Item (CState (HI.to he) x) w1 xs $ HI.label he

-- | For a given list l of tuples of lists ai and bi of symbols,
--   generates all sequences s of length |l| such that the i-th
--   symbol of s is either in ai or in bi, and there is at least
--   one j such that the j-th element of s is in aj.
awesomeSequence :: [([a], [a])] -> ([[a]], [[a]])
awesomeSequence [] = ([], [[]])
awesomeSequence ((ns, os) : xs)
  = ( [ n:as  | n <- ns, as <- nss ++ oss ]
      ++ [ o:ns' | o <- os, ns' <- nss ]
    , [ o:os' | o <- os, os' <- oss ]
    )
  where
    (nss, oss) = awesomeSequence xs
