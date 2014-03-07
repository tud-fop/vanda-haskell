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
import Data.Function


-- * Intersection strategies

-- | Intersects IRTG and n-gram model, emits 'Item's.
intersectBHPS
  :: LM a
  => (Int -> Int)
  -> a                            -- ^ language model
  -> (Int, l)                     -- ^ initial state
  -> (HI.Hyperedge l i -> Double) -- ^ rule weights
  -> (HI.Hyperedge l i -> [NTT])  -- ^ tree to tree homomorphism
  -> HI.Hypergraph l i            -- ^ RTG hypergraph
  -> [Item (State (WTABHPS.State' Int) Int) l Double]
                                  -- ^ resulting list of 'Items'
intersectBHPS rel lm lbl mu h1 hg
  = let wta = WTABHPS.makeWTA lm rel
            . L.nub
            . concatMap (map (\(T x) -> x))
            . map h1 $ filter ((==) 0 . HI.arity)
            $ HI.edges hg
    in  intersect' id wta lbl mu h1 hg

-- | Intersects IRTG and n-gram model, emits 'Item's.
intersectUnsmoothed
  :: LM a
  => (Int -> Int)
  -> a                             -- ^ language model
  -> (Int, l)                      -- ^ initial state
  -> (HI.Hyperedge l i1 -> Double) -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])  -- ^ tree to tree homomorphism
  -> HI.Hypergraph l i1            -- ^ RTG hypergraph
  -> [Item (State (WTA.State' Int) Int) l Double]
                                   -- ^ resulting list of 'Items'
intersectUnsmoothed rel lm
  = let wta = WTA.unsmoothedWTA rel lm
    in  intersect' id wta

-- | Intersects IRTG and n-gram model, emits 'Item's.
intersectSmoothed
  :: LM a
  => (Int -> Int)
  -> a                             -- ^ language model
  -> (Int, l)                      -- ^ initial state
  -> (HI.Hyperedge l i1 -> Double) -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])  -- ^ tree to tree homomorphism
  -> HI.Hypergraph l i1            -- ^ RTG hypergraph
  -> [Item (State (WTA.State' Int) Int) l Double]
                                   -- ^ resulting list of 'Items'
intersectSmoothed rel lm
  = let wta = WTA.smoothedWTA rel lm
    in  intersect' id wta

-- | Intersects IRTG and n-gram model, emits 'Item's.
intersectUnsmoothedPruning
  :: LM a
  => (Int -> Int)
  -> Int                           -- ^ pruning length
  -> a                             -- ^ language model
  -> (Int, l)                      -- ^ initial state
  -> (HI.Hyperedge l i1 -> Double) -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])  -- ^ tree to tree homomorphism
  -> HI.Hypergraph l i1            -- ^ RTG hypergraph
  -> [Item (State (WTA.State' Int) Int) l Double]
                                   -- ^ resulting list of 'Items'
intersectUnsmoothedPruning rel c lm
  = let wta = WTA.unsmoothedWTA rel lm
    in  intersect' (take c . L.sortBy (compare `on` _wt)) wta

intersectPruning
  :: LM a
  => (Int -> Int)
  -> Int                            -- ^ pruning length
  -> a                              -- ^ language model
  -> (Int, l)                       -- ^ initial state
  -> (HI.Hyperedge l i1 -> Double)  -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])   -- ^ tree to tree homomorphism
  -> HI.Hypergraph l i1             -- ^ RTG hypergraph
  -> [Item (State (WTA.State' Int) Int) l Double]
                                    -- ^ resulting list of 'Items'
intersectPruning rel c lm
  = let wta = WTA.smoothedWTA rel lm
    in  intersect' (take c . L.sortBy (compare `on` _wt)) wta

ioProductPruning
  :: LM a
  => (Int -> Int)
  -> Int                           -- ^ pruning length
  -> a                             -- ^ language model
  -> (Int, l)                      -- ^ initial state
  -> [Int]                         -- ^ word
  -> (HI.Hyperedge l i1 -> Double) -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])  -- ^ tree to tree homomorphism
  -> (HI.Hyperedge l i1 -> [NTT])  -- ^ tree to string homomorphism
  -> HI.Hypergraph l i1            -- ^ RTG hypergraph
  -> [Item (IOPState (Int, Int) Int (WTA.State' Int)) l Double]
                                   -- ^ resulting list of 'Items'
ioProductPruning rel c lm
  = let wta = WTA.smoothedWTA rel lm
    in  ioProduct' (take c . L.sortBy (compare `on` ((\(x, y) -> y - x) . _inp . _to))) wta


-- * Intersection functions

intersect'
  :: (Ord (s Int), WTA.State s)
  => ([Item (State (s Int) Int) l Double] -> [Item (State (s Int) Int) l Double])
  -> WTA.WTA Int (s Int)           -- ^ language model
  -> (Int, l)                      -- ^ initial state
  -> (HI.Hyperedge l i1 -> Double) -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])  -- ^ tree to tree homomorphism
  -> HI.Hypergraph l i1            -- ^ RTG hypergraph
  -> [Item (State (s Int) Int) l Double]
                                   -- ^ resulting list of 'Items'
intersect' sel wta (oi, lbl) mu h1 hg
  = let (es0, es)
            = L.partition ((==) 0 . HI.arity)
            $ HI.edges hg
        is0 = M.map (concatMap (initRule mu h1 wta))
            . M.fromListWith (++)
            $ map (\ e -> (HI.to e, [e])) es0
        ns0 = M.map (S.fromList . map _to) is0
        go os ns its
          = let is = M.fromListWith (++)
                   $ [ (HI.to e, concat lst)
                     | e <- es
                     , let lst = [ blowRule mu h1 wta e s
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

ioProduct'
  :: (Ord (s Int), WTA.State s)
  => (  [Item (IOPState (Int, Int) Int (s Int)) l Double]
     -> [Item (IOPState (Int, Int) Int (s Int)) l Double]
     )                                                   -- ^ pruning function
  -> WTA.WTA Int (s Int)                                   -- ^ language model
  -> (Int, l)                                        -- ^ old init, rule label
  -> [Int]                                                           -- ^ word
  -> (HI.Hyperedge l i1 -> Double)                           -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])               -- ^ tree to tree homomorphism
  -> (HI.Hyperedge l i1 -> [NTT])             -- ^ tree to string homomorphism
  -> HI.Hypergraph l i1                                    -- ^ RTG hypergraph
  -> [Item (IOPState (Int, Int) Int (s Int)) l Double]
                                                -- ^ resulting list of 'Items'
ioProduct' sel wta (oi, lbl) word mu h1 h2 hg
  = let flt (T i)  = L.elem i word
        flt (NT _) = True
        (es0, es)
            = L.partition ((==) 0 . HI.arity)
            . filter (all flt . h2)
            $ HI.edges hg
        is0 = M.filter (not . null)
            . M.map (concatMap (initRule' mu h1 h2 wta word))
            . M.fromListWith (++)
            $ map (\ e -> (HI.to e, [e])) es0
        ns0 = M.map (S.fromList . map _to) is0
                                                  -- reachable states from is0
        go os ns its
          = let is = M.fromListWith (++)
                   $ [ (HI.to e, lst)
                     | e <- es
                     , let lst = concat
                               $ [ blowRule' mu h1 h2 wta word e s
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
    in  makeSingleEndState'
         wta 
         (\ (Trinary i c _) -> and [oi == c, i == (0, length word)])
         Initial
         lbl
      $ go M.empty ns0 is0


-- * Deduction rules

-- | Emits an initial 'Item'.
initRule
  :: WTA.State s
  => (HI.Hyperedge l i1 -> Double)        -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])         -- ^ tree to tree homomorphism
  -> WTA.WTA Int (s Int)                  -- ^ language model
  -> HI.Hyperedge l i1                    -- ^ rule
  -> [Item (State (s Int) Int) l Double]  -- ^ resulting 'Item'
initRule mu h1 lm he
  = let h (T x)  = x
        h (NT _) = error "Non-terminal in nullary rule."
        sts      = map h $ h1 he
        xs       = WTA.delta lm [] sts
    in  map (\ (st, w1)
            -> Item (Binary (HI.to he) st)
                (mu he + w1)
                []
                (HI.label he))
            xs

initRule'
  :: WTA.State s
  => (HI.Hyperedge l i1 -> Double)        -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])         -- ^ tree to tree homomorphism
  -> (HI.Hyperedge l i1 -> [NTT])         -- ^ tree to string homomorphism
  -> WTA.WTA Int (s Int)                  -- ^ language model
  -> [Int]                                -- ^ word
  -> HI.Hyperedge l i1                    -- ^ rule
  -> [Item (IOPState (Int, Int) Int (s Int)) l Double]
                                          -- ^ resulting 'Item'
initRule' mu h1 h2 lm word he
  = let unpack (T x)  = x
        unpack (NT _) = error "Non-terminal in nullary rule."
        extractRanges i y x
          =  (if x `L.isPrefixOf` y then [(i, i + length x)] else [])
          ++ (if length x <= length y then extractRanges (i + 1) (tail y) x else [])
    in  [ Item (Trinary s1 (HI.to he) s2) (mu he + w1) [] (HI.label he)
        |  s1      <- extractRanges 0 word . map unpack $ h2 he
        , (s2, w1) <- WTA.delta lm []      . map unpack $ h1 he
        ]

-- | Combines 'Item's by a rule. The 'Item's and the rule must
--   match (not checked).
blowRule
  :: WTA.State s
  => (HI.Hyperedge l i1 -> Double)       -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])        -- ^ tree to string homomorphism
  -> WTA.WTA Int (s Int)                 -- ^ language model
  -> HI.Hyperedge l i1                   -- ^ rule
  -> [State (s Int) Int]                 -- ^ base states
  -> [Item (State (s Int) Int) l Double] -- ^ resulting 'Item'
blowRule mu h1 wta he xs
  = let xr  = doReordering wta (h1 he) $ map _snd xs
        qss = concatMap (\(qs, d) -> map (\(x, y) -> (x, y + d))
                                   $ WTA.delta wta qs []
                        ) xr
    in  map (\(q, w) -> Item (Binary (HI.to he) q)
                             (w + mu he)
                             xs
                             (HI.label he)
            ) qss

blowRule'
  :: WTA.State s
  => (HI.Hyperedge l i1 -> Double)        -- ^ rule weights
  -> (HI.Hyperedge l i1 -> [NTT])         -- ^ tree to tree homomorphism
  -> (HI.Hyperedge l i1 -> [NTT])         -- ^ tree to string homomorphism
  -> WTA.WTA Int (s Int)                  -- ^ language model
  -> [Int]                                -- ^ word
  -> HI.Hyperedge l i1                    -- ^ rule
  -> [(IOPState (Int, Int) Int (s Int))]  -- ^ base states
  -> [Item (IOPState (Int, Int) Int (s Int)) l Double]
                                          -- ^ resulting 'Item'
blowRule' mu h1 h2 wta word he xs
  = let xr  = doReordering wta (h1 he) $ map _out xs
        extractRanges i y x
          =  (if x `L.isPrefixOf` y then [(i, i + length x)] else [])
          ++ (if length x <= length y then extractRanges (i + 1) (tail y) x else [])
        unpack (T x)  = extractRanges 0 word [x]
        unpack (NT x) = [_inp $ (L.!!) xs x]
    in  [ Item (Trinary s1 (HI.to he) s2) (mu he + w) xs (HI.label he)
        | s1 <- concatenateRanges . map unpack $ h2 he
        , (s2, w) <- concatMap (\(qs, d) -> map (\(x, y) -> (x, y + d)) $ WTA.delta wta qs []) xr
        ]
