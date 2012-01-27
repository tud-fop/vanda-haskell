-- (c) 2010 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

-- |
-- Maintainer  :  Matthias Büchse, Toni Dietze
-- Stability   :  unbekannt
-- Portability :  portable
--
-- This module computes the intersect of a 'WSA.WSA' and a 'WTA.WTA'. The resulting 'WTA' will recognize the intersection of the languages of both automata.
--
-- See <http://dl.acm.org/citation.cfm?id=1697236.1697238> for theoretical informations about the Bar-Hillel-Algorithm.

module Algorithms.WTABarHillelComplete
( intersect
, intersectTransitionCount
) where

import Data.Hypergraph
import qualified Data.WSA as WSA
import qualified Data.WTA as WTA

import qualified Data.Map as M

-- | Intersect a 'WSA.WSA' and a 'WTA.WTA'.
intersect
  :: (Ord p, Ord q, Eq t, Num w)
  => WSA.WSA p t w -> WTA.WTA q t w i -> WTA.WTA (p, q, p) t w i
intersect wsa wta
  = WTA.wtaCreate (intersectFinals wsa wta) (intersectTrans wsa wta)

-- | Compute the number of transition of the intersect of the 'WSA.WSA' and 'WTA.WTA'.
intersectTransitionCount
  :: (Eq t, Num w) => WSA.WSA p t w -> WTA.WTA q t w i -> Int
intersectTransitionCount wsa wta
  = let wsaStateCnt = length $ WSA.states wsa           -- amount of states in the WSA
    in (length $ intersectTransLeaf wsa wta)            -- amount of terminal-transitions in the intersect-WTA
       + (sum [ wsaStateCnt ^ (length (eTail tt) + 1)   -- amount of nonterminal-transitions in the intersect-WTA
              | tt <- edges $ WTA.toHypergraph wta      -- a transition of the WTA (as a hyperedge)
              , not $ null $ eTail tt                   -- ... that describes the behavior of a not nullary symbol
              ])

-- | Create a list of transitions from a 'WSA' and a 'WTA'. The resulting 'Hypergraph' - represented by the list of 'Hyperedge's - represents the intersect-'WTA'.
intersectTrans
  :: (Eq t, Num w)
  => WSA.WSA p t w -> WTA.WTA q t w i -> [Hyperedge (p, q, p) t w i]
intersectTrans wsa wta = intersectTransLeaf wsa wta ++ intersectTransNonLeaf wsa wta

-- | Create a list of nullary 'Hyperedge's which describes the behavior of terminals.
intersectTransLeaf
  :: (Eq t, Num w)
  => WSA.WSA p t w -> WTA.WTA q t w i -> [Hyperedge (p, q, p) t w i]
intersectTransLeaf wsa wta
  = [ hyperedge
          (WSA.transStateIn st, eHead tt, WSA.transStateOut st)
          []
          (eLabel tt)
          (WSA.transWeight st * eWeight tt)
          (eId tt)
    | tt <- edges $ WTA.toHypergraph wta        -- a transition of the WTA (as a hyperedge)
    , null $ eTail tt                           -- ... that describes the behavior of a nullary symbol
    , st <- WSA.transitions wsa                 -- a transition of the WSA
    , eLabel tt == WSA.transTerminal st         -- ... that produces the nullary symbol
    ]

-- | Create a list of 'Hyperedge's which describes the behavior of nonterminals.
intersectTransNonLeaf
  :: WSA.WSA p t w' -> WTA.WTA q t w i -> [Hyperedge (p, q, p) t w i]
intersectTransNonLeaf wsa wta
  = [ hyperedge
          (p, eHead tt, p')
          states
          (eLabel tt)
          (eWeight tt)
          (eId tt)
    | tt <- edges $ WTA.toHypergraph wta                        -- a transition of the WTA
    , not $ null $ eTail tt                                     -- ... that describes the behavior of a not nullary symbol
    , (p, states, p') <- combine (WSA.states wsa) (eTail tt)
    ]

-- | Create a list indicating the final weights of the new states of the intersect-'WTA'.
intersectFinals
  :: (Num w) => WSA.WSA p t' w -> WTA.WTA q t w i -> [((p, q, p), w)]
intersectFinals wsa wta
  = [ ((p, q, p'), w1 * w2 * w3)
    | (p , w1) <- WSA.initialWeights wsa
    , (q , w2) <- M.toList $ WTA.finalWeights wta
    , (p', w3) <- WSA.finalWeights wsa
    ]

-- | Create a list of all triples ((p_0,q_1,p_1), (p_0,q_1,p_1)(p_1,q_2,p_2)...(p_k-1,q_k,p_k) ,(p_k-1,q_k,p_k))
combine :: [p] -> [q] -> [(p, [(p, q, p)], p)]
combine ps qs
  = concat [ f p qs | p <- ps ]
    where f p (q:qs') = [ (p, (p, q, p'):x, p'') | p' <- ps, (_, x, p'') <- f p' qs' ]
          f p _      = [ (p, [], p) ]

{-
variations :: (Num n) => n -> [a] -> [[a]]
variations 0 _  = [[]]
variations n xs = [ y:ys | y <- xs, ys <- variations (n - 1) xs ]
-}
