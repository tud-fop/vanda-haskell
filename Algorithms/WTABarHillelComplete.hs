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

module Algorithms.WTABarHillelComplete
( intersect
, intersectTransitionCount
) where

import Data.Hypergraph
import qualified Data.WSA as WSA
import qualified Data.WTA as WTA

import qualified Data.Map as M


intersect
  :: (Ord p, Ord q, Eq t, Num w)
  => WSA.WSA p t w -> WTA.WTA q t w i -> WTA.WTA (p, q, p) t w i
intersect wsa wta
  = WTA.wtaCreate (intersectFinals wsa wta) (intersectTrans wsa wta)


intersectTransitionCount
  :: (Eq t, Num w) => WSA.WSA p t w -> WTA.WTA q t w i -> Int
intersectTransitionCount wsa wta
  = let wsaStateCnt = length $ WSA.states wsa
    in (length $ intersectTransLeaf wsa wta)
       + (sum [ wsaStateCnt ^ (length (eTail tt) + 1)
              | tt <- edges $ WTA.toHypergraph wta
              , not $ null $ eTail tt
              ])


intersectTrans
  :: (Eq t, Num w)
  => WSA.WSA p t w -> WTA.WTA q t w i -> [Hyperedge (p, q, p) t w i]
intersectTrans wsa wta = intersectTransLeaf wsa wta ++ intersectTransNonLeaf wsa wta


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
    | tt <- edges $ WTA.toHypergraph wta
    , null $ eTail tt
    , st <- WSA.transitions wsa
    , eLabel tt == WSA.transTerminal st
    ]


intersectTransNonLeaf
  :: WSA.WSA p t w' -> WTA.WTA q t w i -> [Hyperedge (p, q, p) t w i]
intersectTransNonLeaf wsa wta
  = [ hyperedge
          (p, eHead tt, p')
          states
          (eLabel tt)
          (eWeight tt)
          (eId tt)
    | tt <- edges $ WTA.toHypergraph wta
    , not $ null $ eTail tt
    , (p, states, p') <- combine (WSA.states wsa) (eTail tt)
    ]


intersectFinals
  :: (Num w) => WSA.WSA p t' w -> WTA.WTA q t w i -> [((p, q, p), w)]
intersectFinals wsa wta
  = [ ((p, q, p'), w1 * w2 * w3)
    | (p , w1) <- WSA.initialWeights wsa
    , (q , w2) <- M.toList $ WTA.finalWeights wta
    , (p', w3) <- WSA.finalWeights wsa
    ]


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