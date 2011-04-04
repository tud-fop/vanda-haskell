-- Copyright (c) 2010, Toni Dietze

module WTABarHillelComplete(intersect, intersectTransitionCount) where

import qualified Data.WSA as WSA
import qualified Data.WTA as WTA


intersect
  :: (Ord p, Ord q, Eq t, Num w)
  => WSA.WSA p t w -> WTA.WTA q t w -> WTA.WTA (p, q, p) t w
intersect wsa wta
  = WTA.create (intersectTrans wsa wta) (intersectFinals wsa wta)


intersectTransitionCount
  :: (Eq t, Num w) => WSA.WSA p t w -> WTA.WTA q t w -> Int
intersectTransitionCount wsa wta
  = let wsaStateCnt = length $ WSA.states wsa
    in (length $ intersectTransLeaf wsa wta)
       + (sum [ wsaStateCnt ^ (l + 1) | tt <- WTA.transitions wta, not (WTA.transIsLeaf tt), let l = length (WTA.transStates tt) ])


intersectTrans
  :: (Eq t, Num w)
  => WSA.WSA p t w -> WTA.WTA q t w -> [WTA.Transition (p, q, p) t w]
intersectTrans wsa wta = intersectTransLeaf wsa wta ++ intersectTransNonLeaf wsa wta


intersectTransLeaf
  :: (Eq t, Num w)
  => WSA.WSA p t w -> WTA.WTA q t w -> [WTA.Transition (p, q, p) t w]
intersectTransLeaf wsa wta
  = [ WTA.Transition
          (WTA.transTerminal tt)
          (WSA.transStateIn st, WTA.transState tt, WSA.transStateOut st)
          []
          (WSA.transWeight st * WTA.transWeight tt)
    | tt <- WTA.transitions wta
    , WTA.transIsLeaf tt
    , st <- WSA.transitions wsa
    , WTA.transTerminal tt == WSA.transTerminal st
    ]


intersectTransNonLeaf
  :: WSA.WSA p t w' -> WTA.WTA q t w -> [WTA.Transition (p, q, p) t w]
intersectTransNonLeaf wsa wta
  = [ WTA.Transition
          (WTA.transTerminal tt)
          state
          states
          (WTA.transWeight tt)
    | tt <- WTA.transitions wta
    , not (WTA.transIsLeaf tt)
    , (p, states, p') <- combine (WSA.states wsa) (WTA.transStates tt)
    , let state = (p, WTA.transState tt, p')
    ]


intersectFinals
  :: (Num w) => WSA.WSA p t' w -> WTA.WTA q t w -> [((p, q, p), w)]
intersectFinals wsa wta
  = [ ((p, q, p'), w1 * w2 * w3)
    | (p , w1) <- WSA.initialWeights wsa
    , (q , w2) <- WTA.finalWeights wta
    , (p', w3) <- WSA.finalWeights wsa
    ]


combine :: [p] -> [q] -> [(p, [(p, q, p)], p)]
combine ps qs
  = concat [ f p ps qs | p <- ps ]
    where f p ps (q:qs) = [ (p, (p, q, p'):x, p'') | p' <- ps, (_, x, p'') <- f p' ps qs ]
          f p _  _      = [ (p, [], p) ]

{-
variations :: (Num n) => n -> [a] -> [[a]]
variations 0 _  = [[]]
variations n xs = [ y:ys | y <- xs, ys <- variations (n - 1) xs ]
-}