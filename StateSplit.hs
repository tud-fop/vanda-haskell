-- Copyright (c) 2010, Toni Dietze

module StateSplit where

-- import EM -- TODO
import Tools.Miscellaneous(mapFst, mapSnd)

import qualified Data.WTA as WTA

initialize wta = WTA.mapStates (flip (,) 1) wta

split offset wta
  = WTA.create
      (splitTranss (WTA.transitions wta))
      (splitFinals (WTA.finalWeights wta))
    where
      splitters = [id, mapSnd ((+) offset)]
      factor    = 1 / (fromIntegral (length splitters))
      splitTranss ts
        = [ t{WTA.transState = q', WTA.transStates = qs', WTA.transWeight = w'}
          | t   <- ts
          , let q    = WTA.transState  t
          , let qs   = WTA.transStates t
          , let l    = length qs
          , let qs's = map (flip (zipWith id) qs) (variation l splitters)
          , let q's  = map ($ q) splitters
          , let w'   = (factor ^ l) * WTA.transWeight t
          , q'  <- q's
          , qs' <- qs's
          ]
      splitFinals fs
        = [ (q', w')
          | (q, w) <- fs
          , let w'  = factor * w
          , q'     <- map ($ q) splitters
          ]

variation 0 _  = [[]]
variation k xs = concatMap (\ys -> map (:ys) xs) (variation (k-1) xs)

--------------------------------------------------------------------------------

testWTA =
  WTA.create
    [ WTA.Transition 'a' 'b' "c" 1
    , WTA.Transition 'd' 'e' "fg" 1
    , WTA.Transition 'h' 'i' "jkl" 1
    ]
    [ ('b', 0.25)
    , ('e', 0.75)
    ]
