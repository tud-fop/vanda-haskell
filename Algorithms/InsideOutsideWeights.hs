module Algorithms.InsideOutsideWeights
  ( insideOutside
  ) where

-- fixpunkt
-- newton
-- hill-climbing


import Data.HyperGraph

import qualified Data.List as L
import qualified Data.Map  as M
import Data.Maybe (fromMaybe)

-- import Debug.Trace

zero = 0
one = 1


insideOutside g
  = go $ M.map (\ es -> (es, 0, 0)) (edgesM g)
  where
    go m
      = {-trace "Ding!" $-}
        let m' = step m
        in if maxDiffWith snd3 m m' < 0.000000000000001
        then m'
        else go m'
    m = M.map (\ es -> (es, 0, 0)) (edgesM g)
    step m = M.map (\ (es, i, o) -> (es, insideHead m es, o)) m


insideHead m es
  = let step s e = s + eWeight e * insideTail m (eTail e)
    in L.foldl' step 0 es


insideTail m vs
  = let fromJust' = fromMaybe
          $ error "Algorithms.InsideOutsideWeights.insideTail: Malformed map."
        step p v = p * (snd3 . fromJust' . M.lookup v $ m)
    in L.foldl' step 1 vs


maxDiffWith f m1 m2
  = go (M.elems m1) (M.elems m2) 0
  where
    go (x:xs) (y:ys) maxi = go xs ys $ max maxi $ abs (f x - f y)
    go [] [] maxi = maxi
    go _ _ _ = error "Algorithms.InsideOutsideWeights.maxDiff: Malformed maps."


fst3 (x, _, _) = x
snd3 (_, x, _) = x
trd3 (_, _, x) = x

mapSnd3 f (x, y, z) = (x, f y, z)


hg
  = hyperGraph
      [ hyperEdge 'A' "AB" 's' 0.2
      , hyperEdge 'A' "BA" 's' 0.3
      , hyperEdge 'A' ""   'a' 0.5
      , hyperEdge 'B' "AA" 's' 0.9
      , hyperEdge 'B' ""   'b' 0.1
      ]

