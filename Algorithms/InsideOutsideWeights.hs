module Algorithms.InsideOutsideWeights (
  insideOutside
, inside
, outside
) where

-- fixpunkt
-- newton
-- hill-climbing


import Data.HyperGraph

import qualified Data.List as L
import qualified Data.Map  as M
import Data.Maybe (fromMaybe)

-- import Debug.Trace


insideOutside target g
  = let mIn = inside g
    in M.unionWith
      (\ (i, _) (_, o) -> (i, o))
      (M.map (\ i -> (i, 0)) mIn)
      (M.map (\ o -> (0, o)) (outside mIn target g))


inside g
  = M.map fst $ go $ M.map (\ es -> (0, es)) (edgesM g)
  where
    go m
      = {-trace "Ding!" $-}
        let m' = insideStep m
        in if maxDiffWith fst m m' < 0.000000000000001
        then m'
        else go m'


insideStep m = M.map (\ (_, es) -> (insideHead m es, es)) m


insideHead m es
  = let step s e = s + eWeight e * insideTail m (eTail e)
    in L.foldl' step 0 es


insideTail m vs
  = let step p v = p * (maybe 0 fst (M.lookup v m))
    in L.foldl' step 1 vs


outside m target g
  = M.map fst $ go $ initOutsideMap m target g
  where
    go m
      = {-trace "Dong!" $-}
        let m' = outsideStep m
        in if maxDiffWith fst m m' < 0.000000000000001
        then m'
        else go m'


outsideStep m
  = M.map
      (\ (_, xs) ->
        ( L.foldl' (\ s (v, w) -> s + maybe 0 fst (M.lookup v m) * w) 0 xs
        , xs)
      )
      m


{-initOutsideMap
  :: (Num t, Num s, Ord v)
  => M.Map v (t1, s, t2)    -- ^ contains the inside weights
  -> HyperGraph v l s
  -> M.Map v (t, [(v, s)])-}
initOutsideMap m target
  = M.insert target (1, [(target, 1)])
  . M.map ((,) 0 . M.toList . M.map sum . M.fromListWith (++))
  . M.fromListWith (++)
  . concatMap
      (\ e ->
        map
          (\ (xs, y, zs) ->
            (y, [(eHead e, [eWeight e * insideList xs * insideList zs])])
          )
          (splits3 (eTail e))
      )
  . edges
  where
    insideList vs
      = let step p v = p * (fromMaybe 0 (M.lookup v m))
        in L.foldl' step 1 vs


-- outside v 


maxDiffWith f m1 m2
  = go (M.elems m1) (M.elems m2) 0
  where
    go (x:xs) (y:ys) maxi = go xs ys $ max maxi $ abs (f x - f y)
    go [] [] maxi = maxi
    go _ _ _ = error "Algorithms.InsideOutsideWeights.maxDiff: Malformed maps."


splits3 []     = []
splits3 [x]    = [([], x, [])]
splits3 (x:xs) = ([], x, xs) : map (mapFst3 (x :)) (splits3 xs)

fst3 (x, _, _) = x
snd3 (_, y, _) = y
trd3 (_, _, z) = z

mapFst3 f (x, y, z) = (f x,   y,   z)
mapSnd3 f (x, y, z) = (  x, f y,   z)
mapTrd3 f (x, y, z) = (  x,   y, f z)


hg
  = hyperGraph
      [ hyperEdge 'A' "AB" 's' 0.2
      , hyperEdge 'A' "BA" 's' 0.3
      , hyperEdge 'A' ""   'a' 0.5
      , hyperEdge 'B' "AA" 's' 0.9
      , hyperEdge 'B' ""   'b' 0.1
      , hyperEdge 'C' "A"  'b' 1    -- C not reachable
      , hyperEdge 'A' "D"  'b' 1    -- D not terminating
      , hyperEdge 'E' "E"  'b' 1    -- E not reachable and not terminating
      ]

