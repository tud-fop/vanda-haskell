-- | Computation of inside and outside weights for 'HyperGraph's.
--
-- The weights are computed by a fixpoint approximation.
-- Alternative approaches for future implementations could be Newton's method
-- or hill climbing.
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


-- | Computes the inside and outside weights for a given 'HyperGraph'.
insideOutside
  :: (Fractional w, Ord v, Ord w)
  => v                 -- ^ target node
  -> HyperGraph v l w
  -> M.Map v (w, w)    -- ^ maps a vertex to (inside weight, outside weight)
insideOutside target g
  = let mIn = inside g
    in M.unionWith
      (\ (i, _) (_, o) -> (i, o))
      (M.map (\ i -> (i, 0)) mIn)
      (M.map (\ o -> (0, o)) (outside mIn target g))


-- | Computes the inside weights for a given 'HyperGraph'.
inside
  :: (Fractional w, Ord v, Ord w)
  => HyperGraph v l w
  -> M.Map v w
inside g
  = M.map fst $ go $ M.map (\ es -> (0, es)) (edgesM g)
  where
    go m
      = {-trace "Ding!" $-}
        let m' = insideStep m
        in if maxDiffWith fst m m' < 0.000000000000001
        then m'
        else go m'


-- | Do one iteration step for the fixpoint computation of the inside weights.
insideStep
  :: (Num w, Ord v)
  => M.Map v (w, [HyperEdge v l w])
  -> M.Map v (w, [HyperEdge v l w])
insideStep m = M.map (\ (_, es) -> (insideHead m es, es)) m


insideHead
  :: (Num w, Ord v)
  => M.Map v (w, a)
  -> [HyperEdge v l w]
  -> w
insideHead m es
  = let step s e = s + eWeight e * insideTail m (eTail e)
    in L.foldl' step 0 es


insideTail
  :: (Num w, Ord v)
  => M.Map v (w, a)
  -> [v]
  -> w
insideTail m vs
  = let step p v = p * (maybe 0 fst (M.lookup v m))
    in L.foldl' step 1 vs


-- | Computes the outside weights of a given 'Data.HyperGraph'.
outside
  :: (Fractional w, Ord v, Ord w)
  => M.Map v w         -- ^ inside weights
  -> v                 -- ^ target node
  -> HyperGraph v l w
  -> M.Map v w
outside m target g
  = M.map fst $ go $ initOutsideMap m target g
  where
    go m
      = {-trace "Dong!" $-}
        let m' = outsideStep m
        in if maxDiffWith fst m m' < 0.000000000000001
        then m'
        else go m'


-- | Do one iteration step for the fixpoint computation of the outside weights.
outsideStep
  :: (Num w, Ord v)
  => M.Map v (w, [(v, w)])
  -> M.Map v (w, [(v, w)])
outsideStep m
  = M.map
      (\ (_, xs) ->
        ( L.foldl' (\ s (v, w) -> s + maybe 0 fst (M.lookup v m) * w) 0 xs
        , xs)
      )
      m


-- | Initialize the data structure used for computing the outside weights.
initOutsideMap ::
  (Num w, Ord v)
  => M.Map v w
  -> v                      -- ^ target node
  -> HyperGraph v l w       -- ^ inside weights
  -> M.Map v (w, [(v, w)])
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


-- | Compute the maximum difference between corresponding Elements of two maps.
-- /Both maps must contain exactly the same keys for this function to work!/
-- The given function is used to extract the values to compare from values of
-- the maps.
maxDiffWith
  :: (Ord b, Num b)
  => (a -> b)
  -> M.Map k1 a
  -> M.Map k2 a
  -> b
maxDiffWith f m1 m2
  = go (M.elems m1) (M.elems m2) 0
  where
    go (x:xs) (y:ys) maxi = go xs ys $ max maxi $ abs (f x - f y)
    go [] [] maxi = maxi
    go _ _ _ = error "Algorithms.InsideOutsideWeights.maxDiff: Malformed maps."


-- | Build a list of all possible splits @(xs, y, ys)@ of a list @zs@, such
-- that @zs == xs ++ [y] ++ ys@. For example
-- @splits3 [1, 2, 3] == [([], 1, [2, 3]), ([1], 2, [3]), ([1, 2], 3, [])]@.
splits3 :: [a] -> [([a], a, [a])]
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

