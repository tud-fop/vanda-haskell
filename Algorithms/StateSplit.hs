-- (c) 2011 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

-- | Implementation of state-split grammars based on
--
-- * Slav Petrov, Leon Barrett, Romain Thibaux, Dan Klein.
--   /Learning Accurate, Compact, and Interpretable Tree Annotation./
--   Proc. COLING/ACL 2006 (Main Conference)
--   <http://www.petrovi.de/data/acl06.pdf>
--
-- * Slav Petrov, Dan Klein.
--   /Learning and Inference for Hierarchically Split PCFGs./
--   AAAI 2007 (Nectar Track)
--   <http://www.petrovi.de/data/aaai07.pdf>

module Algorithms.StateSplit where

import Algorithms.EMTrees
import Algorithms.InsideOutsideWeightsTree
import Tools.Miscellaneous(mapFst, mapSnd, sumWith)

import Data.Hypergraph

import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tree as T
import qualified Random as R

-- import Debug.Trace

-- ---------------------------------------------------------------------------

-- | Train the weights of a 'Hypergraph' by refining the states and weights
-- by splitting and merging vertices, and by using the EM algorithm.
train
  :: ( Ord v
     , Ord l
     , RealFloat w, R.Random w
     , Num n, Ord n
     , R.RandomGen gen
     )
  => Int                  -- ^ maximum number of iterations
  -> [Tree l]             -- ^ training 'T.Tree's
  -> v                    -- ^ target vertex
  -> Hypergraph v l w i   -- ^ initial 'Hypergraph'
  -> gen                  -- ^ random number generator
  -> (Hypergraph (v, n) l w [Int], gen)
train maxIt ts target g gen = go maxIt $ train' ts target g gen
  where
    go 0 (x:_)  = x
    go _ [x]    = x
    go n (_:xs) = go (n - 1) xs
    go _ _ = error "StateSplit.train.go"


-- | Train the weights of a 'Hypergraph' by refining the states and weights
-- by splitting and merging vertices, and by using the EM algorithm.
-- All intermediate results are accumulated in a list.
train'
  :: ( Ord v
     , Ord l
     , RealFloat w, R.Random w
     , Num n, Ord n
     , R.RandomGen gen
     )
  => [Tree l]             -- ^ training 'T.Tree's
  -> v                    -- ^ target vertex
  -> Hypergraph v l w i   -- ^ initial 'Hypergraph'
  -> gen                  -- ^ random number generator
  -> [(Hypergraph (v, n) l w [Int], gen)]
train' ts target g0 gen0
  = go 0 1 (mapIds (const []) $ initialize g0) gen0
  where
    target' = initializeVertex target
    go count offset g gen
      = (g, gen)
      : let (g', gen') = splitMergeStep offset ts target' g gen
            count' = S.size $ verticesS g'
            offset' = 2 * offset
        in if offset' <= offset  -- check for overflow
           then []
           else if count' == count
           then [(g', gen')]
           else go count' offset' g' gen'


-- | Perform a split, the EM algorithm an a merge.
splitMergeStep
  :: ( Ord v
     , Ord l
     , RealFloat w, R.Random w
     , Num n, Ord n
     , R.RandomGen gen
     )
  => n                        -- ^ split offset
  -> [T.Tree l]               -- ^ training 'T.Tree's
  -> (v, n)                   -- ^ target vertex
  -> Hypergraph (v, n) l w i  -- ^ initial 'Hypergraph'
  -> gen                      -- ^ random number generator
  -> (Hypergraph (v, n) l w [Int], gen)
splitMergeStep offset ts target g0 gen
  = {-trace "splitMergeStep" $-}
    let
    (_, (g1, gen'))
      = mapSnd (mapFst properize)
      $ mapSnd (flip (randomizeWeights 10) gen)
      $ mapAccumIds (\ (i : is) _ -> (is, i)) [0 ..]
      $ split offset (target ==) g0
    training
      = map (\ t -> (M.singleton target 1, edgeTree find [target] t, 1)) ts
      where
        find len lab
          =  M.findWithDefault M.empty lab
          $ IM.findWithDefault M.empty len m
        m = IM.map (M.map (M.fromListWith (++)) . M.fromListWith (++))
          . IM.fromListWith (++)
          . map (\ e -> (length (eTail e), [(eLabel e, [(eHead e, [e])])]))
          $ edges g1
    wM
      = forestEM
          (map (map eId) . M.elems $ edgesM g1)
          training
          eId
          (\ w n -> w < 0.0001 || n > 100)
          (M.fromList . map (\ e -> (eId e, eWeight e)) $ edges g1)
    getWeight
      = flip (M.findWithDefault err) wM . eId
      where err = error "Algorithms.StateSplit.splitMergeStep.getWeight"
    ios
      = map (\ (m, g, _) -> let im = inside getWeight g in (im, outside getWeight im g m)) training
    toMerge
      = S.fromList
      $ map fst
      $ filter ((0.99 <) . snd)
      $ M.toList
      $ deltasLikelihood offset target ios
--       = let vs = map (mapSnd (offset +) . fst)
--                 . L.sortBy (compare `on` snd)
--                 $ M.toList
--                 $ deltasLikelihood offset target ios
--         in S.fromList $ drop (length vs `div` 2) vs
    mergeVertex v@(v', n)
      = if S.member v toMerge then (v', n - offset) else v
    g2 = dropUnreachables target $ dropZeroWeighted $ mapWeights' getWeight g1
              -- < this is not mentioned in the paper, but it seems reasonable
    g3 = merge mergeVertex g2
  in {-seq i
  $ trace "=== Training Hypergraphs ========================================="
  $ trace (unlines $ map (\ (_, g, _) -> drawHypergraph g) training)
  $ trace ( "Could not parse "
            ++ show (length $ filter (\ (_, g, _) -> null $ edges g) training)
            ++ " from "
            ++ show (length training)
            ++ " trees."
          )
  $ trace "=== Split Hypergraph ============================================="
  $ trace (drawHypergraph $ mapIds (\ i -> (fromJust $ M.lookup i wM, i)) g1)
  $ trace "=== Zero-free Hypergraph ========================================="
  $ trace (drawHypergraph $ g2)
  $ trace "=== Deltas Likelihood ============================================"
  $ trace (unlines $ map show $ M.toList $ deltasLikelihood offset target ios)
  $ trace "=== Vertices to Merge ============================================"
  $ trace (unlines $ map show $ S.toList $ toMerge)
  $ trace "=== Merged Hypergraph ============================================"
  $ trace (drawHypergraph $ g3)
  $ trace "=================================================================="
  $ -} (g3, gen')


deltasLikelihood
  :: (Ord v, Fractional w, Num n, Ord n)
  => n
  -> (v, n)
  -> [(T.Tree (M.Map (v, n) w), T.Tree (M.Map (v, n) w))]
  -> M.Map (v, n) w
deltasLikelihood offset target ios
  = for M.empty ios $ \ m'' (ti, to) ->
      forT m'' (zipT ti to) $ \ m' (im, om) ->
        for m' (M.keys om) $ \ m v1 ->
          if snd v1 >= offset || v1 == target
          then m
          else
            let v2 = mapSnd (offset +) v1
                s = sum
                      [ M.findWithDefault 0 v im * M.findWithDefault 0 v om
                      | v <- M.keys om
                      , v /= v1
                      , v /= v2
                      ]
                i1 = M.findWithDefault 0 v1 im
                o1 = M.findWithDefault 0 v1 om
                i2 = M.findWithDefault 0 v2 im
                o2 = M.findWithDefault 0 v2 om
                p = (s + 0.5 * (i1 + i2) * (o1 + o2)) / (s + i1 * o1 + i2 * o2)
                        -- in the paper special factors are used instead of 0.5
            in M.insertWith' (*) v2 p m
  where
    for x ys f = L.foldl' f x ys
    zipT (T.Node l1 f1) (T.Node l2 f2) = Node (l1, l2) $ zipWith zipT f1 f2
    forT x t f = go x t where go acc (T.Node l ts) = L.foldl' go (f acc l) ts

-- ---------------------------------------------------------------------------

-- | Make a single vertex splitable.
initializeVertex :: (Num n) => v -> (v, n)
initializeVertex v = (v, 0)


-- | Makes the vertices of a 'Hypergraph' splitable without changing the
-- semantics of the 'Hypergraph'.
initialize
  :: (Num n, Ord n, Ord v)
  => Hypergraph  v     l w i
  -> Hypergraph (v, n) l w i
initialize g = mapVerticesMonotonic initializeVertex g


-- | Do a state-splitting step.
split
  :: (Fractional w, Num n, Ord v, Ord n)
  => n
  -> ((v, n) -> Bool)
  -> Hypergraph (v, n) l w i
  -> Hypergraph (v, n) l w i
split offset dontSplit g
  = hypergraph
      [ hyperedge hd tl (eLabel e) w (eId e)
      | e <- edges g
      , let tls = combinations . map splitV $ eTail e
      , let w   = eWeight e / fromIntegral (length tls)
      , hd <- splitV (eHead e)
      , tl <- tls
      ]
  where
    splitV v@(x, n) | dontSplit v = [v]
                    | otherwise   = [v, (x, n + offset)]


-- | Do a state-merging step.
merge
  :: (Fractional w, Ord v, Ord k, Ord l, Ord i)
  => (v -> k)
  -> Data.Hypergraph.Hypergraph v l w  i
  -> Data.Hypergraph.Hypergraph k l w [i]
merge mergeState
  = hypergraph
  . map
      (\ ((hd, tl, l), es) ->
        let w = sumWith eWeight es
                / (fromIntegral . S.size . S.fromList . map eHead $ es)
        in hyperedge hd tl l w (map eId es)
      )
  . M.toList
  . partition
      (\ e ->
        ( mergeState (eHead e)
        , map mergeState (eTail e)
        , eLabel e
        )
      )
  . edges


maxSplit :: (Ord n) => Hypergraph (v, n) l w i -> n
maxSplit = maximum . map snd . vertices

-- ---------------------------------------------------------------------------

-- | Partition a list with respect to a function; e.g.
--
-- > partition (flip mod 3) [0 .. 9]
-- > == fromList [(0,[9,6,3,0]), (1,[7,4,1]), (2,[8,5,2])]
partition :: (Ord k) => (a -> k) -> [a] -> M.Map k [a]
partition f = M.fromListWith (++) . map (\ x -> (f x, [x]))

-- ---------------------------------------------------------------------------

-- | Create a list of lists of all possible combinations of the given elements
-- of the given length respectively.
--
-- > variation 2 [0 .. 1] == [[0,0],[1,0],[0,1],[1,1]]
variation
  :: (Num n)
  => n      -- ^ Length of the combinations.
  -> [a]    -- ^ Given elements.
  -> [[a]]
variation 0 _  = [[]]
variation k xs = concatMap (\ys -> map (:ys) xs) (variation (k-1) xs)


-- | Build a list of all possible combinations of sublist elements by taking
-- an element of each sublist, respectively; e.g.
-- @combinations [[-1, -2], [1, 2]] == [[-1, 1], [-2, 1], [-1, 2], [-2, 2]]@.
combinations :: [[a]] -> [[a]]
combinations []
  = [[]]
combinations (x : xs)
  = [ y : ys
    | ys <- combinations xs
    , y  <- x
    ]

-- ---------------------------------------------------------------------------

-- | Checks, if a given 'Hypergraph' is equivalent to the 'Hypergraph' after
-- splitting and a merging everything back.
--
-- The function takes numerical imprecisions into account.
prop_splitMerge
  :: (Ord v, Integral n, Ord l, Ord i, Fractional w, Ord w)
  => Hypergraph (v, n) l w i -> Bool
prop_splitMerge g
  = let offset = 1 + maxSplit g
        mergeState (q, n) = (q, n `mod` offset)
        g' = merge mergeState . split offset (const False) $ g
    in eqEdges (edges g) (edges g')
    where
      x ~~ y = abs (x - y) < 0.0000001
      eqEdge e1 e2
        =  eHead  e1 == eHead  e2
        && eTail  e1 == eTail  e2
        && eLabel e1 == eLabel e2
        && eWeight e1 ~~ eWeight e2
      eqEdges xs ys
        =  and (map (\ x -> any (eqEdge x) ys) xs)
        && and (map (\ y -> any (eqEdge y) xs) ys)
