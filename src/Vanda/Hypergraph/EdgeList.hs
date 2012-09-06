-- (c) 2012 Matthias Büchse <Matthias.Buechse@tu-dresden.de>
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
-- Maintainer  :  Matthias Buechse
-- Stability   :  unknown
-- Portability :  portable
--
-- Implementation of the 'EdgeList' representation.

module Vanda.Hypergraph.EdgeList
  ( module Vanda.Hypergraph.Basic
  , filterEdges
  , mapNodes
  , mapLabels
  , toSimulation
  , dropNonproducing
  , dropNonproducing'
  , knuth
  ) where

import Prelude hiding ( lookup )

import Control.Arrow ( (***) )
import Control.DeepSeq ( deepseq, NFData (..) )
import qualified Data.Array as A
import Data.Heap ( Prio, Val )
import qualified Data.Heap as H hiding ( Prio, Val )
import qualified Data.IntMap as IM
import qualified Data.Ix as Ix
import Data.List ( foldl' )
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import Data.Maybe
import qualified Data.Queue as Q
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Data.Vector as V

import Debug.Trace

import Vanda.Features
import Vanda.Hypergraph.Basic
import Vanda.Hypergraph.NFData ()

filterEdges
  :: Ord v => (Hyperedge v l i -> Bool) -> EdgeList v l i -> EdgeList v l i
filterEdges p (EdgeList _ es) = EdgeList (nodesL es) (filter p es)

mapLabels
  :: (Hyperedge v l i -> Hyperedge v l' i')
  -> EdgeList v l i
  -> EdgeList v l' i'
mapLabels f (EdgeList vs es) = EdgeList vs (map f es)

mapNodes :: (Ord v, Ord v') => (v -> v') -> EdgeList v l i -> EdgeList v' l i
mapNodes f (EdgeList vs es) 
  = EdgeList (S.fromList $ map f $ S.toList vs) (map (mapHE f) es)

toSimulation :: (Ord v, Ord l) => EdgeList v l i -> Simulation v l i
toSimulation (EdgeList vs es) = Simulation vs lookup
  where
    lookup v l n
      = M.findWithDefault [] (l, n)
      $ a M.! v
    lst = [ (v, M.singleton (l, n) [e])
          | e <- es
          , let v = to e
          , let l = label e
          , let n = arity e {- FIXME: length O(n) -}
          ]
    a = M.union
        (M.fromListWith (M.unionWith (++) ) lst)
        (M.fromList $ zip (S.toList vs) $ repeat M.empty)
        
      -- A.accumArray
      -- (\m (l, n, e) -> M.insertWith (++) (l, n) [e] m)
      -- M.empty
      -- vs
      -- lst



-- | Drops nonproducing nodes and corresponding edges.
dropNonproducing''
  :: forall v l i. (Ord v, NFData v)
  => EdgeList v l i
  -> EdgeList v l i
dropNonproducing'' (EdgeList _ es)
  = EdgeList vs' es'
  where
    vs' = vsS0 -- S.findMin &&& S.findMax $ vsS0
    es' = filter p es
    p e@Nullary{} = to e `S.member` vsS0
    p e@Unary{} = to e `S.member` vsS0 && from1 e `S.member` vsS0
    p e@Binary{} = to e `S.member` vsS0 && from1 e `S.member` vsS0 && from2 e `S.member` vsS0
    p e@Hyperedge{} = to e `S.member` vsS0 && V.foldr (\x y -> x `S.member` vsS0 && y) True (_from e)
    {-
    p e@Nullary{ to = toe }
      = toe `S.member` vsS0
    p e@Unary{ to = toe, from1 = x }
      = toe `S.member` vsS0 && x `S.member` vsS0
    p e@Binary{ to = toe, from1 = x1, from2 = x2 }
      = toe `S.member` vsS0 && x1 `S.member` vsS0 && x2 `S.member` vsS0
    p e@Hyperedge{ to = toe, _from = xs }
      = toe `S.member` vsS0
        && V.foldr (\x y -> x `S.member` vsS0 && y) True xs
    -}
    -- S.fromList (from e) `S.isSubsetOf` vsS0
    {- theMap
      = MS.fromListWith (flip S.union)
      $ [ (x, stoe)
        | e <- es
        , case e of
            Nullary{} -> False
            _ -> True
        , let stoe = S.singleton (to e)
        , x <- from e
        , stoe `seq` True
        ]-}
    theMap
      = foldl' (\m (v, v') -> MS.alter (f v') v m) M.empty
      $ [ (x, to e)
        | e <- es
        , case e of
            Nullary{} -> False
            _ -> True
        , x <- from e
        -- , let toe = to e
        -- , toe `seq` True
        ]
    f v' Nothing = Just (S.singleton v')
    f v' (Just s) = Just (S.insert v' s)
    {- theMap
      = foldl' (\m l -> foldl' (\m (v, v') -> MS.alter (f v') v m) m l) MS.empty
      $ [ case e of
            Unary{ to = toe, from1 = x } -> [ (x, toe) ]
            Binary{ to = toe, from1 = x1, from2 = x2 } -> [ (x1, toe), (x2, toe) ]
            Hyperedge{ to = toe, _from = frome } -> [ (x, toe) | x <- V.toList frome ]
            _ -> undefined
        | e <- es
        , case e of
            Nullary{} -> False
            _ -> True
        ]
    f v' Nothing = Just (S.singleton v')
    f v' (Just s) = Just (S.insert v' s)-}
    theMap0
      = S.fromList
      $ [ to e
        | e <- es
        , case e of
            Nullary{} -> True
            _ -> False
        ]
    -- set of producing nodes
    vsS0 :: S.Set v
    vsS0 = closeProducing
             S.empty
             (Q.fromList (S.toList (theMap0)))
    -- "while loop" for computing the closure
    closeProducing :: S.Set v -> Q.Queue v -> S.Set v
    closeProducing !vsS !vsQ = case Q.deqMaybe vsQ of
      Nothing -> vsS
      Just (v, vsQ') ->
        if S.member v vsS
        then closeProducing vsS vsQ'
        else closeProducing
               (S.insert v vsS)
               (Q.enqList (S.toList (theMap MS.! v)) vsQ')


-- | Drops nonproducing nodes and corresponding edges.
dropNonproducing'
  :: forall v l i. (Ord v, NFData v)
  => EdgeList v l i
  -> EdgeList v l i
dropNonproducing' (EdgeList _ es)
  = EdgeList vs' es'
  where
    vs' = vsS0 -- S.findMin &&& S.findMax $ vsS0
    es' = filter p es
    p e@Nullary{} = to e `S.member` vsS0
    p e@Unary{} = to e `S.member` vsS0 && from1 e `S.member` vsS0
    p e@Binary{} = to e `S.member` vsS0 && from1 e `S.member` vsS0 && from2 e `S.member` vsS0
    p e@Hyperedge{} = to e `S.member` vsS0 && V.foldr (\x y -> x `S.member` vsS0 && y) True (_from e)
    -- S.fromList (from e) `S.isSubsetOf` vsS0
    theMap
      = MS.fromListWith (flip S.union)
      $ [ (x, stoe)
        | e <- es
        , case e of
            Nullary{} -> False
            _ -> True
        , let stoe = S.singleton (to e)
        , stoe `seq` True
        , x <- from e
        ]
    -- set of producing nodes
    vsS0 :: S.Set v
    vsS0 = closeProducing
             S.empty
             (Q.fromList [ toe | Nullary{ to = toe } <- es ])
    -- "while loop" for computing the closure
    closeProducing :: S.Set v -> Q.Queue v -> S.Set v
    closeProducing !vsS !vsQ = case Q.deqMaybe vsQ of
      Nothing -> vsS
      Just (v, vsQ') ->
        if S.member v vsS
        then closeProducing vsS vsQ'
        else closeProducing
               (S.insert v vsS)
               (Q.enqList (S.toList (theMap MS.! v)) vsQ')


dropNonproducing
  :: forall v l i. (NFData v, NFData l, NFData i, Ord i, Ord v, Show l, Show v, Show i)
  => EdgeList v l i
  -> EdgeList v l i
dropNonproducing (EdgeList _ es)
  = EdgeList vsS0 es'
  where
    es' = filter p es
    p e@Nullary{} = to e `S.member` vsS0
    p e@Unary{} = to e `S.member` vsS0 && from1 e `S.member` vsS0
    p e@Binary{} = to e `S.member` vsS0 && from1 e `S.member` vsS0 && from2 e `S.member` vsS0
    p e@Hyperedge{} = to e `S.member` vsS0 && V.foldr (\x y -> x `S.member` vsS0 && y) True (_from e)
    vsS0 = dropLoop (Q.fromList [ e | e@Nullary{} <- es ]) S.empty M.empty
    -- -- --
    forwA :: M.Map v [Hyperedge v l i] -- A.Array v [Hyperedge v l i] -- ^ forward star w/edge ids
    forwA
      = -- M.fromListWith (++) -- A.accumArray (flip (:)) [] vs
        foldl' (\m (v, e) -> M.alter (prep e) v m) M.empty
        [ (v, e)
        | e <- es
        , case e of
            Nullary{} -> False
            _ -> True
        , v <- case e of
                 Binary _ f1 f2 _ _
                   | f1 == f2 -> [f1]
                   | otherwise -> [f1, f2]
                 Unary _ f1 _ _ -> [f1]
                 Hyperedge _ f _ _ -> S.toList (S.fromList (V.toList f))
                 Nullary{} -> undefined -- can not happen
        ]
    prep e Nothing = Just [e]
    prep e (Just es) = Just (e : es)
    magic e
      = case e of
          Binary _ f1 f2 _ _
            | f1 == f2 -> 1
            | otherwise -> 2
          Unary{} -> 1
          Hyperedge _ f _ _ -> S.size (S.fromList (V.toList f))
          Nullary{} -> undefined
    -- -- --
    dropLoop
      :: Q.Queue (Hyperedge v l i)
      -> S.Set v
      -> M.Map i Int
      -> S.Set v
    dropLoop !q !s !adjIM = case Q.deqMaybe q of
      Nothing -> s
      Just (e, q')
        | v `S.member` s -> dropLoop q' s adjIM
        | True -> case loopoMax q' adjIM $ M.findWithDefault [] v forwA of
                    (q'', adjIM') -> dropLoop q'' s' adjIM'
        where
          v = to e
          s' = S.insert v s
          -- es' :: [Hyperedge v l i]
          -- adjChange :: [(i, Maybe Int)] -- < changes to adjacency map
          -- (es', adjChange)
          --   = (catMaybes *** id) . unzip . map work $ M.findWithDefault [] v forwA
          {-adjIM' = (foldl'
                      (\m (k, v) -> maybe (M.delete k) (M.insert k) v m)
                      adjIM
                      adjChange)
          q'' = Q.enqList es' q' -}
          loopoMax !q !adjIM [] = (q, adjIM)
          loopoMax !q !adjIM (e1 : es1) = loopoMax q1 adjIM1 es1
            where
              k = ident e1
              unvis' = M.findWithDefault (magic e1) k adjIM - 1
              (q1, adjIM1) = if (==0) unvis'
                          then (Q.enq e1 q, M.delete k adjIM)
                          else (q, M.insert k unvis' adjIM)
          -- compute change for a given edge information
          work
            :: Hyperedge v l i -> (Maybe (Hyperedge v l i), (i, Maybe Int))
          work e1
            = let k = ident e1
                  -- unvis' = (adjIM M.! k) - 1
                  unvis' = M.findWithDefault (magic e1) k adjIM - 1
              in -- (cand, (k, unvis'))
                if (==0) unvis'
                then (Just e1, (k, Nothing))
                else (Nothing, (k, Just unvis'))



-- | A phantom type to specify our kind of heap.
data MPolicy = MPolicy

-- | Heap type used to efficiently flatten the merge data structure.
type CandidateHeap v l i x = H.Heap MPolicy (Candidate v l i x)

-- | We order candidates by their weights, and instances of 'M' by their
-- head candidate.
instance H.HeapItem MPolicy (Candidate v l i x) where
  newtype Prio MPolicy (Candidate v l i x)
    = FMP Double deriving Eq
  type    Val  MPolicy (Candidate v l i x) = Candidate v l i x

  split c@(Candidate w _ _) = (FMP w, c)
  merge = snd -- (FMP _, c) = c

instance Ord (Prio MPolicy (Candidate v l i x)) where
  compare (FMP x) (FMP y) = compare y x

knuth
  :: forall v l i x. (NFData v, NFData l, NFData i, NFData x, Ord i, Ord v, Show l, Show v, Show i)
  => EdgeList v l i
  -> Feature l i x
  -> V.Vector Double
  -> BestArray v l i x
knuth (EdgeList _ es) feat wV
  = knuthLoop
      iniCandH
      iniBestA
      M.empty
      -- iniAdjIM
  where
    (iniCandH, iniBestA)
      = updateLoop
          H.empty
          M.empty
          -- ^^ (M.fromList [ (v, []) | v <- S.toList vs ])
          -- ^^ (A.array vs [ (v, []) | v <- Ix.range vs ])
          [ topCC feat wV e [] | e@Nullary{} <- es ]
    -- -- --
    forwA :: M.Map v [Hyperedge v l i] -- A.Array v [Hyperedge v l i] -- ^ forward star w/edge ids
    forwA
      = -- M.fromListWith (++) -- A.accumArray (flip (:)) [] vs
        foldl' (\m (v, e) -> M.alter (prep e) v m) M.empty
        [ (v, e)
        | e <- es
        , case e of
            Nullary{} -> False
            _ -> True
        , v <- case e of
                 Binary _ f1 f2 _ _
                   | f1 == f2 -> [f1]
                   | otherwise -> [f1, f2]
                 Unary _ f1 _ _ -> [f1]
                 Hyperedge _ f _ _ -> S.toList (S.fromList (V.toList f))
                 Nullary{} -> undefined -- can not happen
        ]
    prep e Nothing = Just [e]
    prep e (Just es') = Just (e : es')
    {--
    forwA :: M.Map v [Hyperedge v l i] -- A.Array v [Hyperedge v l i] -- ^ forward star w/edge ids
    forwA
      = M.union
        (M.fromListWith (++) -- A.accumArray (flip (:)) [] vs
        [ (v, [e])
        | e <- es
        , case e of
            Nullary{} -> False
            _ -> True
        , v <- case e of
                 Binary _ f1 f2 _ _
                   | f1 == f2 -> [f1]
                   | otherwise -> [f1, f2]
                 Unary _ f1 _ _ -> [f1]
                 Hyperedge _ f _ _ -> S.toList (S.fromList (V.toList f))
                 Nullary{} -> undefined -- can not happen
        ])
        (M.fromList $ zip (S.toList vs) $ repeat []) --}
    -- -- --
    {-- iniAdjIM :: M.Map i Int -- ^ # ingoing adjacencies by edge id
    iniAdjIM
      = M.fromList
        [ case e of
            Binary _ f1 f2 _ _
              | f1 == f2 -> (ie, 1)
              | otherwise -> (ie, 2)
            Unary{} -> (ie, 1)
            Hyperedge _ f _ _ -> (ie, S.size (S.fromList (V.toList f)))
            Nullary{} -> undefined -- can not happen
        | e <- es
        , case e of
            Nullary{} -> False
            _ -> True
        , let ie = ident e
        ] --}
    magic e
      = case e of
          Binary _ f1 f2 _ _
            | f1 == f2 -> 1
            | otherwise -> 2
          Unary{} -> 1
          Hyperedge _ f _ _ -> S.size (S.fromList (V.toList f))
          Nullary{} -> undefined
    -- -- --
    updateLoop
      :: CandidateHeap v l i x
      -> BestArray v l i x
      -> [Candidate v l i x]
      -> (CandidateHeap v l i x, BestArray v l i x)
    updateLoop !candH !bestA [] = (candH, bestA)
    updateLoop !candH !bestA (c@(Candidate w (T.Node e _) _):cs) =
      let v = to e in
      case M.lookup v bestA of
        Nothing -> updateLoop ({-# SCC h1 #-} H.insert c candH) ({-# SCC a1 #-} M.insert v [c] bestA) cs
        Just (Candidate w' _ _ : _)
          | w > w' -> updateLoop ({-# SCC h2 #-} H.insert c candH) ({-# SCC a2 #-} M.insert v [c] bestA) cs
          | otherwise -> updateLoop candH bestA cs
    -- -- --
    knuthLoop
      :: CandidateHeap v l i x
      -> BestArray v l i x
      -> M.Map i Int
      -> BestArray v l i x
    knuthLoop !candH !bestA !adjIM = case H.view candH of
      Nothing -> bestA -- < no candidates, so we are done
      Just (Candidate _ (T.Node e _) _, candH') ->
        case bestA M.! v of
          Candidate _ (T.Node e' _) _ : _
              -- candidate for an as yet unvisited node
            | e == e' -> knuthLoop
                          candH''
                          bestA'
                          -- (M.fromList adjChange `M.union` adjIM)
                          (foldr (\(k, v) m -> maybe (M.delete k) (M.insert k) v m) adjIM adjChange)
                  -- union: left argument preferred
              -- candidate for a visited node, just throw it away
            | otherwise -> knuthLoop candH' bestA adjIM
          _ -> undefined -- can not happen
        where
          v = to e
          (candH'', bestA') = updateLoop candH' bestA newCand
          newCand :: [Candidate v l i x] -- < new candidates from v
          adjChange :: [(i, Maybe Int)] -- < changes to adjacency map
          (newCand, adjChange)
            = (catMaybes *** id) . unzip . map work $ M.findWithDefault [] v forwA
          -- compute change for a given edge information
          work
            :: Hyperedge v l i -> (Maybe (Candidate v l i x), (i, Maybe Int))
          work e1
            = let k = ident e1
                  -- unvis' = (adjIM M.! k) - 1
                  unvis' = M.findWithDefault (magic e1) k adjIM - 1
                  cand = Just $ topCC feat wV e1 $ map (head . (bestA M.!))
                         $ from e1
              in -- (cand, (k, unvis'))
                if (==0) unvis'
                then (cand, (k, Nothing))
                else (Nothing, (k, Just unvis'))


