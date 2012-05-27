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
  , knuth
  ) where

import Prelude hiding ( lookup )

import Control.Arrow ( (***) )
import Control.DeepSeq ( NFData (..) )
import qualified Data.Array as A
import Data.Heap ( Prio, Val )
import qualified Data.Heap as H hiding ( Prio, Val )
import qualified Data.IntMap as IM
import qualified Data.Ix as Ix
import qualified Data.Map as M
import Data.Maybe
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
knuth (EdgeList vs es) feat wV
  = knuthLoop
      iniCandH
      iniBestA
      iniAdjIM
  where
    (iniCandH, iniBestA)
      = updateLoop
          H.empty
          (M.fromList [ (v, []) | v <- S.toList vs ])
          -- (A.array vs [ (v, []) | v <- Ix.range vs ])
          [ topCC feat wV e [] | e@Nullary{} <- es ]
    -- -- --
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
        (M.fromList $ zip (S.toList vs) $ repeat [])
    -- -- --
    iniAdjIM :: M.Map i Int -- ^ # ingoing adjacencies by edge id
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
        ]
    -- -- --
    updateLoop
      :: CandidateHeap v l i x
      -> BestArray v l i x
      -> [Candidate v l i x]
      -> (CandidateHeap v l i x, BestArray v l i x)
    updateLoop !candH !bestA [] = (candH, bestA)
    updateLoop !candH !bestA (c@(Candidate w (T.Node e _) _):cs) =
      let v = to e in
      case bestA M.! v of
        [] -> updateLoop (H.insert c candH) (M.insert v [c] bestA) cs
        Candidate w' _ _ : _
          | w > w' -> updateLoop (H.insert c candH) (M.insert v [c] bestA) cs
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
                          (M.fromList adjChange `M.union` adjIM)
                  -- union: left argument preferred
              -- candidate for a visited node, just throw it away
            | otherwise -> knuthLoop candH' bestA adjIM
          _ -> undefined -- can not happen
        where
          v = to e
          (candH'', bestA') = updateLoop candH' bestA newCand
          newCand :: [Candidate v l i x] -- < new candidates from v
          adjChange :: [(i, Int)] -- < changes to adjacency map
          (newCand, adjChange)
            = (catMaybes *** id) . unzip . map work . (forwA M.!) $ v
          -- compute change for a given edge information
          work :: Hyperedge v l i -> (Maybe (Candidate v l i x), (i, Int))
          work e1
            = let k = ident e1
                  unvis' = (adjIM M.! k) - 1
                  cand =
                    if (==0) unvis'
                    then Just $ topCC feat wV e1 $ map (head . (bestA M.!))
                         $ from e1
                    else Nothing
              in (cand, (k, unvis'))

