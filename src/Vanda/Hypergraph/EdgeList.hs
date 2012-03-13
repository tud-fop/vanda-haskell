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
  , toBackwardStar
  , toForwardStar
  , toSimulation
  , knuth
  ) where

import Control.Arrow ( (***), (&&&) )
import Control.DeepSeq ( NFData (..), ($!!) )
import qualified Data.Array as A
import qualified Data.Heap as H
import Data.Either
import qualified Data.IntMap as IM
import qualified Data.Ix as Ix
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Data.Vector as V

import Vanda.Features
import Vanda.Hypergraph.Basic
import Vanda.Hypergraph.NFData

filterEdges p (EdgeList vs es) = EdgeList (nodesL es) (filter p es)
mapNodes f (EdgeList vs es) = EdgeList ((f *** f) vs) (map (mapHE f) es)
mapLabels f (EdgeList vs es) = EdgeList vs (map f es)
toBackwardStar (EdgeList sts es) = BackwardStar sts (a A.!) True
  where
    lst = [ (v, e) | e <- es, let v = to e ]
    a = A.accumArray (flip (:)) [] sts lst
toForwardStar (EdgeList sts es) = ForwardStar sts lst (a A.!) True
  where
    lst = [ e | e <- es, null (from e) ]
    lst' = [ (v, e)
           | e <- es
           , let from' = from e
           , not . null $ from'
           , v <- S.toList . S.fromList $ from'
           ]
    a = A.accumArray (flip (:)) [] sts lst'
toSimulation (EdgeList sts es) = Simulation sts lookup
  where
    lookup v l n
      = M.findWithDefault [] (l, n)
      $ a A.! v
    lst = [ (v, (l, n, e))
          | e <- es
          , let v = to e
          , let l = label e
          , let n = arity e {- FIXME: length O(n) -}
          ]
    a = A.accumArray
      (\m (l, n, e) -> M.insertWith (++) (l, n) [e] m)
      M.empty
      sts
      lst

-- | A phantom type to specify our kind of heap.
data MPolicy = MPolicy

-- | Heap type used to efficiently flatten the merge data structure.
type CandidateHeap v l i x = H.Heap MPolicy (Candidate v l i x)

-- | We order candidates by their weights, and instances of 'M' by their
-- head candidate.
instance H.HeapItem MPolicy (Candidate v l i x) where
  newtype H.Prio MPolicy (Candidate v l i x)
    = FMP { unFMP :: Double } deriving (Eq, Ord)
  type    H.Val  MPolicy (Candidate v l i x) = (Candidate v l i x)

  split c@(Candidate w d x) = (FMP w, c)
  merge (FMP _, c) = c

{-instance (NFData v, NFData i, NFData l, NFData x)
  => NFData (Candidate v l i x) where
  rnf (Candidate w d x) = rnf w `seq` rnf d `seq` rnf x-}

type BestMapping v l i x = v -> Candidate v l i x

knuth
  :: forall v l i x. (NFData v, NFData l, NFData i, NFData x, Integral i, Ix.Ix v)
  => EdgeList v l i
  -> Feature l i x
  -> V.Vector Double
  -> BestArray v l i x
knuth (EdgeList vs es) feat wV
  = knuthLoop
      iniCandH
      iniBestA
      adjIM
  where
    (iniCandH, iniBestA)
      = updateLoop
          H.empty
          (A.array vs [ (v, []) | v <- Ix.range vs ])
          [ topCC feat wV e [] | e@(Nullary _ _ _) <- es ]
    -- -- --
    forwA :: A.Array v [Hyperedge v l i] -- ^ forward star w/edge ids
    forwA
      = A.accumArray (flip (:)) [] vs
      $ [ (v, e)
        | e <- es
        , let frome = from e
        , not $ null frome
        , let fr = case e of
                      Unary _ f1 _ _ -> [f1]
                      Binary _ f1 f2 _ _
                        | f1 == f2 -> [f1]
                        | otherwise -> [f1, f2]
                      Hyperedge _ f _ _ -> S.toList . S.fromList $ frome
        , v <- fr
        ]
    -- -- --
    adjIM :: IM.IntMap Int -- ^ # ingoing adjacencies by edge id
    adjIM
      = IM.fromList
      $ [ case e of
            Unary _ f1 _ _ -> (ie, 1)
            Binary _ f1 f2 _ _
              | f1 == f2 -> (ie, 1)
              | otherwise -> (ie, 2)
            Hyperedge _ f _ _ -> (ie, S.size ingoing)
              where ingoing = S.fromList frome
        | e <- es
        , let frome = from e
        , not $ null frome
        , let ie = fromIntegral $ i e
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
      case bestA A.! v of
        [] -> updateLoop (H.insert c candH) (bestA A.// [(v, [c])]) cs
        c'@(Candidate w' _ _):_
          | w < w' -> updateLoop (H.insert c candH) (bestA A.// [(v, [c])]) cs
          | otherwise -> updateLoop candH bestA cs
    -- -- --
    knuthLoop
      :: CandidateHeap v l i x
      -> BestArray v l i x
      -> IM.IntMap Int
      -> BestArray v l i x
    knuthLoop !candH !bestA !adjIM = case H.view candH of
      Nothing -> bestA -- < no candidates, so we are done
      Just (c@(Candidate w d@(T.Node e ds) x), candH') ->
        case bestA A.! v of
          c@(Candidate w' (T.Node e' _) _):_
              -- candidate for an as yet unvisited node
            | e == e' -> knuthLoop
                           candH''
                           bestA''
                           (IM.fromList adjChange `IM.union` adjIM)
                  -- union: left argument preferred
              -- candidate for a visited node, just throw it away
            | otherwise -> knuthLoop candH' bestA adjIM
          -- _ -> knuthLoop candH' bestA adjIM
        where
          (candH'', bestA'') = updateLoop candH' bestA' newCand
          bestA' = bestA -- A.// [(v, [c])]
          v = to e
          newCand :: [Candidate v l i x] -- < new candidates from v
          adjChange :: [(Int, Int)] -- < changes to adjacency map
          (newCand, adjChange)
            = (catMaybes *** id) . unzip . map work . (forwA A.!) $ v
          -- compute change for a given edge information
          work :: Hyperedge v l i -> (Maybe (Candidate v l i x), (Int, Int))
          work e
            = let k = fromIntegral $ i e
                  unvis' = (adjIM IM.! k) - 1
                  cand =
                    if (==0) unvis'
                    then Just $ topCC feat wV e $ map (head . (bestA' A.!))
                         $ from e
                    else Nothing
              in (cand, (k, unvis'))

