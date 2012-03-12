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

type CandidateHeap v l i x = H.MinPrioHeap Double (Derivation v l i, x)
type BestMapping   v l i x = v -> Candidate v l i x

instance NFData l => NFData (T.Tree l) where
  rnf (T.Node l ts) = rnf l `seq` rnf ts

knuth
  :: forall v l i x. (NFData v, NFData l, NFData i, NFData x, Ix.Ix v)
  => EdgeList v l i
  -> Feature l i x
  -> V.Vector Double
  -> BestArray v l i x
knuth (EdgeList vs es) feat wV
  = knuthLoop
      (H.fromList $!! map (flip (topCC feat wV) []) $ nullE)
      (A.array vs [ (v, []) | v <- Ix.range vs ])
      adjIM
  where
    -- auxiliary data structures:
    nullE :: [Hyperedge v l i] -- ^ nullary edges
    forwA :: A.Array v [(Int, Hyperedge v l i)] -- ^ forward star w/edge ids
    adjIM :: IM.IntMap Int -- ^ # ingoing adjacencies by edge id
    (nullE, (forwA, adjIM))
      = lefts
        &&& ( (A.accumArray (flip (:)) [] vs *** IM.fromList)
            . (concat *** concat)
            . unzip . rights
            )
      $ [ if null frome
          then Left e
          else Right
            $!! (,)
              [ (v, it) | v <- S.toList ingoing ]
              [ (k, S.size ingoing) ]
        | it@(k, e) <- zip [0..] es
        , let frome = from e
        , let ingoing = S.fromList $ frome
        ]
    -- 
    knuthLoop
      :: CandidateHeap v l i x
      -> BestArray v l i x
      -> IM.IntMap Int
      -> BestArray v l i x
    knuthLoop candH bestA adjIM = case H.view candH of
      Nothing -> bestA -- < no candidates, so we are done
      Just (it@(w, (d@(T.Node e ds), x)), candH') ->
        case bestA A.! v of
          -- candidate for an as yet unvisited node
          [] -> knuthLoop
            (H.union candH' $ H.fromList newCand)
            bestA'
            (IM.fromList adjChange `IM.union` adjIM)
              -- union: left argument preferred
          -- candidate for a visited node, just throw it away
          _ -> knuthLoop candH' bestA adjIM
        where
          bestA' = bestA A.// [(v, [it])]
          v = to e
          newCand :: [Candidate v l i x] -- < new candidates from v
          adjChange :: [(Int, Int)] -- < changes to adjacency map
          (newCand, adjChange)
            = (catMaybes *** id) . unzip . map work . (forwA A.!) $ v
          -- compute change for a given edge information
          work
            :: (Int, Hyperedge v l i)
            -> (Maybe (Candidate v l i x), (Int, Int))
          work (k, e)
            = let unvis' = (adjIM IM.! k) - 1
                  cand =
                    if (==0) unvis'
                    then Just $ topCC feat wV e $ map (head . (bestA' A.!))
                         $ from e
                    else Nothing
              in (cand, (k, unvis'))

