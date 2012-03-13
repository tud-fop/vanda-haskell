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

{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Maintainer  :  Matthias Buechse
-- Stability   :  unknown
-- Portability :  portable
--
-- This module makes 'BackwardStar' a 'Hypergraph' instance, and it exports
-- some algorithm implementations that are specific to 'BackwardStar'.
module Vanda.Hypergraph.BackwardStar
  ( module Vanda.Hypergraph.Basic
  , bests
  , edgeCount
  , filterEdges
  , mapNodes
  , mapLabels
  , memoize
  , toEdgeList
  , toSimulation
  , dropUnreachables
  , product
  , product'
  ) where

import Prelude hiding ( product )

import Control.Arrow ( (***), (&&&) )
import qualified Data.Array as A
import qualified Data.Heap as H
import qualified Data.Ix as Ix
import qualified Data.Map as M
import qualified Data.Queue as Q
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Data.Vector as V

import Vanda.Features
  ( Feature
  , Candidate (..)
  , BestArray
  , topCC )
import Vanda.Hypergraph.Basic
  ( Hyperedge(..)
  , from
  , arity
  , mapHE
  , interlace
  , BackwardStar(..)
  , EdgeList(..)
  , Simulation(..) )

edgeCount (BackwardStar vs b _) = sum $ map (length . b) (Ix.range vs)
filterEdges p (BackwardStar vs b _) = BackwardStar vs (filter p . b) False
mapNodes f (BackwardStar vs b _)
  = BackwardStar vs' (a A.!) True
  where
    vs' = (f *** f) vs
    a = A.array vs' [ (f v, map (mapHE f) (b v)) | v <- Ix.range vs ]
mapLabels f (BackwardStar vs b _) = BackwardStar vs (map f . b) False
memoize bs@(BackwardStar vs b mem)
  | mem = bs -- idempotent
  | otherwise = BackwardStar vs (a A.!) True
  where
    a = A.array vs [ (v, b v) | v <- Ix.range vs ]
toEdgeList (BackwardStar sts b _)
  = EdgeList sts $ concatMap b (Ix.range sts)
toSimulation (BackwardStar sts b _) = Simulation sts lookup
  where
    lookup v l n
      = M.findWithDefault [] (l, n)
      $ a A.! v
    makeM es = M.fromListWith (++)
      [ ((l, n), [e])
      | e <- es
      , let l = label e
      , let n = arity e
      ]
    a = A.array sts [ (v, makeM $ b v) | v <- Ix.range sts ]

-- | Drops unreachable nodes and corresponding edges.
dropUnreachables
  :: forall v l i. Ix.Ix v
  => v
  -> BackwardStar v l i
  -> BackwardStar v l i
dropUnreachables v0 (BackwardStar vs b mem) = BackwardStar vs' b' mem
  where
    vs' = (S.findMin vsS, S.findMax vsS)
    b' v = if S.member v vsS then b v else []
    -- set of reachable nodes
    vsS :: S.Set v
    vsS = closeReachables (S.empty, Q.singleton v0)
    -- "while loop" for computing the closure
    closeReachables :: (S.Set v, Q.Queue v) -> S.Set v
    closeReachables (vsS, vsQ) = case Q.deqMaybe vsQ of
      Nothing -> vsS
      Just (v, vsQ') ->
        if S.member v vsS
        then closeReachables (vsS, vsQ')
        else closeReachables (vsS', Q.enqList (newNodes (b v)) vsQ')
          where
            vsS' = S.insert v vsS
            newNodes bv = [ v'
                          | e <- bv
                          , v' <- from e
                          -- , S.notMember v' vsS' -- don't do it twice?
                          ]

-- | ``Merge'' data structure that is used to represent the alternating of
-- (a) top concatenation and (b) merging of derivation tree lists.
-- We will use a heap in order to flatten this structure, producing an
-- ordered list of candidates.
data M a
  = E         -- ^ empty
  | M a [M a] -- ^ the first argument is the head (best) candidate

-- | A phantom type to specify our kind of heap.
data MPolicy = MPolicy

-- | Heap type used to efficiently flatten the merge data structure.
type Heap v l i x = H.Heap MPolicy (M (Candidate v l i x))

-- | We order candidates by their weights, and instances of 'M' by their
-- head candidate.
instance H.HeapItem MPolicy (M (Candidate v l i x)) where
  newtype H.Prio MPolicy (M (Candidate v l i x))
    = FMP { unFMP :: Maybe Double } deriving (Eq, Ord)
  type    H.Val  MPolicy (M (Candidate v l i x)) = (M (Candidate v l i x))

  split E = (FMP Nothing, E)
  split m@(M (Candidate w _ _) _) = (FMP (Just w), m)
  merge (FMP _, m) = m

-- | Flattens a list of merge data structures, producing a sorted list
-- of candidates.
flatten :: [M (Candidate v l i x)] -> [Candidate v l i x]
flatten = untangle . H.fromList
  where
    untangle :: Heap v l i x -> [Candidate v l i x]
    untangle heap
      = case H.view heap of
          Nothing -> []
          Just (E, heap') -> untangle heap'
          Just (M a l, heap') ->
            a : untangle (heap' `H.union` H.fromList l)

-- | Top concatenation for candidate lists.
topCCL
  :: Feature l i x
  -> V.Vector Double
  -> Hyperedge v l i
  -> [[Candidate v l i x]]
  -> M (Candidate v l i x)
topCCL feat wV e lists
  | any null lists = E
  | otherwise
    = M
      (topCC feat wV e (map head lists))
      (map (topCCL feat wV e) (tail (combine lists)))
  where
    -- | given [l1, ..., ln] compute a partition of the Cartesian product
    -- l1 x ... x ln represented as a list of lists of the form
    -- [l1', ..., ln']
    combine :: [[a]] -> [[[a]]]
    combine [] = [[]]
    combine (x:xs) = map (x:) c ++ [tail x : map (\ x' -> [head x']) xs]
      where c = combine xs

-- | Computes the array of best derivations, given an array of one best
-- derivations (e.g., obtained via Knuth's algorithm).
bests
  :: forall v l i x. (Eq i, Ix.Ix v)
  => BackwardStar v l i
  -> Feature l i x
  -> V.Vector Double
  -> BestArray v l i x
  -> BestArray v l i x
bests (BackwardStar vs b _) feat wV bestA = bestA'
  where
    bestA' :: BestArray v l i x
    bestA' = A.array vs
      [ ( v
        , case bestA A.! v of
            [] -> []
            cand@(Candidate _ (T.Node e _) _) : _ ->
              cand : (flatten . concat)
                [ if e /= e'
                  then [tc]
                  else -- get rid of the head element as this equals cand!
                    case tc of
                      E -> []
                      M _ ts -> ts
                | e' <- b v
                , let tc = topCCL feat wV e
                         $ map (bestA' A.!) $ from e'
                ]
        )
      | v <- Ix.range vs
      ]

product
  :: (Hyperedge Int l i1 -> Hyperedge Int l i2 -> Bool)
  -> BackwardStar Int l i1
  -> BackwardStar Int l i2
  -> BackwardStar Int l (i1,i2)
product comp (BackwardStar (v11,v12) b1 _) (BackwardStar (v21,v22) b2 _)
  = BackwardStar sts (map (uncurry (interlace ix)) . (a A.!)) True
  where
    sts'@(stsl,stsh) = ((v11,v21), (v12,v22)) 
    ix = Ix.index sts'
    sts = (ix stsl, ix stsh)
    a = A.array sts
      [ (ix v, product' (b1 v1) (b2 v2))
      | v@(v1, v2) <- Ix.range sts'
      ]
    product' rs1 rs2 = [ (r,r') | r <- rs1, r' <- rs2, r `comp` r' ]

product'
  :: (Hyperedge Int l i1 -> Hyperedge Int l i2 -> Bool)
  -> BackwardStar Int l i1
  -> BackwardStar Int l i2
  -> BackwardStar Int l (i1,i2)
product' comp (BackwardStar (v11,v12) b1 _) (BackwardStar (v21,v22) b2 _)
  = BackwardStar sts (a A.!) True
  where
    sts'@(stsl,stsh) = ((v11,v21), (v12,v22)) 
    ix = Ix.index sts'
    sts = (ix stsl, ix stsh)
    a = A.array sts
      [ (ix v, product' (b1 v1) (b2 v2))
      | v@(v1, v2) <- Ix.range sts'
      ]
    product' rs1 rs2
      = [ interlace ix r r' | r <- rs1, r' <- rs2, r `comp` r' ]

