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
  , fromEdgeList
  , mapNodes
  , mapLabels
  , memoize
  , toEdgeList
  , toSimulation
  , dropUnreachables
--  , product
--  , product'
  ) where

import Prelude hiding ( lookup, product )

import Control.Arrow ( (***), (&&&) )
import qualified Data.Array as A
import qualified Data.Heap as H hiding ( Prio, Val )
import Data.Heap ( Prio, Val )
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

edgeCount :: Ix.Ix v => BackwardStar v l i -> Int
edgeCount (BackwardStar vs b _) = sum $ map (length . b) (S.toList vs)

filterEdges
  :: (Hyperedge v l i -> Bool) -> BackwardStar v l i -> BackwardStar v l i
filterEdges p (BackwardStar vs b _) = BackwardStar vs (filter p . b) False

fromEdgeList :: Ix.Ix v => EdgeList v l i -> BackwardStar v l i
fromEdgeList (EdgeList vs es) = BackwardStar vs (a M.!) True
  where
    lst = [ (v, [e]) | e <- es, let v = to e ]
    a = M.union
        (M.fromListWith (++) lst)
        (M.fromList $ zip (S.toList vs) $ repeat [])
        -- A.accumArray (flip (:)) [] vs lst

mapNodes
  :: (Ix.Ix v, Ix.Ix v')
  => (v -> v') -> BackwardStar v l i -> BackwardStar v' l i
mapNodes f (BackwardStar vs b _)
  = BackwardStar vs' (a M.!) True
  where
    vs' = S.map f vs -- (f *** f) vs
    a = M.fromList [ (f v, map (mapHE f) (b v)) | v <- S.toList vs ]
        -- A.array vs' [ (f v, map (mapHE f) (b v)) | v <- Ix.range vs ]

mapLabels
  :: (Hyperedge v l i -> Hyperedge v l' i')
  -> BackwardStar v l i
  -> BackwardStar v l' i'
mapLabels f (BackwardStar vs b _) = BackwardStar vs (map f . b) False

memoize :: Ix.Ix v => BackwardStar v l i -> BackwardStar v l i
memoize bs@(BackwardStar vs b mem)
  | mem = bs -- idempotent
  | otherwise = BackwardStar vs (a M.!) True
  where
    a = M.fromList [ (v, b v) | v <- S.toList vs ]
        -- A.array vs [ (v, b v) | v <- Ix.range vs ]

toEdgeList :: Ix.Ix v => BackwardStar v l i -> EdgeList v l i
toEdgeList (BackwardStar vs b _)
  = EdgeList vs $ concatMap b (S.toList vs)

toSimulation :: (Ord l, Ix.Ix v) => BackwardStar v l i -> Simulation v l i
toSimulation (BackwardStar vs b _) = Simulation vs lookup
  where
    lookup v l n
      = M.findWithDefault [] (l, n)
      $ a M.! v
    makeM es = M.fromListWith (++)
      [ ((l, n), [e])
      | e <- es
      , let l = label e
      , let n = arity e
      ]
    a = M.fromList [ (v, makeM $ b v) | v <- S.toList vs ]
        -- A.array vs [ (v, makeM $ b v) | v <- Ix.range vs ]

-- | Drops unreachable nodes and corresponding edges.
dropUnreachables
  :: forall v l i. Ix.Ix v
  => v
  -> BackwardStar v l i
  -> BackwardStar v l i
dropUnreachables v0 (BackwardStar _ b mem) = BackwardStar vs' b' mem
  where
    vs' = vsS0 -- S.findMin &&& S.findMax $ vsS0
    b' v = if S.member v vsS0 then b v else []
    -- set of reachable nodes
    vsS0 :: S.Set v
    vsS0 = closeReachables (S.empty, Q.singleton v0)
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
  newtype Prio MPolicy (M (Candidate v l i x))
    = FMP (Maybe Double) deriving Eq
  type    Val  MPolicy (M (Candidate v l i x)) = M (Candidate v l i x)

  split E = (FMP Nothing, E)
  split m@(M (Candidate w _ _) _) = (FMP (Just w), m)
  merge (FMP _, m) = m

instance Ord (Prio MPolicy (M (Candidate v l i x))) where
  compare (FMP x) (FMP y) = compare y x

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
  -> M.Map v [Candidate v l i x] -- BestArray v l i x
  -> M.Map v [Candidate v l i x] -- BestArray v l i x
bests (BackwardStar vs b _) feat wV bestA = bestA'
  where
    bestA' :: M.Map v [Candidate v l i x] -- BestArray v l i x
    bestA' = M.fromList -- A.array vs
      [ ( v
        , case bestA M.! v of
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
                , let tc = topCCL feat wV e'
                         $ map (bestA' M.!) $ from e'
                ]
        )
      | v <- S.toList vs
      ]

{-
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
      [ (ix v, b1 v1 `pr` b2 v2)
      | v@(v1, v2) <- Ix.range sts'
      ]
    pr rs1 rs2 = [ (r,r') | r <- rs1, r' <- rs2, r `comp` r' ]

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
      [ (ix v, b1 v1 `pr` b2 v2)
      | v@(v1, v2) <- Ix.range sts'
      ]
    pr rs1 rs2
      = [ interlace ix r r' | r <- rs1, r' <- rs2, r `comp` r' ]
-}
