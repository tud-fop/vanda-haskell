-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Matthias Büchse 2012
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-- Maintainer  :  Matthias Buechse
--
-- Implementation of the 'EdgeList' representation.
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


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

import Control.Monad ( when, unless, forM_ )
import Control.Monad.ST
import Data.Heap ( Prio, Val )
import qualified Data.Heap as H hiding ( Prio, Val )
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import qualified Data.Queue as Q
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Data.Vector as V

-- import Debug.Trace

import Vanda.Features
import Vanda.Hypergraph.Basic
import Vanda.Util

filterEdges
  :: Ord v => (Hyperedge v l i -> Bool) -> EdgeList v l i -> EdgeList v l i
filterEdges p (EdgeList _ es) = EdgeList (nodesL es) (filter p es)

mapLabels
  :: (Hyperedge v l i -> Hyperedge v l' i')
  -> EdgeList v l i
  -> EdgeList v l' i'
mapLabels f (EdgeList vs es) = EdgeList vs (map f es)

mapNodes :: Ord v' => (v -> v') -> EdgeList v l i -> EdgeList v' l i
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
dropNonproducing'
  :: forall v l i. Ord v
  => EdgeList v l i
  -> EdgeList v l i
dropNonproducing' (EdgeList _ es)
  = EdgeList vsS0 es'
  where
    -- vs' = vsS0 -- S.findMin &&& S.findMax $ vsS0
    es' = filter (foldpv (`S.member` vsS0)) es
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


data He v l i = He !Int !(Hyperedge v l i)

-- used to count UNIQUE nodes here, but it's not faster and more code
computeForward
  :: Ord v
  => [Hyperedge v l i]
  -> ST s (STRef s (M.Map v [STRef s (He v l i)])) 
computeForward es
  = let
      prep e x = case x of
        Nothing -> Just [e]
        Just es_ -> Just (e : es_)
    in do
      forwA <- newSTRef (M.empty :: M.Map v [STRef s (He v l i)])
      sequence_
        [ do
            he <- newSTRef $! He (arity e) e
            mapM_ (modifySTRef' forwA . M.alter (prep he)) (from e)
        | e <- es
        , case e of { Nullary{} -> False ; _ -> True }
        ]
      return forwA


updateHe :: (Hyperedge v l i -> ST s ()) -> STRef s (He v l i) -> ST s ()
updateHe f he = do
  He i e <- readSTRef he
  if i == 1 then f e else writeSTRef he $! He (i - 1) e


dropNonproducing
  :: forall v l i. Ord v
  => EdgeList v l i
  -> EdgeList v l i
dropNonproducing (EdgeList _ es)
  = EdgeList vsS0 es'
  where
    es' = filter (foldpv (`S.member` vsS0)) es
    vsS0 = runST $ do
      forwA <- computeForward es
      q <- newSTRef $ [ e | e@Nullary{} <- es ]
      s <- newSTRef S.empty
      let go' = do
            lviewSTRef' q (readSTRef s) $ \ e -> let v = to e in do
              b <- readSTRefWith (v `S.member`) s
              unless b $ do
                modifySTRef' s $ S.insert v
                hes <- fmap (M.findWithDefault [] v) $ readSTRef forwA
                forM_ hes $ updateHe (\ e1 -> modifySTRef' q (e1 :))
                modifySTRef' forwA $ M.delete v
              go'
      go'


-- | A phantom type to specify our kind of heap.
data MPolicy -- = MPolicy

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
  :: Ord v
  => EdgeList v l i
  -> Feature l i x
  -> V.Vector Double
  -> BestArray v l i x
knuth (EdgeList _ es) feat wV
  = runST $ do
    forwA <- computeForward es
    candH <- newSTRef (H.empty :: CandidateHeap v l i x)
    bestA <- newSTRef M.empty
    let upd' v c = do
          modifySTRef' candH $ H.insert c
          modifySTRef' bestA $ M.insert v [c]
        upd c@(Candidate w (T.Node e _) _) = let v = to e in
          lookupSTRef' bestA (M.lookup v) (upd' v c)
            $ \ (Candidate w' _ _ : _) -> when (w > w') $ upd' v c
        go = do
          viewSTRef' candH H.view (readSTRef bestA) $
            \ (Candidate w (T.Node e _) _) -> let v = to e in do
              Candidate w' (T.Node e' _) _ : _ <- readSTRefWith (M.! v) bestA
              when (w == w' && v == to e') $ do
                hes <- readSTRefWith (M.findWithDefault [] v) forwA
                forM_ hes $ updateHe $ \ e1 -> do
                  ba <- readSTRef bestA
                  upd $ topCC feat wV e1 $ map (head . (ba M.!)) $ from e1
                modifySTRef' forwA $ M.delete v
              go
    mapM_ upd [ topCC feat wV e [] | e@Nullary{} <- es ]
    go

