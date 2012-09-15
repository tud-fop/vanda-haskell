{-# LANGUAGE EmptyDataDecls #-}
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

module Vanda.Hypergraph.IntHypergraph
  ( Hyperedge (..)
  , foldpv
  , from
  , deref
  , arity
  , mapHE
  , mapHEi
  , Derivation
  , mkHyperedge
  , Hypergraph (..)
  , nodesLL
  , nodesL
  , mkHypergraph
  , filterEdges
  , mapNodes
  , mapLabels
  , dropNonproducing
  , dropNonproducing'
  , Feature
  , Candidate (..)
  , BestArray
  , knuth
  ) where

import Prelude hiding ( lookup )

import Control.Monad ( when, unless, forM_ )
import Control.Monad.ST
import Data.Heap ( Prio, Val )
import qualified Data.Array as A
import qualified Data.Array.Base as AB
import qualified Data.Array.MArray as MA
import qualified Data.Array.ST as STA
import qualified Data.Heap as H hiding ( Prio, Val )
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.List as L ( foldl' )
import qualified Data.Map.Strict as MS
import qualified Data.Queue as Q
import Data.STRef
import qualified Data.Tree as T
import qualified Data.Vector.Unboxed as VU

-- import Debug.Trace

import Vanda.Util

-- | A Hyperedge, consisting of head node, tail nodes, label, and identifier.
-- The identifier can be used for interfacing with feature functions.
-- Hyperedge is made an instance of Eq and Ord solely via the identifier.
data Hyperedge l i
  = Nullary
    { to :: !Int
    , label :: !l
    , ident :: !i
    }
  | Unary
    { to :: !Int
    , from1 :: !Int
    , label :: !l
    , ident :: !i
    }
  | Binary
    { to :: !Int
    , from1 :: !Int
    , from2 :: !Int
    , label :: !l
    , ident :: !i
    }
  | Hyperedge
    { to :: !Int
    , _from :: !(VU.Vector Int)
    , label :: !l
    , ident :: !i
    }

    
instance (Show l, Show i) => Show (Hyperedge l i) where
  show e
    = show (to e)
      ++ " -> "
      ++ show (label e)
      ++ " "
      ++ unwords (map show $ from e)
      ++ " # "
      ++ show (ident e)

foldpv :: (Int -> Bool) -> Hyperedge l i -> Bool
foldpv p (Nullary t _ _) = p t
foldpv p (Unary t f1 _ _) = p t && p f1
foldpv p (Binary t f1 f2 _ _) = p t && p f1 && p f2
foldpv p (Hyperedge t f _ _) = VU.foldr (\x y -> p x && y) (p t) f

from :: Hyperedge l i -> [Int]
from Nullary{} = []
from (Unary _ f1 _ _) = [f1]
from (Binary _ f1 f2 _ _) = [f1, f2]
from (Hyperedge _ f _ _) = VU.toList f

deref :: Hyperedge l i -> Int -> Int
deref (Unary _ f1 _ _) 0 = f1
deref (Binary _ f1 _ _ _) 0 = f1
deref (Binary _ _ f2 _ _) 1 = f2
deref (Hyperedge _ f _ _) i = f VU.! i
-- deref e i = error (show (ident e) ++ show i) 

arity :: Hyperedge l i -> Int
arity Nullary{} = 0
arity Unary{} = 1
arity Binary{} = 2
arity (Hyperedge _ f _ _) = VU.length f

mapHE :: (Int -> Int) -> Hyperedge l i -> Hyperedge l i
mapHE g (Nullary t l i) = Nullary (g t) l i
mapHE g (Unary t f1 l i) = Unary (g t) (g f1) l i
mapHE g (Binary t f1 f2 l i) = Binary (g t) (g f1) (g f2) l i
mapHE g (Hyperedge t f l i) = Hyperedge (g t) (VU.map g f) l i

mapHEi :: (i -> i') -> Hyperedge l i -> Hyperedge l i'
mapHEi g (Nullary t l i) = Nullary t l (g i)
mapHEi g (Unary t f1 l i) = Unary t f1 l (g i)
mapHEi g (Binary t f1 f2 l i) = Binary t f1 f2 l (g i)
mapHEi g (Hyperedge t f l i) = Hyperedge t f l (g i)

instance (Eq l) => Eq (Hyperedge l i) where
  -- instance Eq i => ...
  -- e1 == e2 = ident e1 == ident e2
  Nullary t1 l1 _ == Nullary t2 l2 _
    = t1 == t2 && l1 == l2
  Unary t1 f1 l1 _ == Unary t2 f2 l2 _
    = t1 == t2 && l1 == l2 && f1 == f2
  Binary t1 f11 f12 l1 _ == Binary t2 f21 f22 l2 _
    = t1 == t2 && l1 == l2 && f11 == f21 && f12 == f22
  Hyperedge t1 f1 l1 _ == Hyperedge t2 f2 l2 _
    = t1 == t2 && l1 == l2 && and (zipWith (==) (VU.toList f1) (VU.toList f2))
  _ == _ = False

-- | A derivation (tree), i.e., a tree over hyperedges.
type Derivation l i = T.Tree (Hyperedge l i)

-- | Creates a 'Hyperedge'.
mkHyperedge
  :: Int   -- ^ Head node
  -> [Int] -- ^ Tail nodes
  -> l   -- ^ Label
  -> i   -- ^ Identifier
  -> Hyperedge l i
mkHyperedge !t !f !l !i
  = case f of
      [] -> Nullary t l i
      [f1] -> Unary t f1 l i 
      [f1, f2] -> Binary t f1 f2 l i
      _ -> Hyperedge t (VU.fromList f) l i

-- * Hypergraph representations

-- | Edge list representation of a Hypergraph. It consists of the interval
-- of nodes present in the hypergraph and a list of its hyperedges.
data Hypergraph l i
  = Hypergraph
    { nodes :: Int -- ^ Number of nodes (interval 0..nodes-1)
    , edges :: [Hyperedge l i] -- ^ List of 'Hyperedge's
    }

-- | Extracts the nodes occurring in a list of edges. Does /not/ remove
-- duplicates.
nodesLL :: [Hyperedge l i] -> [Int]
nodesLL es = [ v | e <- es, v <- to e : from e ]

-- | Obtains the interval of nodes occurring in a list of edges.
nodesL :: [Int] -> (Int, Int)
nodesL (x : xs) = L.foldl' minimax (x, x) xs
  where
    minimax (min0, max0) a
      = let min1 = min min0 a
            max1 = max max0 a
        in min1 `seq` max1 `seq` (min1, max1)

mkHypergraph :: [Hyperedge l i] -> Hypergraph l i
mkHypergraph es
  = Hypergraph{ nodes = (+ 1) . snd . nodesL . nodesLL $ es
              , edges = es
              }

filterEdges :: (Hyperedge l i -> Bool) -> Hypergraph l i -> Hypergraph l i
filterEdges p = mkHypergraph . filter p . edges

mapLabels
  :: (Hyperedge l i -> Hyperedge l' i') -> Hypergraph l i -> Hypergraph l' i'
mapLabels f h = h{ edges = map f (edges h) }

mapNodes :: (Int -> Int) -> Hypergraph l i -> Hypergraph l i
mapNodes f (Hypergraph vs es) 
  = Hypergraph{ nodes = (+ 1) . snd . nodesL $ map f $ enumFromTo 0 $ vs - 1
              , edges = map (mapHE f) es
              }

-- | Drops nonproducing nodes and corresponding edges.
dropNonproducing' :: Hypergraph l i -> Hypergraph l i
dropNonproducing' (Hypergraph vs es) = Hypergraph vs es'
  where
    -- vs' = vsS0 -- S.findMin &&& S.findMax $ vsS0
    es' = filter (foldpv (`IS.member` vsS0)) es
    theMap
      = MS.fromListWith (flip IS.union)
      $ [ (x, stoe)
        | e <- es
        , case e of
            Nullary{} -> False
            _ -> True
        , let stoe = IS.singleton (to e)
        , stoe `seq` True
        , x <- from e
        ]
    -- set of producing nodes
    vsS0 :: IS.IntSet
    vsS0 = closeProducing
             IS.empty
             (Q.fromList [ toe | Nullary{ to = toe } <- es ])
    -- "while loop" for computing the closure
    closeProducing :: IS.IntSet -> Q.Queue Int -> IS.IntSet
    closeProducing !vsS !vsQ = case Q.deqMaybe vsQ of
      Nothing -> vsS
      Just (v, vsQ') ->
        if v `IS.member` vsS
        then closeProducing vsS vsQ'
        else closeProducing
               (IS.insert v vsS)
               (Q.enqList (IS.toList (theMap MS.! v)) vsQ')


data He l i = He !Int !(Hyperedge l i)


computeForwardA
  :: Hypergraph l i -> ST s (STA.STArray s Int [STRef s (He l i)]) 
computeForwardA (Hypergraph vs es) = do
  forwA <- MA.newArray (0, vs + 1) []
  sequence_
    [ do
        he <- newSTRef $! He (arity e) e
        forM_ (from e) $
          \ v -> do
            hes <- AB.unsafeRead forwA v
            AB.unsafeWrite forwA v $! he : hes
    | e <- es
    , case e of { Nullary{} -> False ; _ -> True }
    ]
  return forwA


updateHe :: (Hyperedge l i -> ST s ()) -> STRef s (He l i) -> ST s ()
updateHe f he = do
  He i e <- readSTRef he
  if i == 1 then f e else writeSTRef he $! He (i - 1) e


dropNonproducing :: Hypergraph l i -> Hypergraph l i
dropNonproducing hg@(Hypergraph vs es) = Hypergraph vs es'
  where
    es' = filter (foldpv (`IS.member` vsS0)) es
    vsS0 = runST $ do
      forwA <- computeForwardA hg
      q <- newSTRef $ [ e | e@Nullary{} <- es ]
      s <- newSTRef IS.empty
      let go' = do
            lviewSTRef' q (readSTRef s) $ \ e -> let v = to e in do
              b <- readSTRefWith (v `IS.member`) s
              unless b $ do
                modifySTRef' s $ IS.insert v
                hes <- AB.unsafeRead forwA v
                forM_ hes $ updateHe (\ e1 -> modifySTRef' q (e1 :))
                AB.unsafeWrite forwA v []
              go'
      go'


-- | A phantom type to specify our kind of heap.
data MPolicy -- = MPolicy

type Feature l i = l -> i -> [Double] -> Double

data Candidate l i
  = Candidate
    { weight :: !Double
    , deriv :: (Derivation l i)
    }

-- | Top concatenation for derivation candidates.
topCC
  :: Feature l i          -- ^ 'Feature' mapping
  -> Hyperedge l i        -- ^ 'Hyperedge' for top concatenation
  -> [Candidate l i]      -- ^ successor candidates
  -> Candidate l i        -- ^ resulting candidate
topCC feat e cs
  = Candidate
      (feat (label e) (ident e) $ map weight cs)
      (T.Node e $ map deriv cs)


-- | Heap type used to efficiently flatten the merge data structure.
type CandidateHeap l i = H.Heap MPolicy (Candidate l i)

type BestArray l i = A.Array Int [Candidate l i]

-- | We order candidates by their weights, and instances of 'M' by their
-- head candidate.
instance H.HeapItem MPolicy (Candidate l i) where
  newtype Prio MPolicy (Candidate l i)
    = FMP Double deriving Eq
  type    Val  MPolicy (Candidate l i) = Candidate l i

  split c@(Candidate w _) = (FMP w, c)
  merge = snd -- (FMP _, c) = c

instance Ord (Prio MPolicy (Candidate l i)) where
  compare (FMP x) (FMP y) = compare y x

knuth
  :: Hypergraph l i -> Feature l i -> BestArray l i
knuth hg@(Hypergraph vs es) feat = STA.runSTArray $ do
  forwA <- computeForwardA hg
  candH <- newSTRef (H.empty :: CandidateHeap l i)
  bestA <- MA.newArray (0, vs + 1) []
  let upd' v c = do
        modifySTRef' candH $ H.insert c
        AB.unsafeWrite bestA v [c]
      upd c@(Candidate w (T.Node e _)) = let v = to e in do
        cs <- AB.unsafeRead bestA v
        case cs of
          [] -> upd' v c
          Candidate w' _ : _ -> when (w > w') $ upd' v c
      go = do
        viewSTRef' candH H.view (return bestA) $
          \ (Candidate w (T.Node e _)) -> let v = to e in do
            Candidate w' (T.Node e' _) : _ <- AB.unsafeRead bestA v
            when (w == w' && v == to e') $ do
              hes <- AB.unsafeRead forwA v
              forM_ hes $ updateHe $ \ e1 -> do
                scs <- mapM (AB.unsafeRead bestA) $ from e1
                upd $ topCC feat e1 $ map head scs
              AB.unsafeWrite forwA v []
            go
  mapM_ upd [ topCC feat e [] | e@Nullary{} <- es ]
  go

