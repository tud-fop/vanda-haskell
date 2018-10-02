-----------------------------------------------------------------------------
-- |
-- Module      :  LimitedQueue
-- Copyright   :  (c) Thomas Ruprecht 2017
-- License     :  BSD-style
--
-- Maintainer  :  thomas.ruprecht@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Contains a wrapper of "Data.Semiring" used for an efficient update of
-- weights and instances for "Data.Semiring" and 'Weight'.
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Data.Weight
  ( -- * weight class
    Weight(..)
    -- * semiring / weight instances
  , Inside(Inside)
  , unpack
  , Probabilistic
  , probabilistic
  , Cost
  , cost
  ) where


import Control.Monad (liftM)
import Data.Semiring
import Data.Converging
import Numeric.Log (Log(Exp), Precise)

import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M


-- | Instances of this class implement an inverse element
-- for semirings considering multiplication to implement a weight
-- update by substitution.
class (Semiring wt) => Weight wt where
  (</>) :: wt -> wt -> wt
  (</>) x y = x <.> inverse y
  inverse :: wt -> wt
  inverse x = one </> x


-- | Multiplicative monoid for probabilistic weights.
newtype Probabilistic a = Probabilistic a deriving (Show, Eq)

-- | Greater probabilistic weights are encouraged.
instance (Ord a) => Ord (Probabilistic a) where
  (Probabilistic x) `compare` (Probabilistic y) = y `compare` x


-- | Wraps constructor to check for correct ranged values, stores value in log domain.
probabilistic :: (Precise a, Ord a) => a -> Probabilistic (Log a)
probabilistic x
  | x > 0 && x <= 1 = Probabilistic $ Exp $ log x
  | otherwise = error "probabilistic value out of range"


-- | Instance of multiplicative semigroup
instance (Num a, Ord a) => Semigroup (Probabilistic a) where
  (Probabilistic x) <> (Probabilistic y) = Probabilistic $ max x y


-- | Instance of multiplicative monoid.
instance (Num a, Ord a) => Monoid (Probabilistic a) where
  mempty = Probabilistic 0
  mappend = (<>)


-- | Probabilistic group
instance (Num a, Ord a) => Semiring (Probabilistic a) where
  one = Probabilistic 1
  (Probabilistic x) <.> (Probabilistic y) = Probabilistic $ x * y


instance (Floating a, Ord a) => Weight (Probabilistic a) where
  (Probabilistic x) </> (Probabilistic y) = Probabilistic $ x / y


-- | Additive monoid for costs.
data Cost a = Cost a | Infinity deriving (Show, Eq)

-- | Implements comparisons with 'Infinity'.
instance (Ord a) => Ord (Cost a) where
  Infinity `compare` Infinity = EQ
  Infinity `compare` (Cost _) = GT
  (Cost _) `compare` Infinity = LT
  (Cost x) `compare` (Cost y) = x `compare` y


-- | Wraps constructor to check for correct ranged values.
cost :: (Num a, Ord a) => a -> Cost a
cost x
  | x >= 0 = Cost x
  | otherwise = error "cost value out of range"


-- | Instance minimum semigroup.
instance (Num a, Ord a) => Semigroup (Cost a) where
  (Cost x) <> (Cost y) = Cost $ min x y
  Infinity <> x = x
  x <> Infinity = x


-- | Instance minimum monoid.
instance (Num a, Ord a) => Monoid (Cost a) where
  mempty = Infinity
  mappend = (<>)


-- | Tropical semiring.
instance (Num a, Ord a) => Semiring (Cost a) where
  one = Cost 0
  (Cost x) <.> (Cost y) = Cost $ x + y
  _ <.> _ = Infinity


instance (Num a, Ord a) => Weight (Cost a) where
  (Cost x) </> (Cost y) = Cost $ x - y
  _ </> _ = Infinity


-- | Inside semiring using addition and multiplicaiton as operations.
newtype Inside a = Inside a deriving (Show, Eq, Ord)

unpack :: Inside a -> a
unpack (Inside a) = a


instance (Num a) => Semigroup (Inside a) where
  (Inside x) <> (Inside y) = Inside $ x + y


instance (Num a) => Monoid (Inside a) where
  mempty = Inside 0
  mappend = (<>)

instance (Num a) => Semiring (Inside a) where
  one = Inside 1
  (Inside x) <.> (Inside y) = Inside $ x * y


instance (Converging a) =>  Converging (Inside a) where
  (Inside x) `converged` (Inside y) = x `converged` y

instance (Converging a) => Converging (Probabilistic a) where
  (Probabilistic x) `converged` (Probabilistic y) = x `converged` y

instance (Converging a) => Converging (Cost a) where
  (Cost x) `converged` (Cost y) = x `converged` y
  Infinity `converged` Infinity = True
  converged _ _ = False


newtype instance U.Vector    (Probabilistic w) = V_Probabilistic  (U.Vector    w)
newtype instance U.MVector s (Probabilistic w) = MV_Probabilistic (U.MVector s w)

instance (U.Unbox a) => U.Unbox (Probabilistic a)

instance (U.Unbox a) => M.MVector U.MVector (Probabilistic a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Probabilistic v) = M.basicLength v
  basicUnsafeSlice i n (MV_Probabilistic v) = MV_Probabilistic $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Probabilistic v1) (MV_Probabilistic v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Probabilistic `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (Probabilistic x) = MV_Probabilistic `liftM` M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_Probabilistic v) i = Probabilistic `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Probabilistic v) i (Probabilistic x) = M.basicUnsafeWrite v i x
  basicClear (MV_Probabilistic v) = M.basicClear v
  basicInitialize (MV_Probabilistic v) = M.basicInitialize v
  basicSet (MV_Probabilistic v) (Probabilistic x) = M.basicSet v x
  basicUnsafeCopy (MV_Probabilistic v1) (MV_Probabilistic v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_Probabilistic v) n = MV_Probabilistic `liftM` M.basicUnsafeGrow v n

instance (U.Unbox a) => G.Vector U.Vector (Probabilistic a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Probabilistic v) = V_Probabilistic `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Probabilistic v) = MV_Probabilistic `liftM` G.basicUnsafeThaw v
  basicLength (V_Probabilistic v) = G.basicLength v
  basicUnsafeSlice i n (V_Probabilistic v) = V_Probabilistic $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Probabilistic v) i = Probabilistic `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Probabilistic mv) (V_Probabilistic v) = G.basicUnsafeCopy mv v
  elemseq _ (Probabilistic x) z = G.elemseq (undefined :: U.Vector a) x z


newtype instance U.Vector    (Cost w) = V_Cost  (U.Vector    w)
newtype instance U.MVector s (Cost w) = MV_Cost (U.MVector s w)

instance (U.Unbox a) => U.Unbox (Cost a)

instance (U.Unbox a) => M.MVector U.MVector (Cost a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Cost v) = M.basicLength v
  basicUnsafeSlice i n (MV_Cost v) = MV_Cost $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Cost v1) (MV_Cost v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Cost `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (Cost x) = MV_Cost `liftM` M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_Cost v) i = Cost `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Cost v) i (Cost x) = M.basicUnsafeWrite v i x
  basicClear (MV_Cost v) = M.basicClear v
  basicInitialize (MV_Cost v) = M.basicInitialize v
  basicSet (MV_Cost v) (Cost x) = M.basicSet v x
  basicUnsafeCopy (MV_Cost v1) (MV_Cost v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeGrow (MV_Cost v) n = MV_Cost `liftM` M.basicUnsafeGrow v n

instance (U.Unbox a) => G.Vector U.Vector (Cost a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Cost v) = V_Cost `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Cost v) = MV_Cost `liftM` G.basicUnsafeThaw v
  basicLength (V_Cost v) = G.basicLength v
  basicUnsafeSlice i n (V_Cost v) = V_Cost $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Cost v) i = Cost `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Cost mv) (V_Cost v) = G.basicUnsafeCopy mv v
  elemseq _ (Cost x) z = G.elemseq (undefined :: U.Vector a) x z