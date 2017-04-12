-----------------------------------------------------------------------------
-- |
-- Module      :  LimitedQueue
-- Copyright   :  (c) Thomas Ruprecht 2017
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  thomas.ruprecht@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Contains a wrapper of "Data.Semiring" used for an efficient update of
-- weights and instances for "Data.Semiring" and 'Weight'.
-----------------------------------------------------------------------------

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


import Numeric.Log (Log(Exp), Precise)
import Data.Semiring
import Data.Converging
import Prelude hiding (read)


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


-- | Instance of multiplicative monoid.
instance (Num a, Ord a) => Monoid (Probabilistic a) where
  mempty = Probabilistic 0
  (Probabilistic x) `mappend` (Probabilistic y) = Probabilistic $ max x y


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


-- | Instance minimum monoid.
instance (Num a, Ord a) => Monoid (Cost a) where
  mempty = Infinity
  (Cost x) `mappend` (Cost y) = Cost $ min x y
  Infinity `mappend` x = x
  x `mappend` Infinity = x


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

instance (Num a) => Monoid (Inside a) where
  mempty = Inside 0
  (Inside x) `mappend` (Inside y) = Inside $ x + y

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