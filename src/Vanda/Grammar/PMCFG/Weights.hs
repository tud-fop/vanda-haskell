module Vanda.Grammar.PMCFG.Weights where

import Numeric.Log (Log(Exp), Precise)
import Data.Semiring


-- | Multiplicative monoid for probabilistic weights.
newtype Probabilistic a = Probabilistic a deriving (Eq, Show)


instance (Ord a) => Ord (Probabilistic a) where
  (Probabilistic x) `compare` (Probabilistic y) = y `compare` x


-- | Wraps constructor to check for correct ranged values, stores value in log domain.
probabilistic :: (Precise a, Ord a) => a -> Probabilistic (Log a)
probabilistic x
  | x > 0 && x <= 1 = Probabilistic $ Exp $ log x
  | otherwise = error "probabilistic value out of range"


-- | Additive monoid for costs.
data Cost a = Cost a | Infinity deriving (Show, Eq)


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


-- | Instance of multiplicative monoid.
instance (Num a, Ord a) => Monoid (Probabilistic a) where
  mempty = Probabilistic 0
  (Probabilistic x) `mappend` (Probabilistic y) = Probabilistic $ max x y


-- | Probabilistic group
instance (Num a, Ord a) => Semiring (Probabilistic a) where
  one = Probabilistic 1
  (Probabilistic x) <.> (Probabilistic y) = Probabilistic $ x * y


-- | Instance of additive monoid.
instance (Num a, Ord a) => Monoid (Cost a) where
  mempty = Infinity
  (Cost x) `mappend` (Cost y) = Cost $ min x y
  Infinity `mappend` x = x
  x `mappend` Infinity = x


instance (Num a, Ord a) => Semiring (Cost a) where
  one = Cost 0
  Infinity <.> x = Infinity
  x <.> Infinity = Infinity
  (Cost x) <.> (Cost y) = Cost $ x + y 