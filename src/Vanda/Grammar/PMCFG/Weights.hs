module Vanda.Grammar.PMCFG.Weights where

import Numeric.Log (Log(Exp), Precise)
import Data.Group (Group(invert))


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
newtype Cost a = Cost a deriving (Show, Eq, Ord)


-- | Wraps constructor to check for correct ranged values.
cost :: (Num a, Ord a) => a -> Cost a
cost x
  | x >= 0 = Cost x
  | otherwise = error "cost value out of range"


-- | Instance of multiplicative monoid.
instance (Num a) => Monoid (Probabilistic a) where
  mempty = Probabilistic 1
  (Probabilistic x) `mappend` (Probabilistic y) = Probabilistic $ x * y


-- | Probabilistic group
instance (Floating a) => Group (Probabilistic a) where
  invert (Probabilistic x) = Probabilistic $ 1 / x


-- | Instance of additive monoid.
instance (Num a) => Monoid (Cost a) where
  mempty = Cost 0
  (Cost x) `mappend` (Cost y) = Cost $ x + y


instance (Num a) => Group (Cost a) where
  invert (Cost x) = Cost $ negate x