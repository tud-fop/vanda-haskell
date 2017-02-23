module Vanda.Grammar.PMCFG.Wip.Weights where

import Numeric.Log (Log(Exp), Precise)
import Data.Group (Group(invert))


-- | Multiplicative monoid for probabilistic weights.
newtype Probabilistic a = Probabilistic a deriving (Eq, Ord, Show)


-- | Wraps constructor to check for correct ranged values, stores value in log domain.
probabilistic :: (Precise a, Ord a) => a -> Probabilistic (Log a)
probabilistic x
  | x > 0 && x <= 1 = Probabilistic $ Exp $ log x
  | otherwise = error "probabilistic value out of range"


-- | Additive monoid for costs.
newtype Cost a = Cost a deriving (Show, Eq)


-- | Wraps constructor to check for correct ranged values.
cost :: (Num a, Ord a) => a -> Cost a
cost x
  | x >= 0 = Cost x
  | otherwise = error "cost value out of range"


class (Monoid d) => Dividable d where
  -- | Divides an object into a given amount of subobjects.
  -- It should be divided s.t. mconcat (divide x n) = x.
  divide :: d -> Int -> [d]


-- | Instance of multiplicative monoid.
instance (Num a) => Monoid (Probabilistic a) where
  mempty = Probabilistic 1
  (Probabilistic x) `mappend` (Probabilistic y) = Probabilistic $ x * y


-- | Probabilistic group
instance (Floating a) => Group (Probabilistic a) where
  invert (Probabilistic x) = Probabilistic $ 1 / x


-- | Uses root to divide a probability into n subprobabilities.
instance (Floating a) => Dividable (Probabilistic a) where
  divide (Probabilistic x) rt = replicate rt $ Probabilistic $ x ** (1 / fromIntegral rt)


-- | Instance of additive monoid.
instance (Num a) => Monoid (Cost a) where
  mempty = Cost 0
  (Cost x) `mappend` (Cost y) = Cost $ x + y


instance (Num a) => Group (Cost a) where
  invert (Cost x) = Cost $ negate x


-- | Divides by division.
instance (Fractional a) => Dividable (Cost a) where
  divide (Cost x) d = replicate d $ Cost $ x / fromIntegral d


-- | Uses inverted comparison to find best results with least cost.
instance (Ord a) => Ord (Cost a) where
  (Cost x) `compare` (Cost y) = y `compare` x