-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Technische Universität Dresden 2018
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------


module Data.Converging
  ( Converging(..)
  , Viterbi(..)
  , convergedRatio
  ) where

import Numeric.Log (Log(Exp))
  
-- | The property @convergedRatio epsilon x y@ holds, iff the ratio between
-- @x@ and @y@ differs at most @epsilon@ from @1@.
convergedRatio :: (Ord a, Num a) => a -> a -> a -> Bool
convergedRatio epsilon x y
  = let (mi, ma) = if x < y then (x, y) else (y, x)
    in ma - mi <= ma * epsilon


-- | @True@, iff both arguments are equal or both are @NaN@.
convergedRealFloat :: (RealFloat a) => a -> a -> Bool
convergedRealFloat x y = x == y || (isNaN x && isNaN y)


-- | The class contains types whose elements can converge against a fixpoint
-- of a function.
class Converging a where
  -- | The property @converged x y@ holds, iff @x@ and @y@ are values of
  -- consecutive steps in a fixpoint iteration and @x@ and @y@ are close
  -- enough such that another iteration step would probably not improve
  -- the result significantly.
  converged :: a -> a -> Bool

instance Converging Float where
  converged = convergedRealFloat

instance Converging Double where
  converged = convergedRealFloat

instance Converging Int where
  converged = (==)

instance (Converging a) => Converging (Log a) where
  converged (Exp e1) (Exp e2) = e1 `converged` e2

-- | This wrapper should allow us to use the same fixpoint computation
-- we used to compute inside/outside sums in order to calculate
-- Viterbi scores.
newtype Viterbi a = Viterbi { unViterbi :: a } deriving (Eq, Ord, Show)

instance (Ord a, Num a) => Num (Viterbi a) where
  a + b       = Viterbi (unViterbi a `max` unViterbi b)
  (-)         = undefined
  a * b       = Viterbi (unViterbi a * unViterbi b)
  abs         = Viterbi . abs . unViterbi
  fromInteger = Viterbi . fromInteger
  signum      = Viterbi . signum . unViterbi

instance Converging a => Converging (Viterbi a) where
  a `converged` b = unViterbi a `converged` unViterbi b
