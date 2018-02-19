-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.Shuffle
-- Description :  shuffling lists
-- Copyright   :  (c) Technische Universität Dresden 2016
-- License     :  BSD-style
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Shuffling list.
-----------------------------------------------------------------------------

module Data.List.Shuffle
( shuffle
) where


import Control.Monad (forM)
import Control.Monad.ST
import Data.Array.MArray.Safe (newListArray)
import Data.Array.ST.Safe
import Data.STRef
import System.Random (RandomGen, randomR)


-- | Shuffle a list using a given 'RandomGen'.
--
-- The solution is based on the
-- [Haskell Wiki](https://wiki.haskell.org/Random_shuffle#Imperative_algorithm).
shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle xs g = runST $ do
    gST <- newSTRef g
    aST <- newListArrayST (0, hi) xs
    xs' <- forM [0 .. hi] $ \ i -> do
      j  <- stateST gST $ randomR (i, hi)
      vi <- readArray aST i
      vj <- readArray aST j
      writeArray aST j vi
      return vj
    g' <- readSTRef gST
    return (xs', g')
  where
    hi = pred $ length xs


-- | Identical to 'newListArray', but with more specific type.
newListArrayST :: Ix i => (i, i) -> [e] -> ST s (STArray s i e)
newListArrayST = newListArray


-- | Like 'Control.Monad.Trans.State.Lazy.state', but for "Control.Monad.ST"
-- and "Data.STRef".
stateST :: STRef s a -> (a -> (b, a)) -> ST s b
stateST s f = do
  (b, a) <- f <$> readSTRef s
  writeSTRef s a
  return b
