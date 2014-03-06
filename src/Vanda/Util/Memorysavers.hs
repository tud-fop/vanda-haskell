-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Util.Memorysavers
-- Copyright   :  (c) Technische Universität Dresden 2014
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Save memory by rebuilding 'Traversable' data structures.
--
-----------------------------------------------------------------------------

module Vanda.Util.Memorysavers
( -- * Porcelain

  -- ** Intify
  -- | Replace the values in a 'Traversable' data structure by 'Int's in an
  -- injective manner. The (partial) injection is returned in a 'M.Map'.
   intify1, intify2, intify3

, -- ** Unify
  -- | Replace equal values (with respect to 'Eq') in a 'Traversable' data
  -- structure by identical instances, e.g.
  -- @'unify1' [1 + 2, 3] == let x = 3 in [x, x]@.
  unify1, unify2, unify3

, -- * Plumbing
  intifyS0, intifyS1, intifyS2, intifyS3
, unifyS0, unifyS1, unifyS2, unifyS3
) where

import Prelude hiding (mapM)

import Control.Monad.State.Lazy hiding (mapM)
import qualified Data.Map.Lazy as M
import Data.Traversable (Traversable, mapM)


intify1 :: (Ord k, Traversable t1) => t1 k -> (t1 Int, M.Map k Int)
intify1 x = runState (intifyS1 x) M.empty


intify2
  :: (Ord k, Traversable t1, Traversable t2)
  => t2 (t1 k) -> (t2 (t1 Int), M.Map k Int)
intify2 x = runState (intifyS2 x) M.empty


intify3
  :: (Ord k, Traversable t1, Traversable t2, Traversable t3)
  => t3 (t2 (t1 k)) -> (t3 (t2 (t1 Int)), M.Map k Int)
intify3 x = runState (intifyS3 x) M.empty


unify1 :: (Ord k, Traversable t1) => t1 k -> t1 k
unify1 x = evalState (unifyS1 x) M.empty


unify2 :: (Ord k, Traversable t1, Traversable t2) => t2 (t1 k) -> t2 (t1 k)
unify2 x = evalState (unifyS2 x) M.empty


unify3
  :: (Ord k, Traversable t1, Traversable t2, Traversable t3)
  => t3 (t2 (t1 k)) -> t3 (t2 (t1 k))
unify3 x = evalState (unifyS3 x) M.empty


-- | The actual worker for the @intify@ functions. Updates the 'M.Map' as
-- needed. The initial 'M.Map' must be a partial injection (e.g. 'M.empty'),
-- and all 'M.elems' of the 'M.Map' must be smaller than the 'M.size' of the
-- 'M.Map'. These conditions are preserved by this function, but they are
-- /not/ checked for the initial 'M.Map'.
intifyS0 :: Ord k => k -> State (M.Map k Int) Int
intifyS0 k = do
  m <- get
  case M.lookup k m of
    Just v  -> return v
    Nothing -> let v = M.size m in put (M.insert k v m) >> return v


-- | @'mapM' 'intifyS0'@
intifyS1 :: (Ord k, Traversable t1) => t1 k -> State (M.Map k Int) (t1 Int)
intifyS1 = mapM intifyS0


-- | @'mapM' ('mapM' 'intifyS0')@
intifyS2
  :: (Ord k, Traversable t1, Traversable t2)
  => t2 (t1 k) -> State (M.Map k Int) (t2 (t1 Int))
intifyS2 = mapM intifyS1


-- | @'mapM' ('mapM' ('mapM' 'intifyS0'))@
intifyS3
  :: (Ord k, Traversable t1, Traversable t2, Traversable t3)
  => t3 (t2 (t1 k)) -> State (M.Map k Int) (t3 (t2 (t1 Int)))
intifyS3 = mapM intifyS2


-- | The actual worker for the @unify@ functions. Updates the 'M.Map' as
-- needed. The initial 'M.Map' must be a partial identity (e.g. 'M.empty').
-- This condition is preserved by this function, but it is /not/ checked for
-- the initial 'M.Map'.
unifyS0 :: Ord k => k -> State (M.Map k k) k
unifyS0 k = do
  m <- get
  case M.lookup k m of
    Just v  -> return v
    Nothing -> put (M.insert k k m) >> return k


-- | @'mapM' 'unifyS0'@
unifyS1 :: (Ord k, Traversable t1) => t1 k -> State (M.Map k k) (t1 k)
unifyS1 = mapM unifyS0


-- | @'mapM' ('mapM' 'unifyS0')@
unifyS2
  :: (Ord k, Traversable t1, Traversable t2)
  => t2 (t1 k) -> State (M.Map k k) (t2 (t1 k))
unifyS2 = mapM unifyS1


-- | @'mapM' ('mapM' ('mapM' 'unifyS0'))@
unifyS3
  :: (Ord k, Traversable t1, Traversable t2, Traversable t3)
  => t3 (t2 (t1 k)) -> State (M.Map k k) (t3 (t2 (t1 k)))
unifyS3 = mapM unifyS2
