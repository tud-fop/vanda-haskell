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
  -- injective manner. The (partial) injection is returned in a 'M.Map' and
  -- its inverse is returned in an 'A.Array'.
  --
  -- The primed variants ensure that the resulting data structure is evaluated
  -- before the 'M.Map' and the 'A.Array' to prevent stack overflows. If your
  -- code ensures this anyway, you may prefer the slightly faster unprimed
  -- variants.
  intify1, intify1'
, intify2, intify2'
, intify3, intify3'

, -- ** Unify
  -- | Replace equal values (with respect to 'Eq') in a 'Traversable' data
  -- structure by identical instances, e.g.
  -- @'unify1' [1 + 2, 3] == let x = 3 in [x, x]@.
  unify1, unify2, unify3

, -- * Plumbing
  -- $plumbing
  invertMap
, intifyS0, intifyS1, intifyS2, intifyS3
, unifyS0, unifyS1, unifyS2, unifyS3
) where

import Prelude hiding (mapM)

import Control.Monad.Trans.State.Lazy (State, evalState, get, put, runState)
import Control.Parallel.Strategies
import qualified Data.Array as A
import qualified Data.Map.Lazy as M
import Data.Traversable (Traversable, mapM)
import Data.Tuple (swap)


intify1, intify1'
  :: (Ord e, Traversable t1)
  =>  t1 e
  -> (t1 Int, (M.Map e Int, A.Array Int e))
intify1 = doIntify intifyS1
intify1'
  = withStrategy (evalTuple2 (evalTraversable rseq) r0)
  . intify1


intify2, intify2'
  :: (Ord e, Traversable t1, Traversable t2)
  =>  t2 (t1 e)
  -> (t2 (t1 Int), (M.Map e Int, A.Array Int e))
intify2 = doIntify intifyS2
intify2'
  = withStrategy (evalTuple2 (evalTraversable (evalTraversable rseq)) r0)
  . intify2


intify3, intify3'
  :: (Ord e, Traversable t1, Traversable t2, Traversable t3)
  =>  t3 (t2 (t1 e))
  -> (t3 (t2 (t1 Int)), (M.Map e Int, A.Array Int e))
intify3 = doIntify intifyS3
intify3'
  = withStrategy
    (evalTuple2 (evalTraversable (evalTraversable (evalTraversable rseq))) r0)
  . intify3


unify1 :: (Ord k, Traversable t1) => t1 k -> t1 k
unify1 x = evalState (unifyS1 x) M.empty


unify2 :: (Ord k, Traversable t1, Traversable t2) => t2 (t1 k) -> t2 (t1 k)
unify2 x = evalState (unifyS2 x) M.empty


unify3
  :: (Ord k, Traversable t1, Traversable t2, Traversable t3)
  => t3 (t2 (t1 k)) -> t3 (t2 (t1 k))
unify3 x = evalState (unifyS3 x) M.empty


doIntify
  :: (s -> State (M.Map e Int) t)
  -> s
  -> (t, (M.Map e Int, A.Array Int e))
doIntify intifyS
  = \ x -> let (y, m) = runState (intifyS x) M.empty
            in (y, (m, invertMap m))


-- $plumbing
-- We will use the following terms:
--
-- - injectiveness: A 'M.Map' is injective, if it maps different keys to
--   different values.
--
-- - size-boundedness: A 'M.Map' is size-bounded, if all its 'M.elems' are
--   smaller than the 'M.size' of the 'M.Map'.
--
-- - inverve-array-likeness: A 'M.Map' is inverve-array-like, if 'M.elems' of
--   the 'M.Map' contains every 'Int' from 0 to @('M.size' - 1)@ exactly once.
--
-- Note that inverse-array-likeness implies size-boundedness and
-- injectiveness.


-- | Invert an inverse-array-like 'M.Map'.
-- /Inverse-array-likeness is not checked!/
invertMap :: M.Map k Int -> A.Array Int k
invertMap m = A.array (0, M.size m - 1) $ map swap $ M.assocs m


-- | The actual worker for the @intify@ functions. Updates the 'M.Map' as
-- needed. The initial 'M.Map' must be a injective and size-bounded.
-- /This is not checked!/ Inverve-array-likeness is not needed, yet,
-- 'intifyS0' preserves injectiveness, size-boundedness and
-- inverse-array-likeness of the 'M.Map'.
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
