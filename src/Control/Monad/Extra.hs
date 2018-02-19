-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Extra
-- Copyright   :  (c) Technische Universität Dresden 2016
-- License     :  BSD-style
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Control.Monad.Extra where


-- | An if-then-else where the predicate is a monadic action.
ifM
  :: Monad m
  => m Bool  -- ^ predicate
  -> m b     -- ^ result if predicate returns 'True'
  -> m b     -- ^ result if predicate returns 'False'
  -> m b
ifM predicateM thn els = do
  b <- predicateM
  if b then thn else els
