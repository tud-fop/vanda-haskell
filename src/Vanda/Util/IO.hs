-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Util.IO
-- Copyright   :  (c) Technische Universität Dresden 2014-2016
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------


module Vanda.Util.IO where


import Control.Concurrent
import Control.Exception (finally)
import Control.Monad
import Data.Foldable (for_, traverse_)
import Data.Traversable (for)
import System.Posix.Signals


-- | The call @handleOnDemand mn mt signals worker handler@ runs @worker@ and
-- passes an action to fix/update a (partial) result. If there currently is a
-- fixed result, then @handler@ is called with that result on the following
-- events:
--
-- * termination of worker,
-- * if @mn = 'Just' n@, then for every @n@-th fixed result,
-- * if @mt = 'Just' t@, then every @t@ microseconds,
-- * any 'Signal' from @signals@,
-- * exception in worker (the exception is reraised); this includes 'sigINT',
--   if it is not handled by @worker@ and it is not element of @signals@.
--
-- For each result @handler@ is called at most once.
-- It is guaranteed that there is only one thread in @handler@.
handleOnDemand
  :: Maybe Int                -- ^ handler interval in number of results
  -> Maybe Int                -- ^ handler interval in microseconds
  -> [Signal]                 -- ^ signals on which handler is called
  -> ((a -> IO ()) -> IO ())  -- ^ worker
  -> (a -> IO ())             -- ^ handler
  -> IO ()
handleOnDemand mn mt signals worker handler = do
  varResult <- newEmptyMVar
  varCount <- newMVar (1 :: Int)  -- this also acts as mutex in update
  mutex <- newMVar ()
  let handlerMutexed = withMVar mutex . const . handler
  let handlerVar = tryTakeMVar varResult >>= traverse_ handlerMutexed
  let update result = do
        i <- takeMVar varCount
        _ <- tryTakeMVar varResult
        if maybe False ((0 ==) . (i `mod`)) mn
          then void $ forkIO $ handlerMutexed result
          else putMVar varResult result
        putMVar varCount $! succ i
  for_ signals $ \ s -> installHandler s (Catch handlerVar) Nothing
  mtidTimer <- for mt $ \ t ->
    forkIO $ forever $ void $ threadDelay t >> forkIO handlerVar
  finally (worker update) $ do
    for_ mtidTimer killThread
    handlerVar
    takeMVar mutex  -- wait for handlers to finish
