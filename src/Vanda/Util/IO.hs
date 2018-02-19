-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Util.IO
-- Copyright   :  (c) Technische Universität Dresden 2014-2016
-- License     :  BSD-style
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------


module Vanda.Util.IO
( handleOnDemand
, createForkerWithWaiter
)where


import Control.Concurrent
import Control.Concurrent.STM  -- package stm
         (atomically, modifyTVar', newTVarIO, readTVar, retry, writeTVar)
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
  (fork, wait) <- createForkerWithWaiter
  varResult <- newEmptyMVar
  varCount <- newMVar (1 :: Int)  -- this also acts as mutex in update
  mutex <- newMVar ()
  let handlerMutexed = withMVar mutex . const . handler
  let handlerVar = tryTakeMVar varResult >>= traverse_ handlerMutexed
  let update result = do
        i <- takeMVar varCount
        _ <- tryTakeMVar varResult
        if maybe False ((0 ==) . (i `mod`)) mn
          then void $ fork $ handlerMutexed result
          else putMVar varResult result
        putMVar varCount $! succ i
  for_ signals $ \ s -> installHandler s (Catch handlerVar) Nothing
  mtidTimer <- for mt $ \ t ->
    forkIO $ forever $ void $ threadDelay t >> fork handlerVar
  finally (worker update) $ do
    for_ mtidTimer killThread
    handlerVar
    wait


-- | Returns a function @fork@ and an action @wait@.
--
-- Use @fork@ to spawn new threads and use @wait@ to block until all those
-- spawned threads terminate.
--
-- Spawning new threads using @fork@ will fail as soon as @wait@ returned.
--
-- /Examples:/
--
-- Spawn a thread, do something else, wait for the thread’s termination.
--
-- > do (fork, wait) <- createForkerWithWaiter
-- >    void $ fork $ sub-thread-action
-- >    main-thread-action
-- >    wait
--
-- Using @fork@ after @wait@ will always fail.
--
-- > do (fork, wait) <- createForkerWithWaiter
-- >    wait
-- >    void $ fork $ never-executed-action
--
-- Race condition: @sub-sub-thread-action@ may or may not be run, depending
-- on whether @fork@ or @wait@ is executed first. Note that @wait@ does not
-- wait for the thread created with 'forkIO'.
--
-- > do (fork, wait) <- createForkerWithWaiter
-- >    void $ forkIO $ do
-- >      sub-thread-action
-- >      void $ fork $ sub-sub-thread-action
-- >    main-thread-action
-- >    wait
--
-- Deadlock: A thread waiting for itself to terminate.
--
-- > do (fork, wait) <- createForkerWithWaiter
-- >    void $ fork $ wait
createForkerWithWaiter :: IO (IO () -> IO (Maybe ThreadId), IO ())
createForkerWithWaiter = do
  nTVar <- newTVarIO (0 :: Int)
  -- We count the number of running forks in this variable. The waiter will
  -- set this variable to a negative number as soon as there is no running
  -- fork. This prevents any future fork.
  let fork act = do
        abort <- atomically $ do
          n <- readTVar nTVar
          if n < 0 then return True
                   else writeTVar nTVar (succ n) >> return False
        if abort then return Nothing
                 else fmap Just
                    $ forkFinally act
                    $ const $ atomically $ modifyTVar' nTVar pred
  let wait = atomically $ do
        n <- readTVar nTVar
        if n > 0 then retry
                 else writeTVar nTVar (pred n)
  return (fork, wait)


{-
testHandleOnDemand :: IO ()
testHandleOnDemand = do
  putStrLn "=== main thread started ==="
  handleOnDemand (Just 3) (Just 7000000) [sigUSR1] (worker [1 .. 10]) handler
  putStrLn "=== main thread ended gracefully ==="
  where
    worker :: [Int] -> (Int -> IO ()) -> IO ()
    worker [] _ = return ()
    worker (i : is) update = do
      putStrLn $ "Iteration " ++ show i ++ " started"
      threadDelay 1000000
      update i
      putStrLn $ "Iteration " ++ show i ++ " done"
      worker is update

    handler :: Int -> IO ()
    handler i = do
      putStrLn $ shift ++ "Handling " ++ show i ++ " started"
      threadDelay 3000000
      putStrLn $ shift ++ "Handling " ++ show i ++ " done"

    shift = replicate 25 ' '


testCreateForkerWithWaiter :: IO ()
testCreateForkerWithWaiter = do
  (fork, wait) <- createForkerWithWaiter
  mtid <- fork $ do
    threadDelay 10000
    putStrLnWithTId "Forked successfully."
    threadDelay 2000000
    putStrLnWithTId "Attempting another fork ..."
    void $ fork $ do
      threadDelay 10000
      putStrLnWithTId "Forked successfully."
      threadDelay 2000000
      putStrLnWithTId "Done."
    putStrLnWithTId "Done."
  threadDelay 1000000
  -- traverse_ killThread mtid
  putStrLnWithTId $ show mtid
  putStrLnWithTId "Waiting for threads to finish ..."
  wait
  putStrLnWithTId "All threads finished."
  fork (putStrLnWithTId "Unreachable") >>= putStrLnWithTId . show


putStrLnWithTId :: String -> IO ()
putStrLnWithTId cs = do
  tid <- myThreadId
  putStrLn $ show tid ++ ": " ++ cs
-}
