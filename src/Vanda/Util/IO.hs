-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Util.IO
-- Copyright   :  (c) Technische Universität Dresden 2014
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


import Control.Concurrent.MVar
import Control.Exception (catch, SomeException)
import System.Posix.Signals


-- | Calls worker with an action to fix/update a (partial) result.
-- Calls handler on completion of worker, 'sigUSR1', or any exception in
-- worker, including 'sigINT', if the worker fixed a result. A result is
-- removed after it was passed to handler. It is guaranteed that there is only
-- one thread in handler.
handleInterrupt
  :: ((a -> IO ()) -> IO ())  -- ^ worker
  -> (a -> IO ())             -- ^ handler
  -> IO ()
handleInterrupt worker handler = do
  varResult <- newEmptyMVar
  mutex <- newMVar ()
  let handler' = do
        maybeResult <- tryTakeMVar varResult
        case maybeResult of
          Nothing -> putStrLn "There is currently no (new) result."
          Just result -> withMVar mutex (const (handler result))
  let update result = tryTakeMVar varResult >> putMVar varResult result
  _ <- installHandler sigUSR1 (Catch handler') Nothing
  catch (worker update) $ \ e ->
    putStr "Worker: " >> print (e :: SomeException)
  handler'
