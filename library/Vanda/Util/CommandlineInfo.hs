-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Util.CommandlineInfo
-- Description :  gather information about command line and environment
-- Copyright   :  (c) Technische Universität Dresden 2017
-- License     :  BSD-style
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Gather information about the command line and the environment of the
-- current process.
-----------------------------------------------------------------------------

module Vanda.Util.CommandlineInfo where


import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import System.Environment

import Vanda.Util.ShellEscape (shellescape)


-- | Gather information about the command line and the environment of the
-- current process in a human readable 'String'.
commandlineInfo :: IO String
commandlineInfo = do
  wallclockTime  <- formatTime defaultTimeLocale "%F %T %Z" <$> getZonedTime
  args           <- getArgs
  progName       <- getProgName
  executablePath <- getExecutablePath
  environment    <- getEnvironment
  return $ unlines $
    [ wallclockTime
    , ""
    , executablePath
    , ""
    , unwords $ map shellescape $ progName : args
    , ""
    ] ++ map (\ (v, s) -> v ++ "=" ++ shellescape s) environment
