-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Main
-- Description :  unified Vanda command line tool
-- Copyright   :  (c) Technische Universität Dresden 2015
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Brings together all Vanda functionality to provide a unified command line
-- tool with several sub-commands.
--
-- "System.Console.CmdArgs.Explicit" is used to provide the command line interface. The actual work is done in other @Main@ modules. These modules must expose a data type @Args@ that represents the local command line options, a @'Mode' Args@ that defines the local command line interface of the module, and a function @Args -> 'IO' ()@ to do the actual work.
--
-- In this module, the local command line definitions are 'remap'ped to provide the unified command line interface. Note that the 'modeNames' of the local 'Mode's are thereby used as names for the sub-commands.
-----------------------------------------------------------------------------

module Main
( main
, mainArgs
, cmdArgs
, Args()
) where


import           System.Console.CmdArgs.Explicit.Misc
import qualified Vanda.Algorithms.MATLearner.Main

import           System.Console.CmdArgs.Explicit


data Args
  = Help String
  | MATLearner Vanda.Algorithms.MATLearner.Main.Args
  deriving Show


cmdArgs :: Mode Args
cmdArgs
  = modes "vanda-gui" (Help $ defaultHelp cmdArgs) "Some GUI functionalities in Vanda"
  [ 
    remap2 MATLearner (\ (MATLearner x) -> x) Vanda.Algorithms.MATLearner.Main.cmdArgs
  ]


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()
mainArgs (Help      cs) = putStrLn cs
mainArgs (MATLearner x ) = Vanda.Algorithms.MATLearner.Main.mainArgs x
