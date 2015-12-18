{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

module Vanda.Algorithms.MATLearner.Main
( main
, mainArgs
, cmdArgs
, Args()
) where

import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Explicit.Misc
import Vanda.Algorithms.MATLearner.MATLearner

data Args
  = Help String
  | InteractiveArg
  deriving Show



cmdArgs :: Mode Args
cmdArgs
  = modes "matlearner" (Help $ defaultHelp cmdArgs) "implementation of the MAT-Learner algorithm"
  [ (modeEmpty InteractiveArg)
    { modeNames = ["interactive"]
    , modeHelp = "opens a window interface for showcasing the MAT-Learner algorithm"
    , modeArgs = ( [], Nothing )
    , modeGroupFlags = toGroup []
    }
  ]
      
main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()

mainArgs (Help cs) = putStr $ cs

mainArgs InteractiveArg
  =  matLearner