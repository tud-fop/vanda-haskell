{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

module Vanda.Algorithms.MATLearner.Main
( main
, mainArgs
, cmdArgs
, Args()
) where

import Vanda.Alforithms.MATLearner.MATLearner
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Explicit.Misc


data Args
  = Help String
  | Interactive
    { verbose :: Bool
    }
  | Corpus
    { verbose :: Bool
    , corpus :: FilePath 
    }
  | Automaton
    { verbose :: Bool
    , automaton :: FilePath
    }
  deriving Show



cmdArgs :: Mode Args
cmdArgs
  = modes "matlearner" (Help $ defaultHelp cmdArgs) "MAT-Learner algorithm for different types of teachers"
  [ (modeEmpty $ Interactive False)
    { modeNames = ["interactive"]
    , modeHelp = "MAT-Learner algorithm with an interactive teacher"
    , modeArgs = ( [], Nothing )
    , modeGroupFlags = toGroup [flagNoneVerbose]
    }
  , (modeEmpty $ Corpus False undefined)
    { modeNames = ["corpus"]
    , modeHelp = "MAT-Learner algorithm with a corpus"
    , modeArgs = ( [flagArgCorpus{argRequire = True}], Nothing )
    , modeGroupFlags = toGroup [flagNoneVerbose]
    }
  , (modeEmpty $ Automaton False undefined)
    { modeNames = ["automaton"]
    , modeHelp = "MAT-Learner algorithm with an automaton"
    , modeArgs = ( [flagArgAutomaton{argRequire = True}], Nothing )
    , modeGroupFlags = toGroup [flagNoneVerbose]
    }
  ]
  where
    flagArgAutomaton
      = flagArg (\ a x -> Right x{automaton = a}) "AUTOMATON"
    flagArgCorpus
      = flagArg (\ a x -> Right x{corpus = a}) "CORPUS"
    flagNoneVerbose
      = flagNone ["v"]
                 (\ x -> x{verbose = True})
                 "regularly output the observation table"
main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()

mainArgs (Help cs) = putStr $ cs

mainArgs (Interactive v)
  = case v of True      -> putStrLn "Doing verbose Interactive Learn stuff"
              False     -> putStrLn "Doing NONverbose Interactive Learn stuff"
              
mainArgs (Corpus v filepath)
  = case v of True      -> putStrLn $ "Doing verbose Learn stuff with the corpus located at " ++ filepath
              False     -> putStrLn $ "Doing NONverbose Learn stuff with the corpus located at " ++ filepath
mainArgs (Automaton v filepath)
  = case v of True      -> putStrLn $ "Doing verbose Learn stuff with the automaton located at " ++ filepath
              False     -> putStrLn $ "Doing NONverbose Learn stuff with the automaton located at " ++ filepath
