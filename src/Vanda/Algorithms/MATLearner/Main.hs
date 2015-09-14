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
import Vanda.Algorithms.MATLearner.Util

data Args
  = Help String
  | InteractiveArg
    { verbose :: Bool
    }
  | CorpusArg
    { verbose :: Bool
    , corpus :: FilePath 
    }
  | AutomatonArg
    { verbose :: Bool
    , automaton :: FilePath
    }
  deriving Show



cmdArgs :: Mode Args
cmdArgs
  = modes "matlearner" (Help $ defaultHelp cmdArgs) "MAT-Learner algorithm for different types of teachers"
  [ (modeEmpty $ InteractiveArg False)
    { modeNames = ["interactive"]
    , modeHelp = "MAT-Learner algorithm with an interactive teacher"
    , modeArgs = ( [], Nothing )
    , modeGroupFlags = toGroup [flagNoneVerbose]
    }
  , (modeEmpty $ CorpusArg False undefined)
    { modeNames = ["corpus"]
    , modeHelp = "MAT-Learner algorithm with a corpus"
    , modeArgs = ( [flagArgCorpus{argRequire = True}], Nothing )
    , modeGroupFlags = toGroup [flagNoneVerbose]
    }
  , (modeEmpty $ AutomatonArg False undefined)
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

mainArgs (InteractiveArg v)
  = case v of True      -> putStrLn "Doing verbose Interactive Learn stuff"
              False     -> nonVerboseInteractive
              
mainArgs (CorpusArg v filepath)
  = case v of True      -> putStrLn $ "Doing verbose Learn stuff with the corpus located at " ++ filepath
              False     -> nonVerboseCorpus filepath
mainArgs (AutomatonArg v filepath)
  = case v of True      -> putStrLn $ "Doing verbose Learn stuff with the automaton located at " ++ filepath
              False     -> putStrLn $ "Doing NONverbose Learn stuff with the automaton located at " ++ filepath
              
              
nonVerboseInteractive :: IO ()
nonVerboseInteractive = do
  putStrLn "Your Tree-Language consists of the following alphabet:"
  alphabet <- getSigma Interactive
  putStrLn $ show alphabet ++ "\n"
  automat <- main' Interactive
  putStrLn $ show automat
  
nonVerboseCorpus :: FilePath -> IO ()
nonVerboseCorpus filepath = do
  forest <- parseFile filepath parseCorpus
  automat <- main' $ Corpus forest
  putStrLn $ show automat
