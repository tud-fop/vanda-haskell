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
import Vanda.Algorithms.MATLearner.Teacher

data Args
  = Help String
  | InteractiveArg
    { verbose :: Bool
    , string :: Bool
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
  [ (modeEmpty $ InteractiveArg False False)
    { modeNames = ["interactive"]
    , modeHelp = "MAT-Learner algorithm with an interactive teacher, using a default ranked alphabet"
    , modeArgs = ( [], Nothing )
    , modeGroupFlags = toGroup [flagNoneVerbose, flagNoneStringAlphabet]
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
    flagNoneStringAlphabet
      = flagNone ["s"]
                 (\ x -> x{string = True})
                 "use a string alphabet, simplifying the queries"
      
main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()

mainArgs (Help cs) = putStr $ cs

mainArgs (InteractiveArg v s)
  =  case v of True     -> verboseInteractive s
               False    -> nonVerboseInteractive s
              
mainArgs (CorpusArg v filepath)
  = case v of True      -> verboseCorpus filepath
              False     -> nonVerboseCorpus filepath
mainArgs (AutomatonArg v filepath)
  = case v of True      -> verboseAutomaton filepath
              False     -> nonVerboseAutomaton filepath
              
   
verboseInteractive :: Bool -> IO ()
verboseInteractive stringAlphabet = error "not working"
    
nonVerboseInteractive :: Bool -> IO ()
nonVerboseInteractive b = matLearner
  
verboseCorpus :: FilePath -> IO ()
verboseCorpus filepath = error "not working"

nonVerboseCorpus :: FilePath -> IO ()
nonVerboseCorpus filepath = error "not working"
  
verboseAutomaton :: FilePath -> IO ()
verboseAutomaton filepath = error "not working"
  
nonVerboseAutomaton :: FilePath -> IO ()
nonVerboseAutomaton filepath = error "not working"
