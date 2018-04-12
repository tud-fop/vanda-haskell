{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

{-|
Module:      VandaCLI.NGrams
Description: functions to work n-gram models
Copyright:   Ⓒ Tobias Denkinger, 2015
License:     BSD-style
Maintainer:  Tobias.Denkinger@tu-dresden.de
Stability:   experimental

This module contains functions to work n-gram models.
-}

module VandaCLI.NGrams
( main
, mainArgs
, cmdArgs
, Args()
) where

import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import           Vanda.Grammar.NGrams.Functions (trainModel, writeNGrams, loadNGrams, evaluateLine)

import           System.Console.CmdArgs.Explicit
import           System.Console.CmdArgs.Explicit.Misc

data Args
  = Help String
  | Train
    { bound :: Int
    , degree :: Int
    }
  | Evaluate
    { model :: FilePath
    }
  deriving Show

cmdArgs :: Mode Args
cmdArgs
  = modes "ngrams" (Help $ defaultHelp cmdArgs) "algorithms for n-gram models"
  [ (modeEmpty $ Train 5 3)
    { modeNames = ["train"]
    , modeHelp = "trains an n-gram model (with Katz Backoff) given a corpus"
    , modeGroupFlags = toGroup [flagReqBound, flagReqDegree]
    }
  , (modeEmpty $ Evaluate undefined)
    { modeNames = ["evaluate", "parse"]
    , modeHelp = "evaluates sentences in a corpus according to an n-gram model"
    , modeArgs = ([ flagArgModel{argRequire = True} ], Nothing)
    }
  ]
  where
    flagReqBound
      = flagReq ["b", "bound"]
                (\ a x -> Right x {bound = read a})
                "BOUND"
                "consider n-grams that occur at least BOUND times as reliable (default: 5) "
    flagReqDegree
      = flagReq ["d", "degree"]
                (\ a x -> Right x {degree = read a})
                "DEGREE"
                "build an n-gram model of degree DEGREE (default: 3)"
    flagArgModel
      = flagArg (\ a x -> Right x {model = a}) "MODEL"


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs

mainArgs :: Args -> IO ()

mainArgs (Help cs) = putStr cs

mainArgs (Train k n)
  = do
      corpus <- TIO.getContents
      let nGrams = trainModel k n corpus
      TIO.putStr $ writeNGrams nGrams

mainArgs (Evaluate grammar)
  = do
      nGrams <- loadNGrams grammar
      input  <- TIO.getContents
      let wts = L.map (evaluateLine nGrams) . T.lines $ input
      TIO.putStr . T.unlines . flip map wts $ T.pack . show . exp
