{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

module Main
( main
, mainArgs
, cmdArgs
, Args()
) where


import           Codec.Compression.GZip ( compress )

import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import           Vanda.Algorithms.MATLearner.MATLearner (main')


import           System.Console.CmdArgs.Explicit
import           System.Console.CmdArgs.Explicit.Misc


data Args
  = Help String
  | Learn
    { teacher :: Teacher
    }
  deriving Show

data Teacher
  = Interactive | Corpus deriving (Eq, Show)


cmdArgs :: Mode Args
cmdArgs
  = modes "matlearner" (Help $ defaultHelp cmdArgs) "MAT-Learner algorithm for different types of teachers"
  [ (modeEmpty $ Learn undefined)
    { modeNames = ["interactive"]
    , modeHelp = "MAT-Learner algorithm with an interactive teacher"
    , modeArgs = ( [flagArg (\ a x -> Right x{teacher = Interactive}) ""], Nothing )
    , modeGroupFlags = toGroup []
    }
  ]

main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()

mainArgs (Help cs) = putStr $ cs

mainArgs (Learn t)
  = case t of Interactive       -> putStrLn "YNE"
              Corpus            -> putStrLn "NYE"
