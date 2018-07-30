-----------------------------------------------------------------------------
-- |
-- Module      :  VandaCLI.PennToSentenceCorpus
-- Copyright   :  (c) Technische Universität Dresden 2018
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module VandaCLI.PennToSentenceCorpus where

import Vanda.Corpus.Penn.Text
import qualified Data.Text.Lazy.IO as TIO
import Data.Tree

import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Explicit.Misc

data Args = Help String | Args String deriving Show

cmdArgs :: Mode Args
cmdArgs = (modeEmpty $ Args "-"){ modeNames = ["penn-to-sentence-corpus"]
                                , modeHelp = "Reads of the yield of trees in a PennTreeCorpus."
                                , modeArgs = ([flagArg (\ s _ -> Right $ Args s) "FILE"], Nothing)
                                , modeGroupFlags = toGroup [flagNone ["help"] (\ _ -> Help $ defaultHelp cmdArgs) "Prints the help message."]
                                }

mainArgs :: Args -> IO ()
mainArgs (Help cs) = putStr cs
mainArgs (Args filename)
  = do cTrees <- if filename == "-" then TIO.getContents else TIO.readFile filename
       TIO.putStr . yield $ (parsePenn cTrees :: [Tree String])
