{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

{-|
Module:      Vanda.Grammar.PCFG.PCFG
Description: program structure of the /PCFG/ part of Vanda
Copyright:   (c) Technische Universität Dresden 2015
License:     Redistribution and use in source and binary forms, with
             or without modification, is ONLY permitted for teaching
             purposes at Technische Universität Dresden AND IN
             COORDINATION with the Chair of Foundations of Programming.
Maintainer:  markus.napierkowski@mailbox.tu-dresden.de
Stability:   unknown

-}
module Vanda.Grammar.PCFG.Main
( main
, mainArgs
, cmdArgs
, Args()
) where

import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Explicit.Misc

import qualified Data.Text.Lazy as T
import Text.Printf
import Vanda.Corpus.Penn.Text
import Vanda.Corpus.SExpression
import Vanda.Grammar.PCFG.Functions
import Vanda.Grammar.PCFG.IO
import Vanda.Grammar.PCFG.PCFG


data Args
  = Help String
  | Extract
    { treeBank :: FilePath
    , outputGrammar :: FilePath
    , binaryOutput :: Bool
    }
  | Train
    { inputGrammar :: FilePath
    , stringCorpus :: FilePath
    , outputGrammar :: FilePath
    , number :: Int
    , binaryInput :: Bool
    , binaryOutput :: Bool
    }
  | Bests
    { number :: Int
    , inputGrammar :: FilePath
    , yld :: Bool
    , probs :: Bool
    , binaryInput :: Bool
    }
  | Intersect 
    { inputGrammar :: FilePath
    , string :: String
    , outputGrammar :: FilePath
    , binaryInput :: Bool
    , binaryOutput :: Bool
    }
  | Convert
    { inputGrammar :: FilePath
    , outputGrammar :: FilePath
    , binaryInput :: Bool
    , binaryOutput :: Bool
    }
  deriving Show

cmdArgs :: Mode Args
cmdArgs
  = modes "pcfg" (Help $ defaultHelp cmdArgs) 
    "algorithms for extracting and training PCFGs"
  [ (modeEmpty $ Extract undefined undefined False)
    { modeNames = ["extract"]
    , modeHelp = "Extracts a PCFG from <Treebank> and writes it to \
                 \<Output Grammar>."
    , modeArgs = ( [ flagArgOutputGrammar{argRequire = True}
                   , flagArgTreeBank{argRequire = True} 
                   ]
                 , Nothing 
                 )
    , modeGroupFlags = toGroup [ flagOutBinary ]
    }
  , (modeEmpty $ Train undefined undefined undefined undefined False False)
    { modeNames = ["train"]
    , modeHelp = "Reads a PCFG from <Input Grammar> and trains it with \
                 \<N> Iterations on <Corpus> and writes the resulting grammar\
                 \ to <Output Grammar>."
    , modeArgs = ( [ flagArgInputGrammar{argRequire = True}
                   , flagArgOutputGrammar{argRequire = True}
                   , flagArgStringCorpus{argRequire = True}
                   , flagArgN{argRequire = True} 
                   ]
                 , Nothing 
                 )
    , modeGroupFlags = toGroup [ flagOutBinary, flagInBinary ]
    }
  , (modeEmpty $ Bests undefined undefined False False False)
    { modeNames = ["bests"]
    , modeHelp = "Reads a PCFG from <Input Grammar>, extracts the <N> best\
                 \ derivations and writes it to stdout."
    , modeArgs = ( [ flagArgInputGrammar{argRequire = True}
                   , flagArgN{argRequire = True} 
                   ]
                 , Nothing
                 )
    , modeGroupFlags = toGroup [ flagInBinary, flagYield, flagProbs ]
    }
  , (modeEmpty $ Intersect undefined undefined undefined False False)
    { modeNames = ["intersect"]
    , modeHelp = "Reads a PCFG from <Input Grammar>, intersects it with \
                 \ <String> and writes the result to <Output Grammar>."
    , modeArgs = ( [ flagArgInputGrammar{argRequire = True}
                   , flagArgOutputGrammar{argRequire = True}
                   , flagArgString{argRequire = True} 
                   ]
                 , Nothing )
    , modeGroupFlags = toGroup [ flagOutBinary, flagInBinary ]
    }
  , (modeEmpty $ Convert undefined undefined False False)
    { modeNames = ["convert"]
    , modeHelp = "Converts a PCFG from <Input Grammar> from text format to \ 
                 \binary or vice-versa and writes the result to \
                 \<Output Grammar>"
    , modeArgs = ( [ flagArgInputGrammar{argRequire = True}
                   , flagArgOutputGrammar{argRequire = True} 
                   ]
                 , Nothing )
    , modeGroupFlags = toGroup [ flagOutBinary, flagInBinary ]
    }
  ]
  where
    flagArgTreeBank
      = flagArg (\ a x -> Right x{treeBank = a}) "<Treebank>"
    flagArgInputGrammar
      = flagArg (\ a x -> Right x{inputGrammar = a}) "<Input Grammar>"
    flagArgOutputGrammar
      = flagArg (\ a x -> Right x{outputGrammar = a}) "<Output Grammar>"
    flagArgN
      = flagArg (\ a x -> Right x{number = read a}) "<N>"
    flagArgString
      = flagArg (\ a x -> Right x{string = a}) "<String>"
    flagArgStringCorpus
      = flagArg (\ a x -> Right x{stringCorpus = a}) "<Corpus>"
    flagInBinary
      = flagNone ["bin"]
                 (\ x -> x{binaryInput = True})
                 "Read <Input Grammar> from a binary format."
    flagYield
      = flagNone ["y"]
                 (\ x -> x{yld = True})
                 "Only show the yield of the derivations."
    flagProbs
      = flagNone ["p"]
                 (\ x -> x{probs = True})
                 "Also display derivation probabilities."
    flagOutBinary
      = flagNone ["bout"]
                 (\ x -> x{binaryOutput = True})
                 "Write <Output Grammar> to a binary format."


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()

mainArgs (Help cs) = putStr cs

mainArgs (Extract treebank outgrammar bout)
  = do
  e <- parseFromFile pSExpressions treebank 
  writeGrammar bout outgrammar . extractPCFG $ map treeToDeriv $ concatMap toForest e
  
mainArgs (Train ingrammar stringcorpus outgrammar nit bin bout)
  = do
    g <- readGrammar bin ingrammar
    s <- readFile stringcorpus
    writeGrammar bout outgrammar $ train g (map words $ lines s) nit
  
mainArgs (Bests n ingrammar yld0 probs0 bin)
  = do
    g <- readGrammar bin ingrammar
    putStr $ toStr (bestDerivations g n) yld0 probs0
    where toStr :: [(Deriv String String,Double)] -> Bool -> Bool -> String
          toStr [] _ _ = ""
          toStr ((deriv,weight):rest) yld1 probs1 = 
            (if probs1 then printf "%.3f   " weight
                       else "")
            ++ T.unpack (if yld1 then yield [derivToTree deriv]
                                 else unparsePenn [derivToTree deriv])
            ++ toStr rest yld1 probs1
                        
mainArgs (Intersect ingrammar string0 outgrammar bin bout)
  = do
    g <- readGrammar bin ingrammar
    writeGrammar bout outgrammar (intersect g $ words string0)
  
mainArgs (Convert ingrammar outgrammar bin bout)
  = do
    g <- readGrammar bin ingrammar
    writeGrammar bout outgrammar g

 
