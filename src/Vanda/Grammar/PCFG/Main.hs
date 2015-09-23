{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

module Vanda.Grammar.PCFG.Main
( main
, mainArgs
, cmdArgs
, Args()
) where

import           System.Console.CmdArgs.Explicit
import           System.Console.CmdArgs.Explicit.Misc


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
    , binaryInput :: Bool
    , binaryOutput :: Bool
    }
  | Bests
    { number :: Int
    , inputGrammar :: FilePath
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
  = modes "pcfg" (Help $ defaultHelp cmdArgs) "algorithms for extracting and training PCFGs"
  [ (modeEmpty $ Extract undefined undefined False)
    { modeNames = ["extract"]
    , modeHelp = "Extracts a PCFG from a given Treebank."
    , modeArgs = ( [ flagArgOutputGrammar{argRequire = True}, flagArgTreeBank{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup [ flagOutBinary ]
    }
  , (modeEmpty $ Train undefined undefined undefined False False)
    { modeNames = ["train"]
    , modeHelp = "Trains a given PCFG with a Terminal String Corpus."
    , modeArgs = ( [ flagArgInputGrammar{argRequire = True}, flagArgStringCorpus{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup [ flagOutBinary, flagInBinary ]
    }
  , (modeEmpty $ Bests undefined undefined False)
    { modeNames = ["bests"]
    , modeHelp = "Extracts the N best derivations from a PCFG."
    , modeArgs = ( [ flagArgInputGrammar{argRequire = True}, flagArgN{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup [ flagInBinary ]
    }
  , (modeEmpty $ Intersect undefined undefined undefined False False)
    { modeNames = ["intersect"]
    , modeHelp = "Intersects a Terminal String and a PCFG."
    , modeArgs = ( [ flagArgInputGrammar{argRequire = True}, flagArgOutputGrammar{argRequire = True}, flagArgString{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup [ flagOutBinary, flagInBinary ]
    }
  , (modeEmpty $ Convert undefined undefined False False)
    { modeNames = ["convert"]
    , modeHelp = "Converts a pcfg file from binary format to text format or vice-versa."
    , modeArgs = ( [ flagArgInputGrammar{argRequire = True}, flagArgOutputGrammar{argRequire = True} ], Nothing )
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
      = flagArg (\ a x -> Right x{string = a}) "<Terminal String>"
    flagArgStringCorpus
      = flagArg (\ a x -> Right x{stringCorpus = a}) "<String Corpus>"
    flagInBinary
      = flagNone ["bin"]
                 (\ x -> x{binaryInput = True})
                 "read the input Grammar from a binary format"
    flagOutBinary
      = flagNone ["bout"]
                 (\ x -> x{binaryOutput = True})
                 "write the output Grammar to a binary format"


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()

mainArgs (Help cs) = putStr cs

mainArgs (Extract treebank outgrammar bout)
  = undefined
  
mainArgs (Train ingrammar stringcorpus outgrammar bin bout)
  = undefined
  
mainArgs (Bests n ingrammar bin)
  = undefined
  
mainArgs (Intersect ingrammar string outgrammar bin bout)
  = undefined
  
mainArgs (Convert ingrammar outgrammar bin bout)
  = undefined

 
