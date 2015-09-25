{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

module Vanda.Grammar.PCFG.Main
( main
, mainArgs
, cmdArgs
, Args()
) where

import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Explicit.Misc
import Vanda.Grammar.PCFG.Functions
import Vanda.Grammar.PCFG.Util
import Vanda.Grammar.PCFG.PCFG
import Vanda.Corpus.Penn.Text
import Vanda.Corpus.TreeTerm
import Vanda.Corpus.SExpression
import qualified Data.Text.Lazy as T
import Text.Printf


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
    , iterations :: Int
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
  = modes "pcfg" (Help $ defaultHelp cmdArgs) "algorithms for extracting and training PCFGs"
  [ (modeEmpty $ Extract undefined undefined False)
    { modeNames = ["extract"]
    , modeHelp = "Extracts a PCFG from a given Treebank."
    , modeArgs = ( [ flagArgOutputGrammar{argRequire = True}, flagArgTreeBank{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup [ flagOutBinary ]
    }
  , (modeEmpty $ Train undefined undefined undefined undefined False False)
    { modeNames = ["train"]
    , modeHelp = "Trains a given PCFG with a Terminal String Corpus."
    , modeArgs = ( [ flagArgInputGrammar{argRequire = True},flagArgOutputGrammar{argRequire = True}, flagArgStringCorpus{argRequire = True}, flagArgIterations{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup [ flagOutBinary, flagInBinary ]
    }
  , (modeEmpty $ Bests undefined undefined False False False)
    { modeNames = ["bests"]
    , modeHelp = "Extracts the N best derivations from a PCFG."
    , modeArgs = ( [ flagArgInputGrammar{argRequire = True}, flagArgN{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup [ flagInBinary, flagYield, flagProbs ]
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
    flagArgIterations
      = flagArg (\ a x -> Right x{iterations = read a}) "<Number of Iterations>"
    flagArgN
      = flagArg (\ a x -> Right x{number = read a}) "<N>"
    flagArgString
      = flagArg (\ a x -> Right x{string = a}) "<Terminal String>"
    flagArgStringCorpus
      = flagArg (\ a x -> Right x{stringCorpus = a}) "<String Corpus>"
    flagInBinary
      = flagNone ["bin"]
                 (\ x -> x{binaryInput = True})
                 "Read the input Grammar from a binary format."
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
                 "Write the output Grammar to a binary format."


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()

mainArgs (Help cs) = putStr cs

mainArgs (Extract treebank outgrammar bout)
  = do
  e <- parseFromFile pSExpressions treebank 
  writeGrammar outgrammar . extractPCFG $ map (treeToDeriv . toTree) e
  
mainArgs (Train ingrammar stringcorpus outgrammar nit bin bout)
  = do
    g <- readGrammar ingrammar
    s <- readFile stringcorpus
    writeGrammar outgrammar $ train g (map words $ lines s) nit
  
mainArgs (Bests n ingrammar yld probs bin)
  = do
    g <- readGrammar ingrammar
    putStr $ toStr (bestDerivations g n) yld probs
    where toStr :: [(Deriv String String,Double)] -> Bool -> Bool -> String
          toStr [] _ _ = ""
          toStr ((deriv,weight):rest) yld probs = 
            (if probs then printf "%.3f   " weight
                      else "")
            ++ (if yld then (T.unpack $ yield [derivToTree deriv])
                       else (T.unpack $ unparsePenn [derivToTree deriv]))
            ++ toStr rest yld probs
                        
mainArgs (Intersect ingrammar string outgrammar bin bout)
  = do
    g <- readGrammar ingrammar
    writeGrammar outgrammar (intersect g $ words string)
  
mainArgs (Convert ingrammar outgrammar bin bout)
  = undefined

 
