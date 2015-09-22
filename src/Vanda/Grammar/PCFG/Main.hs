{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

module Vanda.Grammar.PCFG.Main
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

import           Vanda.Algorithms.IntEarley
import qualified Vanda.Algorithms.Earley.WSA as WSA
import qualified Vanda.Algorithms.IntersectWithNGram as IS
import qualified Vanda.Algorithms.IntersectWithNGramUtil as ISU
import qualified Vanda.Grammar.NGrams.Functions as LM
import qualified Vanda.Grammar.XRS.Functions as IF
import qualified Vanda.Grammar.XRS.IRTG as I
import qualified Vanda.Hypergraph.IntHypergraph as HI
import qualified Vanda.Token as TK

import           System.Console.CmdArgs.Explicit
import           System.Console.CmdArgs.Explicit.Misc


data Args
  = Help String
  | Extract
    { treeBank :: FilePath
    , binary :: Bool
    }
  | Train
    { grammar :: FilePath
    , stringCorpus :: FilePath
    , binary :: Bool
    }
  | Bests
    { number :: Int
    , grammar :: FilePath
    , binary :: Bool
    }
  | Intersect 
    { grammar :: FilePath
    , string :: String
    , binary :: Bool
    }
  deriving Show

cmdArgs :: Mode Args
cmdArgs
  = modes "pcfg" (Help $ defaultHelp cmdArgs) "algorithms for extracting and training PCFGs"
  [ (modeEmpty $ Extract undefined False)
    { modeNames = ["extract"]
    , modeHelp = "Extracts a PCFG from a given Treebank."
    , modeArgs = ( [ flagArgTreeBank{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup [ flagBinary ]
    }
  , (modeEmpty $ Train undefined undefined False)
    { modeNames = ["train"]
    , modeHelp = "Trains a given PCFG with a Terminal String Corpus."
    , modeArgs = ( [ flagArgGrammar{argRequire = True}, flagArgStringCorpus{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup [ flagBinary ]
    }
  , (modeEmpty $ Bests undefined undefined False)
    { modeNames = ["bests"]
    , modeHelp = "Extracts the N best derivations from a PCFG."
    , modeArgs = ( [ flagArgGrammar{argRequire = True}, flagArgN{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup [ flagBinary ]
    }
  , (modeEmpty $ Intersect undefined undefined False)
    { modeNames = ["intersect"]
    , modeHelp = "Intersects a Terminal String and a PCFG."
    , modeArgs = ( [ flagArgGrammar{argRequire = True}, flagArgString{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup [ flagBinary ]
    }
  ]
  where
    flagArgTreeBank
      = flagArg (\ a x -> Right x{treeBank = a}) "<Treebank>"
    flagArgGrammar
      = flagArg (\ a x -> Right x{grammar = a}) "<Grammar>"
    flagArgN
      = flagArg (\ a x -> Right x{number = a}) "<N>"
    flagArgString
      = flagArg (\ a x -> Right x{string = a}) "<Terminal String>"
    flagNoneBHPS
      = flagNone ["b"]
                 (\ x -> x{binary = True})
                 "read and write Grammars in a binary format"


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()

mainArgs (Help cs) = putStr cs

mainArgs (Extract tb binary)
  = undefined
mainArgs (Train g sc binary)
  = undefined
mainArgs (Bests n g binary)
  = undefined
mainArgs (Intersect g s binary)
  = undefined

 
