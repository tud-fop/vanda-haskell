{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

-- (c) 2016 Tobias Denkinger <Tobias.Denkinger@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

module Vanda.Grammar.PMCFG.Main
  ( main
  , mainArgs
  , cmdArgs
  , Args ()
  ) where

import Codec.Compression.GZip (compress, decompress)
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Explicit.Misc
import Vanda.Corpus.Negra.Text (parseNegra)
import Vanda.Grammar.PMCFG (prettyPrintWPMCFG)
import Vanda.Grammar.PMCFG.Functions (extractFromNegra)


data Args
  = Help String
  | Extract
    { argOutput :: FilePath
    }
  deriving Show


cmdArgs :: Mode Args
cmdArgs
  = modes "pmcfg" (Help $ defaultHelp cmdArgs) "algorithms for weighted parallel multiple context-free grammars"
  [ (modeEmpty $ Extract undefined)
    { modeNames = ["extract"]
    , modeHelp = "Reads of a wPMCFG from a NeGra corpus."
    , modeArgs = ( [ flagArgCorpus{argRequire = True} ], Nothing )
    }
  ]
  where
    flagArgCorpus
      = flagArg (\ a x -> Right x{argOutput = a}) "OUTPUT"


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()
mainArgs (Help cs) = putStr cs
mainArgs (Extract outfile)
  = do
      corpus <- TIO.getContents
      let pmcfg = extractFromNegra $ parseNegra corpus
      BS.writeFile outfile . compress $ B.encode pmcfg
      writeFile (outfile ++ ".readable") $ prettyPrintWPMCFG pmcfg
