{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Tobias Denkinger 2016
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Grammar.PMCFG.Main
  ( main
  , mainArgs
  , cmdArgs
  , Args ()
  ) where

import Codec.Compression.GZip (compress, decompress)
import Control.Exception.Base (evaluate)
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as T
import Data.Tree (drawTree)
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Explicit.Misc
import Vanda.Corpus.Negra.Text (parseNegra)
import Vanda.Grammar.PMCFG (WPMCFG (..), PMCFG (..), prettyPrintWPMCFG)
import Vanda.Grammar.PMCFG.Functions (extractFromNegra, extractFromNegraAndBinarize)
import Vanda.Grammar.PMCFG.Parse (parse)
import Vanda.Grammar.XRS.LCFRS.Binarize (binarizeNaively, binarizeByAdjacency, binarizeHybrid)


data Args
  = Help String
  | Extract
    { argOutput :: FilePath
    , flagPreterminals :: Bool
    , flagBinarize :: Bool
    , flagStrategy :: BinarizationStrategy
    }
  | Parse
    { flagAlgorithm :: ParsingAlgorithm
    , argGrammar :: FilePath
    }
  deriving Show

data BinarizationStrategy = Naive | Optimal | Hybrid Int deriving (Eq, Show)
data ParsingAlgorithm = UnweightedAutomaton deriving (Eq, Show)


cmdArgs :: Mode Args
cmdArgs
  = modes "pmcfg" (Help $ defaultHelp cmdArgs) "algorithms for weighted parallel multiple context-free grammars"
  [ (modeEmpty $ Extract undefined False False undefined)
    { modeNames = ["extract"]
    , modeHelp = "Reads of a wPMCFG from a NeGra corpus."
    , modeArgs = ( [ flagArgCorpus{argRequire = True}], Nothing )
    , modeGroupFlags = toGroup [flagNoneBinarize,  flagNoneNaive, flagNoneOptimal, flagReqHybrid, flagNonePreterminals]
    }
  , (modeEmpty $ Parse undefined undefined)
    { modeNames = ["parse"]
    , modeHelp = "Parses, given a (w)PMCFG, each in a sequence of sentences."
    , modeArgs = ( [ flagArgGrammar{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup  [flagUnweightedAutomaton]
    }
  ]
  where
    flagArgCorpus
      = flagArg (\ a x -> Right x{argOutput = a}) "OUTPUT"
    flagArgGrammar
      = flagArg (\ a x -> Right x{argGrammar = a}) "GRAMMAR"
    flagUnweightedAutomaton
      = flagNone ["a", "automaton"] (\ x -> x{flagAlgorithm = UnweightedAutomaton}) "use an unweighted automaton constructed from the grammar"
    flagNoneBinarize
      = flagNone ["b", "binarize", "binarise"] (\ x -> x{flagBinarize = True}) "binarize the extracted grammar"
    flagNonePreterminals
      = flagNone ["p", "only-preterminals"] (\ x -> x{flagPreterminals = True}) "drop terminal symbols and use the preterminals instead"
    flagNoneNaive
      = flagNone ["n", "naive"] (\ x -> x{flagStrategy = Naive}) "use naive binarization"
    flagNoneOptimal
      = flagNone ["o", "optimal"] (\ x -> x{flagStrategy = Optimal}) "use optimal binarization (i.e., minimize the maximal fanout of the resulting PLCFRS)"
    flagReqHybrid
      = flagReq ["h", "hybrid"] (\ a x -> Right x{flagStrategy = Hybrid $ read a})
          "BOUND"
          "binarize rules up to rank BOUND optimally and the rest naively"


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()
mainArgs (Help cs) = putStr cs
mainArgs (Extract outfile usePreterminals False _)
  = do
      corpus <- TIO.getContents
      let pmcfg = extractFromNegra usePreterminals $ parseNegra corpus :: WPMCFG String Double String
      BS.writeFile outfile . compress $ B.encode pmcfg
      writeFile (outfile ++ ".readable") $ prettyPrintWPMCFG prettyShowString prettyShowString pmcfg
mainArgs (Extract outfile usePreterminals True strategy)
  = do
      corpus <- TIO.getContents
      let s = case strategy of Naive -> binarizeNaively
                               Optimal -> binarizeByAdjacency
                               Hybrid b -> binarizeHybrid b
      let pmcfg = extractFromNegraAndBinarize usePreterminals s $ parseNegra corpus :: WPMCFG String Double String
      BS.writeFile outfile . compress $ B.encode pmcfg
      writeFile (outfile ++ ".readable") $ prettyPrintWPMCFG prettyShowString prettyShowString pmcfg
mainArgs (Parse _ grFile)
  = do
      WPMCFG inits wrs <- B.decode . decompress
                          <$> BS.readFile grFile :: IO (WPMCFG String Double String)
      let pmcfg = PMCFG inits (map fst wrs)
      _ <- evaluate pmcfg
      corpus <- TIO.getContents
      mapM_ (putStrLn . drawTree . fmap show . head . parse pmcfg . map T.unpack . T.words) $ T.lines corpus

prettyShowString :: String -> String
prettyShowString s = '\"' : concatMap g s ++ "\"" where
  g '\"' = "\\\""
  g '\\' = "\\\\"
  g c    = [c]
