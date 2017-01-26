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
import Control.Applicative ((<$>))
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
import qualified Vanda.Grammar.PMCFG.Parse as UnweightedAutomaton
import qualified Vanda.Grammar.PMCFG.CYKParser as CYK
import qualified Vanda.Grammar.PMCFG.NaiveParser as Naive
import Vanda.Grammar.PMCFG.WeightedDeductiveSolver (Probabilistic, probabilistic)
import Vanda.Grammar.XRS.LCFRS.Binarize (binarizeNaively, binarizeByAdjacency, binarizeHybrid)


data Args
  = Help String
  | Extract
    { argOutput :: FilePath
    , flagBinarize :: Bool
    , flagStrategy :: BinarizationStrategy
    }
  | Parse
    { flagAlgorithm :: ParsingAlgorithm
    , argGrammar :: FilePath
    }
  deriving Show

data BinarizationStrategy = Naive | Optimal | Hybrid Int deriving (Eq, Show)
data ParsingAlgorithm = UnweightedAutomaton | UnweightedCYK | CYK | UnweightedNaive | NaiveP deriving (Eq, Show)

cmdArgs :: Mode Args
cmdArgs
  = modes "pmcfg" (Help $ defaultHelp cmdArgs) "algorithms for weighted parallel multiple context-free grammars"
  [ (modeEmpty $ Extract undefined False undefined)
    { modeNames = ["extract"]
    , modeHelp = "Reads of a wPMCFG from a NeGra corpus."
    , modeArgs = ( [ flagArgCorpus{argRequire = True}], Nothing )
    , modeGroupFlags = toGroup [flagNoneBinarize,  flagNoneNaive, flagNoneOptimal, flagReqHybrid]
    }
  , (modeEmpty $ Parse undefined undefined)
    { modeNames = ["parse"]
    , modeHelp = "Parses, given a (w)PMCFG, each in a sequence of sentences."
    , modeArgs = ( [ flagArgGrammar{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup  [flagUnweightedAutomaton, flagCYK, flagUnweightedCYK, flagNaive, flagUnweightedNaive]
    }
  ]
  where
    flagArgCorpus
      = flagArg (\ a x -> Right x{argOutput = a}) "OUTPUT"
    flagArgGrammar
      = flagArg (\ a x -> Right x{argGrammar = a}) "GRAMMAR"
    flagUnweightedAutomaton
      = flagNone ["a", "automaton"] (\ x -> x{flagAlgorithm = UnweightedAutomaton}) "use an unweighted automaton constructed from the grammar"
    flagCYK
      = flagNone ["c", "wcyk"] (\ x -> x{flagAlgorithm = CYK}) "use a basic cyk-like deduction system constructed from the grammar"
    flagUnweightedCYK
      = flagNone ["cyk"] (\ x -> x{flagAlgorithm = UnweightedCYK}) "use an unweighted cyk-like deduction system constructed from the grammar"
    flagUnweightedNaive
      = flagNone ["naive"] (\ x -> x{flagAlgorithm = UnweightedNaive}) "use an unweighted binarized deduction system constructed from the grammar"
    flagNaive
      = flagNone ["n", "wnaive"] (\ x -> x{flagAlgorithm = NaiveP}) "use a binarized deduction system constructed from the grammar"
    flagNoneBinarize
      = flagNone ["b", "binarize", "binarise"] (\ x -> x{flagBinarize = True}) "binarize the extracted grammar"
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
mainArgs (Extract outfile False _)
  = do
      corpus <- TIO.getContents
      let pmcfg = extractFromNegra $ parseNegra corpus :: WPMCFG String Double String
      BS.writeFile outfile . compress $ B.encode pmcfg
      writeFile (outfile ++ ".readable") $ prettyPrintWPMCFG pmcfg
mainArgs (Extract outfile True strategy)
  = do
      corpus <- TIO.getContents
      let s = case strategy of Naive -> binarizeNaively
                               Optimal -> binarizeByAdjacency
                               Hybrid b -> binarizeHybrid b
      let pmcfg = extractFromNegraAndBinarize s $ parseNegra corpus :: WPMCFG String Double String
      BS.writeFile outfile . compress $ B.encode pmcfg
      writeFile (outfile ++ ".readable") $ prettyPrintWPMCFG pmcfg
mainArgs (Parse algorithm grFile)
  = do
      WPMCFG inits wrs <- B.decode . decompress
                          <$> BS.readFile grFile :: IO (WPMCFG String Double String)
      let parse = case algorithm of CYK -> CYK.weightedParse (WPMCFG inits $ map (\ (r, w) -> (r, probabilistic w)) wrs)
                                    NaiveP -> Naive.weightedParse (WPMCFG inits $ map (\ (r, w) -> (r, probabilistic w)) wrs)
                                    UnweightedCYK -> CYK.parse (PMCFG inits (map fst wrs))
                                    UnweightedNaive -> Naive.parse (PMCFG inits (map fst wrs))
                                    --UnweightedAutomaton -> UnweightedAutomaton.parse (PMCFG inits (map fst wrs))
      --_ <- evaluate pmcfg
      corpus <- TIO.getContents
      mapM_ (putStrLn . drawTree . fmap show . head . parse . map T.unpack . T.words) $ T.lines corpus
