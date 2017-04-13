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
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as T
import Data.Tree (drawTree)
import Data.Interner
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Explicit.Misc
import Control.Exception.Base (evaluate)

import Vanda.Corpus.Negra.Text (parseNegra)
import Vanda.Grammar.PMCFG.Functions (extractFromNegra, extractFromNegraAndBinarize)
import Vanda.Grammar.XRS.LCFRS.Binarize (binarizeNaively, binarizeByAdjacency, binarizeHybrid)

import Vanda.Grammar.PMCFG (WPMCFG (..), prettyPrintWPMCFG, integerize, deintegerize)
--import qualified Vanda.Grammar.PMCFG.Parse as UnweightedAutomaton
import qualified Vanda.Grammar.PMCFG.CYKParser as CYK
import qualified Vanda.Grammar.PMCFG.NaiveParser as Naive
import qualified Vanda.Grammar.PMCFG.ActiveParser as Active
import Data.Weight (probabilistic, cost)
import Control.Arrow


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
    , unweighted :: Bool
    , beamwidth :: Int
    , maxAmount :: Int
    }
  deriving Show

data BinarizationStrategy = Naive | Optimal | Hybrid Int deriving (Eq, Show)
data ParsingAlgorithm = UnweightedAutomaton | CYK | NaiveActive | Active deriving (Eq, Show)

cmdArgs :: Mode Args
cmdArgs
  = modes "pmcfg" (Help $ defaultHelp cmdArgs) "algorithms for weighted parallel multiple context-free grammars"
  [ (modeEmpty $ Extract undefined False undefined)
    { modeNames = ["extract"]
    , modeHelp = "Reads of a wPMCFG from a NeGra corpus."
    , modeArgs = ( [ flagArgCorpus{argRequire = True}], Nothing )
    , modeGroupFlags = toGroup [flagNoneBinarize,  flagNoneNaive, flagNoneOptimal, flagReqHybrid]
    }
  , (modeEmpty $ Parse undefined undefined False 1000 10)
    { modeNames = ["parse"]
    , modeHelp = "Parses, given a (w)PMCFG, each in a sequence of sentences."
    , modeArgs = ( [ flagArgGrammar{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup  [ flagUnweightedAutomaton
                                , flagCYK
                                , flagNaive
                                , flagActive
                                , flagUseWeights
                                , flagBeamwidth
                                , flagMax
                                ]
    }
  ]
  where
    flagArgCorpus
      = flagArg (\ a x -> Right x{argOutput = a}) "OUTPUT"
    flagArgGrammar
      = flagArg (\ a x -> Right x{argGrammar = a}) "GRAMMAR"
    flagBeamwidth
      = flagReq ["blimit", "bw"] (\ a x -> Right x{beamwidth = read a}) "Int" "beam width: limits the number of items held in memory"
    flagMax
      = flagReq ["plimit", "ts"] (\ a x -> Right x{maxAmount = read a}) "Int" "limits the maximum amount of output parse trees"
    flagUseWeights
      = flagBool ["u", "unweighted"] (\ b x -> x{unweighted = b}) "use an unweighted parsing algorithm"
    flagUnweightedAutomaton
      = flagNone ["a", "automaton"] (\ x -> x{flagAlgorithm = UnweightedAutomaton}) "use an unweighted automaton constructed from the grammar"
    flagCYK
      = flagNone ["c", "cyk"] (\ x -> x{flagAlgorithm = CYK}) "use a basic cyk-like deduction system constructed from the grammar"
    flagNaive
      = flagNone ["naive-active"] (\ x -> x{flagAlgorithm = NaiveActive}) "use a binarized deduction system constructed from the grammar"
    flagActive
      = flagNone ["active"] (\ x -> x{flagAlgorithm = Active}) "use a binarized active deduction system constructed from the grammar"
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
mainArgs (Parse algorithm grFile uw bw trees)
  = do
      wpmcfg <- B.decode . decompress <$> BS.readFile grFile :: IO (WPMCFG String Double String)
      let (WPMCFG inits wrs, nti, ti) = integerize wpmcfg
      _ <- evaluate wrs        
      let parse = if uw
                  then let urs = WPMCFG inits $ map (second $ const $ cost (1 :: Int)) wrs in
                           case algorithm of CYK -> CYK.parse urs
                                             NaiveActive -> Naive.parse urs
                                             Active -> Active.parse urs
                                             UnweightedAutomaton -> error "not implemented"
                                            --UnweightedAutomaton -> UnweightedAutomaton.parse urs
                  else let wrs' = WPMCFG inits $ map (second probabilistic) wrs in
                           case algorithm of CYK -> CYK.parse wrs'
                                             NaiveActive -> Naive.parse wrs'
                                             Active -> Active.parse wrs'
                                             UnweightedAutomaton -> error "not implemented"
      corpus <- TIO.getContents
      mapM_ (putStrLn . drawTree . fmap show . head . map (deintegerize (nti, ti)) . parse bw trees . snd . internListPreserveOrder ti . map T.unpack . T.words) $ T.lines corpus
 