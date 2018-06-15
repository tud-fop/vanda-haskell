{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  VandaCLI.LCFRS
-- Copyright   :  (c) Technische Universität Dresden 2014
--                Developed by Sebastian Mielke during his Bachelors thesis.
-- License     :  BSD-style
--
-- Maintainer  :  Tobias.Denkinger@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module VandaCLI.LCFRS
( main
, mainArgs
, cmdArgs
, Args()
) where


import           Codec.Compression.GZip (compress, decompress)

import qualified Data.Binary as B
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import           Vanda.Corpus.Negra.Text (parseNegra)
import           Vanda.Grammar.XRS.LCFRS (showPLCFRS, niceStatictics, PLCFRS)
import           Vanda.Grammar.XRS.LCFRS.Binarize (binarizeNaively, binarizeByAdjacency, binarizeHybrid, binarizeUsing)
import           Vanda.Grammar.XRS.LCFRS.Extraction (extractPLCFRSFromNegra)

import           System.Console.CmdArgs.Explicit
import           System.Console.CmdArgs.Explicit.Misc

import           Vanda.Grammar.PMCFG.Functions (fromPLCFRS)
import           Vanda.Grammar.PMCFG (WPMCFG(..), integerize, deintegerize, pos, posTab, prepare)
import qualified Vanda.Grammar.XRS.LCFRS.CYKParser    as CYK
import qualified Vanda.Grammar.XRS.LCFRS.NaiveParser  as Naive
import qualified Vanda.Grammar.XRS.LCFRS.ActiveParser as Active
import qualified Vanda.Grammar.XRS.LCFRS.IncrementalParser as Incremental
import           Control.Arrow (second)
import           Control.Exception.Base (evaluate)
import           Data.Interner
import           Data.Maybe (catMaybes)
import           Data.Tree (drawTree)
import           Data.Weight (probabilistic, cost)

import           System.TimeIt
import           System.Timeout
import           Numeric (showFFloat)


data Args
  = Help String
  | Extract
    { argOutput :: FilePath
    }
  | Binarize
    { flagStrategy :: BinarizationStrategy
    , argInput :: FilePath
    , argOutput :: FilePath
    }
  | Parse
    { flagAlgorithm :: ParsingAlgorithm
    , argGrammar :: FilePath
    , unweighted :: Bool
    , flagOutput :: ParsingOutput
    , beamwidth :: Int
    , maxAmount :: Int
    , iTimeout :: Int
    }
    deriving Show

data BinarizationStrategy = Naive | Optimal | Hybrid Int deriving (Eq, Show)

data ParsingAlgorithm = CYK | NaiveActive | Active | Incremental deriving (Eq, Show, Read)
data ParsingOutput = POS | Derivation deriving (Eq, Show, Read)


cmdArgs :: Mode Args
cmdArgs
  = modes "lcfrs" (Help $ defaultHelp cmdArgs) "algorithms for probabilistic LCFRS"
  [ (modeEmpty $ Extract undefined)
    { modeNames = ["extract"]
    , modeHelp = "Reads of a PLCFRS from a NeGra corpus."
    , modeArgs = ( [ flagArgCorpus{argRequire = True} ], Nothing )
    }
  , (modeEmpty $ Binarize (Hybrid 5) undefined undefined)
    { modeNames = ["binarize", "binarise"]
    , modeHelp = "Binarizes a given PLCFRS."
    , modeArgs = ( [ flagArgInput{argRequire = True}, flagArgOutput{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup [flagNoneNaive, flagNoneOptimal, flagReqHybrid]
    }
  , (modeEmpty $ Parse Active undefined False Derivation 1000 1 (-1))
    { modeNames = ["parse"]
    , modeHelp = "Parses, given a (w)PMCFG, each in a sequence of sentences."
    , modeArgs = ( [ flagArgGrammar{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup  [ flagAlgorithmOption
                                , flagDisplayOption
                                , flagUseWeights
                                , flagBeamwidth
                                , flagMax
                                , flagTimeout
                                ]
    }
  ]
  where
    flagArgCorpus
      = flagArg (\ a x -> Right x{argOutput = a}) "OUTPUT"
    flagArgInput
      = flagArg (\ a x -> Right x{argInput = a}) "INPUT"
    flagArgOutput
      = flagArg (\ a x -> Right x{argOutput = a}) "OUTPUT"
    flagNoneNaive
      = flagNone ["n", "naive"] (\ x -> x{flagStrategy = Naive}) "use naive binarization"
    flagNoneOptimal
      = flagNone ["o", "optimal"] (\ x -> x{flagStrategy = Optimal}) "use optimal binarization (i.e., minimize the maximal fanout of the resulting PLCFRS)"
    flagReqHybrid
      = flagReq ["h", "hybrid"] (\ a x -> Right x{flagStrategy = Hybrid $ read a})
          "BOUND"
          "binarize rules up to rank BOUND optimally and the rest naively"
    -- parsing options
    flagArgGrammar
      = flagArg (\ a x -> Right x{argGrammar = a}) "GRAMMAR"
    flagAlgorithmOption
      = flagReq ["algorithm", "a"] (\ a x -> Right x{flagAlgorithm = read a}) "CYK/NaiveActive/Active/Incremental" "solution algorithm, default is 'Active'"
    flagDisplayOption
      = flagReq ["print"] (\ a x -> Right x{flagOutput = read a}) "POS/Derivation" "display solutions POS tags or full derivation (default)"
    flagBeamwidth
      = flagReq ["beam-width", "bw"] (\ a x -> Right x{beamwidth = read a}) "number" "beam width: limits the number of items held in memory"
    flagMax
      = flagReq ["results", "ts"] (\ a x -> Right x{maxAmount = read a}) "number" "limits the maximum amount of output parse trees"
    flagUseWeights
      = flagBool ["u", "unweighted"] (\ b x -> x{unweighted = b}) "use an unweighted parsing algorithm"
    flagTimeout
      = flagReq ["timeout", "t"] (\ a x -> Right x{iTimeout = read a}) "number" "limits the maximum parsing time in seconds"


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()
mainArgs (Help cs) = putStr cs
mainArgs (Extract outfile)
  = do
      corpus <- TIO.getContents
      let plcfrs = extractPLCFRSFromNegra False $ parseNegra corpus
      BS.writeFile outfile . compress $ B.encode plcfrs
      writeFile (outfile ++ ".readable") $ showPLCFRS plcfrs
--       hPutStrLn stderr $ "Extracted PLCFRS:" ++ niceStatictics plcfrs
mainArgs (Binarize strategy infile outfile)
  = do
      plcfrs <- fmap (B.decode . decompress . BS.fromChunks . (:[]))
              $ SBS.readFile infile :: IO PLCFRS
      let binarizer = case strategy of
                        Naive -> binarizeNaively
                        Optimal -> binarizeByAdjacency
                        Hybrid b -> binarizeHybrid b
          newPlcfrs = binarizeUsing binarizer plcfrs
      BS.writeFile outfile . compress $ B.encode newPlcfrs
      writeFile (outfile ++ ".readable") $ showPLCFRS newPlcfrs
      putStrLn $ show strategy ++ " binarization yielded:" ++ niceStatictics newPlcfrs
mainArgs (Parse algorithm grFile uw display bw trees itime)
  = do
      wpmcfg <- fromPLCFRS . B.decode . decompress <$> BS.readFile grFile :: IO (WPMCFG String Double String)
      let (WPMCFG inits wrs, nti, ti) = integerize wpmcfg
      _ <- evaluate wrs
      corpus <- TIO.getContents
      
      let pok _ [] = "Could not find any derivation.\n"
          pok showfunc xs = showfunc xs
          show' = case display of
                        POS -> let prefix splitchar = T.unpack . head . T.split (== splitchar) . T.pack
                                   showtabline (h, vs) = h  ++ foldl (\ s  v -> s ++ "\t" ++ prefix '_' v) "" vs
                              in unlines . fmap showtabline . posTab . catMaybes . fmap pos
                        Derivation -> concatMap (drawTree . fmap show)
      
      flip mapM_ (T.lines corpus)
        $ \ sentence -> do let intSent = (snd . internListPreserveOrder ti . map T.unpack . T.words) $ sentence
                           (filtertime, parse) <- if uw
                                                    then do let urs = WPMCFG inits $ map (second $ const $ cost (1 :: Int)) wrs
                                                            (filtertime', urs') <- timeItT (return $! prepare urs intSent)
                                                            return (filtertime', case algorithm of CYK -> CYK.parse' urs'
                                                                                                   NaiveActive -> Naive.parse' urs'
                                                                                                   Active -> Active.parse' urs'
                                                                                                   Incremental -> Incremental.parse' urs')
                                                    else do let wrs' = WPMCFG inits $ map (second probabilistic) wrs
                                                            (filtertime', wrs'') <- timeItT (return $! prepare wrs' intSent)
                                                            return (filtertime', case algorithm of CYK -> CYK.parse' wrs''
                                                                                                   NaiveActive -> Naive.parse' wrs''
                                                                                                   Active -> Active.parse' wrs''
                                                                                                   Incremental -> Incremental.parse' wrs'')
                           (parsetime, mParseTrees) <- timeItT $ timeout (itime*1000000) (return $! parse bw trees intSent)
                           let parseTrees = case mParseTrees of
                                                 Nothing -> []
                                                 Just ts -> ts
                           putStrLn $ showFFloat Nothing filtertime ""
                           putStrLn $ showFFloat Nothing parsetime ""
                           (putStrLn . pok show' . map (deintegerize (nti, ti))) $ parseTrees
