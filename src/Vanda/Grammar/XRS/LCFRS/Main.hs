{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Grammar.XRS.LCFRS.Main
-- Copyright   :  (c) Technische Universität Dresden 2014
--                Mainly developed by Sebastian Mielke during his Bachelors
--                thesis.
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Tobias.Denkinger@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Grammar.XRS.LCFRS.Main
( main
, mainArgs
, cmdArgs
, Args()
) where


import           Codec.Compression.GZip (compress, decompress)

import qualified Data.Binary as B
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy.IO as TIO

import           Vanda.Corpus.Negra.Text (parseNegra)
import           Vanda.Grammar.XRS.LCFRS (showPLCFRS, niceStatictics, PLCFRS)
import           Vanda.Grammar.XRS.LCFRS.Binarize (binarizeNaively, binarizeByAdjacency, binarizeHybrid, binarizeUsing)
import           Vanda.Grammar.XRS.LCFRS.Extraction (extractPLCFRSFromNegra)

import           System.Console.CmdArgs.Explicit
import           System.Console.CmdArgs.Explicit.Misc


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
  deriving Show

data BinarizationStrategy = Naive | Optimal | Hybrid Int deriving (Eq, Show)


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


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()
mainArgs (Help cs) = putStr cs
mainArgs (Extract outfile)
  = do
      corpus <- TIO.getContents
      let plcfrs = extractPLCFRSFromNegra $ parseNegra corpus
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
--       when (subset == ["-plussmall"]) $ do -- partial bounded binarization
--           putStrLn $ "The following small subset binarizations are computed, "
--                      ++ "but not stored anywhere (they are useless).\n\n"
--           let (_, rules, (a_nt, _)) = plcfrs
--               printableFromRules rs = ([0], rs, (A.array (0,0) [], undefined))
--               pred = (<7) . getRk
--           putStrLn $ "Small PLCFRS:"
--                      ++ (niceStatictics . printableFromRules)
--                         (filter pred rules)
--           putStrLn $ "Naively binarized small PLCFRS:"
--                      ++ (niceStatictics . printableFromRules)
--                         (binarizeRuleSubset binarizeNaively pred a_nt rules)
--           putStrLn $ "Boundedly binarized small PLCFRS:"
--                      ++ (niceStatictics . printableFromRules)
--                         (binarizeRuleSubset binarizeByAdjacency pred a_nt rules)
