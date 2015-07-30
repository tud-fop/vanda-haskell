-- (c) 2015 Sebastian Mielke <sebastian.mielke@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

module Main where

import           Codec.Compression.GZip (compress, decompress)
import           Control.Monad (when)
import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as BS
import           System.Environment (getArgs, getProgName)

import Vanda.Grammar.XRS.LCFRS (getRk, niceStatictics, PLCFRS)
import Vanda.Grammar.XRS.LCFRS.Binarize


-- | Binarizing a PLCFRS
main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    (strategy : "-i" : infile : "-o" : outfile : subset) -> do
      plcfrs <- fmap (B.decode . decompress . BS.fromChunks . (:[]))
              $ SBS.readFile infile :: IO PLCFRS
      -- full binarization
      let binarizer = case strategy of
                        "naive" -> binarizeNaively
                        "lowmaxfo" -> binarizeByAdjacency
                        _ -> error $ strategy ++ " is no valid strategy!"
          newPlcfrs = binarizeUsing binarizer plcfrs
      BS.writeFile outfile (compress $ B.encode newPlcfrs)
      putStrLn $ "Binarized PLCFRS (" ++ strategy ++ "):"
                 ++ niceStatictics newPlcfrs
      when (subset == ["-plussmall"]) $ do -- partial bounded binarization
          putStrLn $ "The following small subset binarizations are computed, "
                     ++ "but not stored anywhere (they are useless).\n\n"
          let (_, rules, (a_nt, _)) = plcfrs
              printableFromRules rs = ([0], rs, (A.array (0,0) [], undefined))
              pred = (<7) . getRk
          putStrLn $ "Small PLCFRS:"
                     ++ (niceStatictics . printableFromRules)
                        (filter pred rules)
          putStrLn $ "Naively binarized small PLCFRS:"
                     ++ (niceStatictics . printableFromRules)
                        (binarizeRuleSubset binarizeNaively pred a_nt rules)
          putStrLn $ "Boundedly binarized small PLCFRS:"
                     ++ (niceStatictics . printableFromRules)
                        (binarizeRuleSubset binarizeByAdjacency pred a_nt rules)
    _ -> putStrLn $ "Usage: " ++ progName ++ " (naive|optimal) -i infile"
                    ++ "-o outfile [-plussmall]"
                    ++ "\ninfile and outfile are an gzipped plcfrs files"
