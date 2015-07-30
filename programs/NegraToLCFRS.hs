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

import           Codec.Compression.GZip (compress)
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy.IO as TIO
import           System.Environment (getArgs, getProgName)
import           System.IO (hPutStrLn, stderr)

import Vanda.Corpus.Negra.Text (parseNegra)
import Vanda.Grammar.XRS.LCFRS (niceStatictics)
import Vanda.Grammar.XRS.LCFRS.Extraction (extractPLCFRSFromNegra)


main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [outfile] -> do
      corpus <- TIO.getContents
      let plcfrs = extractPLCFRSFromNegra $ parseNegra corpus
      BS.writeFile outfile . compress $ B.encode plcfrs
      hPutStrLn stderr $ "Extracted PLCFRS:" ++ niceStatictics plcfrs
    _ -> putStrLn $ "Usage: " ++ progName ++ " <outfile>"
                    ++ "\n  a NeGra corpus file is read from stdin"
                    ++ ", outfile will be a gzipped PLCFRS file"
