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

import Vanda.Corpus.Negra.Text
import Vanda.Grammar.XRS.LCFRS (niceStatictics)
import Vanda.Grammar.XRS.LCFRS.Extraction


-- | Extracting a PLCFRS
main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    ["-n", negrafile, "-o", outfile] -> do
      plcfrs <- fmap (extractPLCFRSFromNegra . parseNegra)
              $ TIO.readFile negrafile
      BS.writeFile outfile (compress $ B.encode plcfrs)
      putStrLn $ "Extracted PLCFRS:" ++ niceStatictics plcfrs
    _ -> putStrLn $ "Usage: " ++ progName ++ " -n negrafile -o outfile"
                    ++ "\nnegrafile is an export format corpus file"
                    ++ ", outfile will be a gzipped plcfrs file"
