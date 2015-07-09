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
      BS.writeFile (outfile ++ ".lcfrs.gz") (compress $ B.encode plcfrs)
      putStrLn $ "Extracted PLCFRS:" ++ niceStatictics plcfrs
    _ -> print $ "Usage: " ++ progName ++ " -n negrafile -o outfile"
