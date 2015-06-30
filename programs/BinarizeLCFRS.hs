module Main where

import           Codec.Compression.GZip (compress, decompress)
import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as BS
import           System.Environment (getArgs, getProgName)

import Vanda.Grammar.XRS.LCFRS (Rule, niceStatictics)
import Vanda.Grammar.XRS.LCFRS.Binarize


-- | Binarizing a PLCFRS
main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    ["-i", infile, "-o", outfile] -> do
      plcfrs <- fmap (B.decode . decompress . BS.fromChunks . (:[]))
              $ SBS.readFile (infile ++ ".lcfrs.gz")
        :: IO ([Int], [(Rule, Double)], (A.Array Int String, A.Array Int String))
      let newPlcfrs = binarizeUsing binarizeNaively plcfrs
      BS.writeFile (outfile ++ ".bin.lcfrs.gz")
                   (compress $ B.encode newPlcfrs)
      putStrLn $ "Binarized PLCFRS:" ++ niceStatictics newPlcfrs
    _ -> print $ "Usage: " ++ progName ++ " -i infile -o outfile"
