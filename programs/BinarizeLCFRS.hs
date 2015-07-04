module Main where

import           Codec.Compression.GZip (compress, decompress)
import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as BS
import           System.Environment (getArgs, getProgName)

import Vanda.Grammar.XRS.LCFRS (Rule, getRk, niceStatictics)
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
      
      -- {- naive binarization
      let newPlcfrs = binarizeUsing binarizeNaively plcfrs
      BS.writeFile (outfile ++ ".bin.lcfrs.gz")
                   (compress $ B.encode newPlcfrs)
      putStrLn $ "Binarized PLCFRS:" ++ niceStatictics newPlcfrs
      -- -}
      
      putStrLn $ "The following small subset binarizations are computed, "
                 ++ "but not stored anywhere (they are useless).\n\n"
      
      -- {- partial bounded binarization
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
      -- -}
    _ -> print $ "Usage: " ++ progName ++ " -i infile -o outfile"
