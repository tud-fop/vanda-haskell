module Main where

import Vanda.Grammar.XRS.Functions
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-e", eMapFile, "-f", fMapFile, "-z", zhgFile] -> do
      irtg <- loadIRTG (zhgFile ++ ".bhg.gz")
      ws <- loadWeights (zhgFile ++ ".weights.gz")
      em <- loadTokenArray eMapFile
      fm <- loadTokenMap fMapFile
      let translate input
            = let wsa = toWSAmap fm input
                  inprod = wsa `inputProduct` irtg
                  best = bestDeriv inprod ws
              in toString em (getOutputTree inprod best)
      interact (unlines . map translate . lines)

