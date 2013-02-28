{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Environment ( getArgs )
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import qualified Vanda.Grammar.XRS.Functions as IF
import qualified Vanda.Grammar.XRS.IRTG as I
import qualified Vanda.Grammar.NGrams.KenLM as LM
import qualified Vanda.Algorithms.IntersectWithNGram as IS
import qualified Vanda.Hypergraph.IntHypergraph as HI

main
  :: IO ()
main = do
  args <- getArgs
  case args of
    ["-f", fMapFile, "-z", zhgFile, "-l", lmFile] -> do
      irtg1 <- IF.loadIRTG (zhgFile ++ ".bhg.gz")
      ws    <- IF.loadWeights (zhgFile ++ ".weights.gz")
      fm    <- IF.loadTokenArray fMapFile
      lm    <- LM.loadNGrams lmFile
      let irtg  = I.XRS irtg1 (VU.generate (V.length ws) (ws V.!))
      let irtg' = IS.intersect lm . IS.relabel lm fm $ irtg
      TIO.putStr . T.pack . show $ irtg
      TIO.putStr . T.pack $ "\n"
      TIO.putStr . T.pack . show $ irtg'
      TIO.putStr . T.pack $ "\n"