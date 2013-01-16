module Main where

import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import System.Environment ( getArgs )

import Vanda.Grammar.NGrams.Functions

main
  :: IO ()
main = do
  args <- getArgs
  case args of
    ["-g", grammar, "-n", n, text] -> do
      nGrams <- loadNGrams grammar
      input  <- TIO.readFile text
      let i = read n :: Int
          wts   = L.map (evaluateLine nGrams i) . T.lines $ input
      TIO.putStr . T.unlines . map (T.pack . show) $ wts
