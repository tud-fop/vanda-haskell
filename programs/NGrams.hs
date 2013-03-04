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
    ["-g", grammar, "-l", text] -> do
      nGrams <- loadNGrams grammar
      input  <- TIO.readFile text
      let wts = L.map (evaluateLine nGrams) . T.lines $ input
      TIO.putStr . T.unlines . map (T.pack . show) $ wts
    ["-l", "-g", grammar, text] -> do
      nGrams <- loadNGrams grammar
      input  <- TIO.readFile text
      let wts = L.map (evaluateLine nGrams) . T.lines $ input
      TIO.putStr . T.unlines . map (T.pack . show) $ wts
    ["-g", grammar, text] -> do
      nGrams <- loadNGrams grammar
      input  <- TIO.readFile text
      let wts = L.map (evaluateLine nGrams) . T.lines $ input
      TIO.putStr . T.unlines . map (T.pack . show . exp) $ wts
    _ -> do
      TIO.putStr
      . T.pack
      $ "usage: NGrams [-l] -g <grammar> <inputFile> > <outputFile>\n"
