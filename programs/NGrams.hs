module Main where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment ( getArgs )

import Vanda.Grammar.NGrams.Functions

main
  :: IO ()
main = do
  args <- getArgs
  case args of
    ["--train"] -> do
      corpus <- TIO.getContents
      let nGrams = trainModel 5 3 corpus
      TIO.putStr $ writeNGrams nGrams
    [grammar, "-l"] -> do
      nGrams <- loadNGrams grammar
      input  <- TIO.getContents
      let wts = L.map (evaluateLine nGrams) $ T.lines input
      TIO.putStr . T.unlines . flip map wts $ T.pack . show
    ["-l", grammar] -> do
      nGrams <- loadNGrams grammar
      input  <- TIO.getContents
      let wts = L.map (evaluateLine nGrams) . T.lines $ input
      TIO.putStr . T.unlines . flip map wts $ T.pack . show
    [grammar] -> do
      nGrams <- loadNGrams grammar
      input  <- TIO.getContents
      let wts = L.map (evaluateLine nGrams) . T.lines $ input
      TIO.putStr . T.unlines . flip map wts $ T.pack . show . exp
    _ -> do
      TIO.putStr
      . T.pack
      $  "usage: NGrams [-l] MODEL < CORPUS > SCORES   # scores each sentence in a corpus\n"
      ++ "  or   NGrams --train < CORPUS > MODEL       # trains an n-gram model"
