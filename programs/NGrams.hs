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
    ["--train", grammar] -> do
      nGrams <- loadNGrams grammar
      TIO.putStr $ writeNGrams nGrams
    ["-g", grammar, "-l", text] -> do
      nGrams <- loadNGrams grammar
      input  <- TIO.readFile text
      let wts = L.map (evaluateLine nGrams) $ T.lines input
      TIO.putStr . T.unlines . flip map wts $ T.pack . show
    ["-l", "-g", grammar, text] -> do
      nGrams <- loadNGrams grammar
      input  <- TIO.readFile text
      let wts = L.map (evaluateLine nGrams) . T.lines $ input
      TIO.putStr . T.unlines . flip map wts $ T.pack . show
    ["-g", grammar, text] -> do
      nGrams <- loadNGrams grammar
      input  <- TIO.readFile text
      let wts = L.map (evaluateLine nGrams) . T.lines $ input
      TIO.putStr . T.unlines . flip map wts $ T.pack . show . exp
    _ -> do
      TIO.putStr
      . T.pack
      $  "usage: NGrams [-l] -g <grammar> [<corpus>] > <scores>\n"
      ++ "  or   NGrams --train <corpus> > <grammar>"
