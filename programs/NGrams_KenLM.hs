module Main where

import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import System.Environment ( getArgs )
import Control.Monad

import Vanda.Grammar.NGrams.KenLM

main
  :: IO ()
main = do
  args <- getArgs
  case args of
    ["-g", grammar, text] -> do
      input  <- TIO.readFile text
      let nGrams = loadNGrams grammar
      let wts = L.map (evaluateLine nGrams)
              . T.lines
              $ input
      TIO.putStr . T.unlines
                 . map (T.pack . show)
                 $ wts
    ["-g", grammar, "-i", text] -> do
      input  <- TIO.readFile text
      let nGrams = loadNGrams grammar
      let wts = L.map (evaluateLine nGrams)
              . T.lines
              $ input
      TIO.putStr . T.unlines
                 . map (T.pack . show)
                 $ wts
    ["-g", grammar, text, "-o", outFile] -> do
      input  <- TIO.readFile text
      let nGrams = loadNGrams grammar
      let wts = L.map (evaluateLine nGrams)
              . T.lines
              $ input
      TIO.writeFile outFile
                  . T.unlines
                  . map (T.pack . show)
                  $ wts
    ["-g", grammar, "-i", text, "-o", outFile] -> do
      input  <- TIO.readFile text
      let nGrams = loadNGrams grammar
      let wts = L.map (evaluateLine nGrams)
              . T.lines
              $ input
      TIO.writeFile outFile
                  . T.unlines
                  . map (T.pack . show)
                  $ wts
    _ -> do
      TIO.putStr
      . T.pack
      $ "usage: NGrams_KenLM -g <grammar> [-i] <inputFile> [-o <outputFile>]\n"
