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
      nGrams <- loadNGrams grammar
      input  <- TIO.readFile text
      wts <- sequence
           . L.map (evaluateLine nGrams)
           . T.lines
           $ input
      TIO.putStr . T.unlines
                 . map (T.pack . show)
                 $ wts
