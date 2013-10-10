module Main where

import Vanda.Corpus.Penn.Text
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Tree
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      cTrees <- TIO.getContents
      TIO.putStr . yield $ (parsePenn cTrees :: [Tree String])
    _ -> putStr $ "Usage: PennToSentenceCorpus < treeCorpus > sentenceCorpus\n"
