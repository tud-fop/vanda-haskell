module Main where

import Data.List
import System.Environment (getArgs, getProgName)

import Vanda.Corpus.TreeTerm
import Vanda.Util.Tree


main :: IO ()
main = do
  args <- getArgs
  ts <- getContents
  let delim    = if "-m" `elem` args then "$" else ""
  let indenter = if "-t" `elem` args then "\t" else ""
  case args of
    [] -> printUsage
    _ | any (`notElem` ["-m", "-t"]) args -> printUsage
      | otherwise -> putStrLn
                     . unlines
                     . intersperse ""
                     . map (flip (++) ";" . toTikZ delim delim indenter . parseTree)
                     $ lines ts


printUsage :: IO ()
printUsage = do
  pname <- getProgName
  putStrLn $    "usage: echo \"<tree term>\" | "
             ++ pname
             ++ " [-m] [-t] \n       e.g. echo \"a(b,c)\" | "
             ++ pname
             ++ " -t"
