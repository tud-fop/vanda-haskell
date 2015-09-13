module Vanda.Algorithms.MATLearner.Util where

import Vanda.Algorithms.MATLearner.MATLearner
import Data.Tree

nonVerboseCorpus :: FilePath -> IO ()
nonVerboseCorpus filepath = do
  corpus <- parseFile filepath parseCorpus
  automaton <- main' corpus
  putStrLn $ show automaton

parseFile :: (Teacher t) => FilePath -> (String -> t) -> IO t 
parseFile fp parser = do
  file <- readFile fp
  putStrLn $ "Parsing file " ++ fp
  return $ parser (filter (/= ' ') file)

parseCorpus :: String -> Corpus
parseCorpus s = Corpus (map parseTree $ zip (lines s) [1..])

parseTree :: (String,Int) -> Tree Int
parseTree ([],l) = error $ "Parsing Error in Line " ++ show l ++ ": Line mustn't be empty."
parseTree ((symbol : subTree),l) = Node (read' [symbol] l) (parseSubTree (subTree,l))
  where parseSubTree :: (String,Int) -> [Tree Int]
        parseSubTree (( '[' : rest),l)
          | rest == "" = error $ "Parsing Error in Line " ++ show l ++ ": ']' missing."
          | last rest == ']' = if length rest == 1 then []
                                                   else map parseTree $ (zip (separateTrees $ take ((length rest) - 1) rest) [l,l..])
          | otherwise = error $ "Parsing Error in Line " ++ show l ++ ": ']' missing."
        parseSubTree (_,l) = error $ "Parsing Error in Line " ++ show l ++ ": '[' missing."
        read' :: String -> Int -> Int
        read' string line = case reads string of
                                 [(x, "")] -> x
                                 _ -> error $ "Parsing Error in Line " ++ show line ++ ": Symbols have to be integers."
 

separateTrees :: String -> [String]
separateTrees s = separateTrees' 0 "" s
  where separateTrees' :: Int -> String -> String -> [String]
        separateTrees' _ currentWord [] = [currentWord]
        separateTrees' level currentWord (c:rest) 
          | and [(c == ','),(level <= 0)] = (currentWord : separateTrees rest)
          | c == '[' = separateTrees' (level + 1) (currentWord ++ "[") rest
          | c == ']' = separateTrees' (level - 1) (currentWord ++ "]") rest
          | otherwise = separateTrees' level (currentWord ++ [c]) rest