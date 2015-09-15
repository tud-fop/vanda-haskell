module Vanda.Algorithms.MATLearner.Util where

import Vanda.Algorithms.MATLearner.TreeAutomaton
import Vanda.Hypergraph
import Data.Tree
import qualified Data.Set as S
import qualified Data.Vector as V


parseFile :: FilePath -> (String -> t) -> IO t 
parseFile fp parser = do
  file <- readFile fp
  putStrLn $ "Parsing file " ++ fp
  return $ parser (filter (/= ' ') file)

parseCorpus :: String -> Forest String
parseCorpus s = map parseTree $ zip (lines s) [1..]

parseTree :: (String,Int) -> Tree String
parseTree ([],line) = parsingError line "No empty Lines allowed."
parseTree (string,line)
  | symbol == "" = parsingError line "Tree Nodes can't be empty."
  | otherwise    = Node symbol (parseSubTree (subTrees,line))
  where (symbol,subTrees) = parseSymbol string line
        parseSubTree :: (String,Int) -> [Tree String]
        parseSubTree (( '[' : rest),l)
          | rest == "" = parsingError l "']' missing."
          | last rest == ']' = if length rest == 1 then []
                                                   else map parseTree $ (zip (separateTrees $ take ((length rest) - 1) rest) [l,l..])
          | otherwise = parsingError l "']' missing."
        parseSubTree (_,l) = parsingError l "'[' missing."
 
parseStringToTree :: (String,String) -> Tree String
parseStringToTree ([],end) = Node end []
parseStringToTree ((a:rest),end) = Node [a] [parseStringToTree (rest,end)]

separateTrees :: String -> [String]
separateTrees s = separateTrees' 0 "" s
  where separateTrees' :: Int -> String -> String -> [String]
        separateTrees' _ currentWord [] = [currentWord]
        separateTrees' level currentWord (c:rest) 
          | and [(c == ','),(level <= 0)] = (currentWord : separateTrees rest)
          | c == '[' = separateTrees' (level + 1) (currentWord ++ "[") rest
          | c == ']' = separateTrees' (level - 1) (currentWord ++ "]") rest
          | otherwise = separateTrees' level (currentWord ++ [c]) rest
          
parsingError :: Int -> String -> a
parsingError line message = error $ "Parsing Error in Line " ++ show line ++ ": " ++ message

parseAutomaton :: String -> Automaton Int
parseAutomaton s = Automaton (EdgeList (S.fromList states) edges) (S.fromList $ fst finalstates)
  where states = [0..(read $ head (lines s))]
        finalstates = parseList (head $ tail (lines s)) 2
        edges = map parseEdge $ zip (drop 2 $ lines s) [3..]
        
parseEdge :: (String,Int) -> Hyperedge Int String Int
parseEdge (s,l) = Hyperedge to (V.fromList from) label 0
  where (from,s') = parseList s l
        (label,s'') = parseSymbol s' l
        (to) = read s''

parseList :: String -> Int -> ([Int],String)
parseList ('[':']':s) _ = ([],s)
parseList ('[':s) l = parseList' s l
  where parseList' :: String -> Int -> ([Int],String)
        parseList' [] l = parsingError l "']' missing."
        parseList' list l = 
          case reads list of
            [(x,s')] -> let (xs,s'') = parseList'' s' l in ((x:xs),s'')
            _        -> parsingError l "Only numbers allowed."
        parseList'' :: String -> Int -> ([Int],String)
        parseList'' (',':s) l = parseList' s l
        parseList'' (']':s) _ = ([],s)
        parseList'' _ l = parsingError l "Numbers have to be seperated by ','."
parseList _ l = parsingError l "'[' missing."
        
parseSymbol :: String -> Int -> (String,String)
parseSymbol ('"' : s) l = parseSymbol' s l
  where parseSymbol' :: String -> Int -> (String, String)
        parseSymbol' ('"': s) _ = ("",s)
        parseSymbol' (c : s) l = let (restSymbol,restString) = parseSymbol' s l in (c: restSymbol,restString)
        parseSymbol' [] l = parsingError l "'\"' missing."
parseSymbol _ l = parsingError l "'\"' missing."