module Vanda.Algorithms.MATLearner.Util (parseFile, parseCorpus, parseAutomaton, parseTree, parseStringToTree, nicerShow) where

import Vanda.Algorithms.MATLearner.TreeAutomaton
import Vanda.Hypergraph
import Data.Tree
import Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.Vector as V


-- | Display a Tree as a String. Only display parantheses, if the number of children > 1.
nicerShow :: Tree String -> String
nicerShow (Node a []  ) = a 
nicerShow (Node a list) = a ++ "(" ++ (intercalate "," $ map nicerShow list) ++ ")"


-- | Opens a file and parses it with an arbitrary parser.
parseFile :: FilePath -> (String -> t) -> IO t 
parseFile fp parser = do
    file <- readFile fp
    putStrLn $ "Parsing file " ++ fp
    return $ parser (filter (/= ' ') file)


-- | Parses a Tree Corpus from the following text format: One tree per line, the tree format is specified at 'parseTree'
parseCorpus :: String -> Forest String
parseCorpus s = map parseTree $ zip (lines s) [1..]


-- | Parses a String to a Tree in the following format: A tree consists of a node in quotation marks followed by its subtrees in brackets, separated by commas. The following exmaple is a tree in the correct format:
-- @
--      "sigma" [ "alpha" [], "alpha [] ]
-- @
-- The integer value is used in a potential error message as line number.
parseTree :: (String,Int) -> Tree String
parseTree ([],line) = parsingError line "No empty Lines allowed."
parseTree (string,line)
    | symbol == "" = parsingError line "Tree Nodes can't be empty."
    | otherwise    = Node symbol (parseSubTree (subTrees,line))
        where (symbol,subTrees) = parseSymbol string line
              parseSubTree :: (String,Int) -> [Tree String]
              parseSubTree (( '[' : rest),l)
                  | rest == ""       = parsingError l "']' missing."
                  | last rest == ']' = if length rest == 1 then []
                                                           else map parseTree $ (zip (separateTrees $ take ((length rest) - 1) rest) [l,l..])
                  | otherwise        = parsingError l "']' missing."
              parseSubTree (_,l)     = parsingError l "'[' missing."
 

-- | Parses a string to a monadic tree, treating every character as a node. The final node is represented by the second argument.
parseStringToTree :: String -> String -> Tree String
parseStringToTree [] end       = Node end []
parseStringToTree (a:rest) end = Node [a] [parseStringToTree rest end]


separateTrees :: String -> [String]
separateTrees s = separateTrees' 0 "" s
    where separateTrees' :: Int -> String -> String -> [String]
          separateTrees' _     currentWord [] = [currentWord]
          separateTrees' level currentWord (c:rest) 
              | and [(c == ','),(level <= 0)] = currentWord : separateTrees rest
              | c == '['                      = separateTrees' (level + 1) (currentWord ++ "[") rest
              | c == ']'                      = separateTrees' (level - 1) (currentWord ++ "]") rest
              | otherwise                     = separateTrees' level       (currentWord ++ [c]) rest
                    

parsingError :: Int -> String -> a
parsingError line message = error $ "Parsing Error in Line " ++ show line ++ ": " ++ message

-- | Parses an Automaton from the following text format: States are coded as integers, node labels as strings. 
-- The first line consists of a single number, the maximal state.
-- The second line consists of a sequence of states in brackets, separated by commas, the final states.
-- All remaining lines consist of transitions in the following format: A sequence of states in brackets, 
-- followed by the node label in quotation marks, followed by the target state. 
-- The following example is in the correct format:
-- @
--      1
--      [1]
--      [] "a" 0
--      [0] "gamma" 1
--      [1] "gamma" 1
-- @
parseAutomaton :: String -> Automaton Int
parseAutomaton s = Automaton (EdgeList (S.fromList states) _edges) (S.fromList $ fst finalstates)
    where states = [0..(read $ head (lines s))]
          finalstates = parseList (head $ tail (lines s)) 2
          _edges = map parseEdge $ zip (drop 2 $ lines s) [3..]
                

parseEdge :: (String,Int) -> Hyperedge Int String Int
parseEdge (s,l) = Hyperedge _to (V.fromList _from) _label 0
    where (_from,s') = parseList s l
          (_label,s'') = parseSymbol s' l
          (_to) = read s''


parseList :: String -> Int -> ([Int],String)
parseList ('[':']':rest) _    = ([],rest)
parseList ('[':rest)     line = parseList' rest line
    where parseList' :: String -> Int -> ([Int],String)
          parseList' []   l = parsingError l "']' missing."
          parseList' list l = 
              case reads list of
                  [(x,s')] -> let (xs,s'') = parseList'' s' l in ((x:xs),s'')
                  _        -> parsingError l "Only numbers allowed."

          parseList'' :: String -> Int -> ([Int],String)
          parseList'' (',':s) l = parseList' s l
          parseList'' (']':s) _ = ([],s)
          parseList'' _       l = parsingError l "Numbers have to be seperated by ','."
parseList _              line = parsingError line "'[' missing."


parseSymbol :: String -> Int -> (String,String)
parseSymbol ('"' : rest) line = parseSymbol' rest line
    where parseSymbol' :: String -> Int -> (String, String)
          parseSymbol' ('"': s) _ = ("",s)
          parseSymbol' (c  : s) l = let (restSymbol,restString) = parseSymbol' s l in (c: restSymbol,restString)
          parseSymbol' []       l = parsingError l "'\"' missing."
parseSymbol _             line = parsingError line "'\"' missing."

