module Vanda.Algorithms.MATLearner.Util (parseFile, parseAutomaton, parseTree, parseStringToTree, nicerShow) where

import Vanda.Algorithms.MATLearner.TreeAutomaton
import Vanda.Algorithms.MATLearner.Strings
import Vanda.Hypergraph
import Data.Tree
import Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.Vector as V
import Debug.Trace




-- | Opens a file and parses it with a given parser.
parseFile :: (Show t) => FilePath -> (String -> t) -> IO t 
parseFile fp parser = do
    file <- readFile fp
    return $ parser (filter (/= ' ') file)



-- | Parses a String to a Tree in the following format: A tree consists of a node in quotation marks followed by its subtrees in brackets, separated by commas. The following exmaple is a tree in the correct format:
-- @
--      "sigma" [ "alpha" [], "alpha" [] ]
-- @
-- The integer value is used in a potential error message as line number.
parseTree :: String -> Either (Tree String) (String)
parseTree [] = Right counterexampleNothing
parseTree string = case s of
                        Right err   -> Right err
                        Left (symbol,subTree) -> case parseSubTree subTree of
                                                        Left st -> Left $ Node symbol (st)
                                                        Right err -> Right err
  where s = parseSymbol' string
        parseSubTree :: String -> Either [Tree String] String
        parseSubTree ( '(' : rest)
            | rest == ""       = Right parseErrorRightBracket
            | last rest == ')' = if length rest == 1 then Left []
                                                     else map' parseTree $ (separateTrees $ take ((length rest) - 1) rest)
            | otherwise        = Right parseErrorLeftBracket
        parseSubTree ""        = Left []
        parseSubTree _         = Right parseErrorLeftBracket
        parseSymbol' :: String -> Either (String,String) String
        parseSymbol' ('(' : rest) = Right parseErrorInvalidSymbol
        parseSymbol' (')' : rest) = Right parseErrorInvalidSymbol
        parseSymbol' ('"' : rest) = Right parseErrorInvalidSymbol
        parseSymbol' (c : rest) = Left ([c],rest)
        parseSymbol' [] = Right parseErrorNoTreeNode
          
        map' :: (a -> Either b c) -> [a] -> Either [b] c
        map' f [] = Left []
        map' f (a:rest) = case (f a,map' f rest) of
                               (Right c,_) -> Right c
                               (_,Right c) -> Right c
                               (Left b,Left c) -> Left (b:c)
                                          
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
-- The first line consists of a sequence of states in brackets, separated by commas, the final states.
-- All remaining lines consist of transitions in the following format: A sequence of states in brackets, 
-- followed by the node label in quotation marks, followed by the target state. 
-- The following example is in the correct format:
-- @
--      [1]
--      [] "a" 0
--      [0] "g" 1
--      [1] "g" 1
-- @
parseAutomaton :: String -> Automaton Int
parseAutomaton s = trace (show states) $ Automaton (EdgeList (states) _edges) (S.fromList $ fst finalstates)
    where finalstates = parseList (head (lines s)) 1
          _edges = map parseEdge $ zip (tail $ lines s) [2..]
          states = getStates (S.fromList $ fst finalstates) _edges
          
          getStates :: S.Set Int -> [Hyperedge Int String Int] -> S.Set Int
          getStates currentStates [] = currentStates 
          getStates currentStates (he:rest) = getStates (S.singleton (to he) `S.union` S.fromList (from he) `S.union` currentStates) rest

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