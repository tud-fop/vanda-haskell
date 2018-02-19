{-|
Module:      Vanda.Algorithms.MATLearner.Parser
Description: Parsing functions for the MAT Learner
Copyright:   (c) Technische Universität Dresden 2015
License:     BSD-style
Maintainer:  Markus Napierkowski <markus.napierkowski@mailbox.tu-dresden.de>
Stability:   unknown
-}

module Vanda.Algorithms.MATLearner.Parser (parseFile, parseAutomaton, parseTree, parseStringToTree) where

import Vanda.Algorithms.MATLearner.TreeAutomaton hiding (getStates)
import Vanda.Algorithms.MATLearner.Strings
import Vanda.Hypergraph
import Data.Tree
import qualified Data.Set as S
import qualified Data.Vector as V




-- | Opens a file and parses it with a given parser.
parseFile :: (Show t) => FilePath -> (String -> t) -> IO t 
parseFile fp parser = do
    file <- readFile fp
    return $ parser (filter (/= ' ') file)



-- | Parses a String to a Tree in the following format: A tree consists of a node (a simple character) followed by its subtrees in parantheses, separated by commas. With symbols of rank 0, the parantheses can be ommited. The following exmaple is a tree in the correct format:
-- @
--      s(g(a),a)
-- @
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
        parseSymbol' ('(' : _) = Right parseErrorInvalidSymbol
        parseSymbol' (')' : _) = Right parseErrorInvalidSymbol
        parseSymbol' ('"' : _) = Right parseErrorInvalidSymbol
        parseSymbol' (c : rest) = Left ([c],rest)
        parseSymbol' [] = Right parseErrorNoTreeNode
          
                                          
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
              | c == '('                      = separateTrees' (level + 1) (currentWord ++ "(") rest
              | c == ')'                      = separateTrees' (level - 1) (currentWord ++ ")") rest
              | otherwise                     = separateTrees' level       (currentWord ++ [c]) rest
                    

map' :: (a -> Either b c) -> [a] -> Either [b] c
map' _ [] = Left []
map' f (a:rest) = case (f a,map' f rest) of
                        (Right c,_) -> Right c
                        (_,Right c) -> Right c
                        (Left b,Left c) -> Left (b:c)

parsingError :: Int -> String -> String
parsingError line message = "Parsing Error in Line " ++ show line ++ ": " ++ message

-- | Parses an Bottom-Up Tree Automaton from the following text format: States are coded as integers, node labels as characters. 
-- The first line consists of a sequence of states in parantheses, separated by commas, the final states.
-- All remaining lines consist of transitions in the following format: A sequence of states in parantheses, followed by an arrow,
-- followed by the node label, followed by the target state. 
-- The following example is in the correct format:
-- @
--      (0)
--      () -> a 0
--      (0) -> g 1
--      (1) -> g 1
-- @ 
parseAutomaton :: String -> Either (Automaton Int) String
parseAutomaton s = case (finalstates,_edges) of
                        ((Right err,_),_)                   -> Right err
                        (_,Right err)                       -> Right err
                        ((Left finalstates',_),Left edges') -> Left $ Automaton (EdgeList (states) edges') (S.fromList finalstates')
    where finalstates = parseList (head (lines s)) 1
          _edges = map' parseEdge $ zip (tail $ lines s) [2..]
          states = case (finalstates,_edges) of
                        ((Left finalstates',_),Left edges') -> getStates (S.fromList finalstates') edges'
                        (_,_)                               -> S.empty
          
          getStates :: S.Set Int -> [Hyperedge Int String Int] -> S.Set Int
          getStates currentStates [] = currentStates 
          getStates currentStates (he:rest) = getStates (S.singleton (to he) `S.union` S.fromList (from he) `S.union` currentStates) rest

parseEdge :: (String,Int) -> Either (Hyperedge Int String Int) String
parseEdge (s,l) = case (_from,_label,_to,arrow) of
                       (_,Right err,_,_) -> Right err
                       (Right err,_,_,_) -> Right err
                       (_,_,_,Just err)  -> Right err
                       (_,_,Right err,_) -> Right err
                       (Left from',Left label',Left to',Nothing) -> Left $ Hyperedge to' (V.fromList from') label' 0
  where (_label,s') = parseSymbol s l
        (_from,s'') = parseList s' l
        (arrow,s''') = parseArrow s'' l
        (_to) = case reads s''' of
                     [(x,_)] -> Left x
                     _        -> Right $ parsingError l parseErrorOnlyNumbers


parseArrow :: String -> Int -> (Maybe String,String)
parseArrow ('-':'>':rest) _ = (Nothing,rest)
parseArrow _ l = (Just $ parsingError l parseErrorArrowMissing,"")
                       
parseList :: String -> Int -> (Either [Int] String,String)
parseList ('(':')':rest) _    = (Left [],rest)
parseList ('(':rest)     line = parseList' rest line
  where parseList' :: String -> Int -> (Either [Int] String,String)
        parseList' []   l = (Right $ parsingError l parseErrorRightBracket,"")
        parseList' list l = 
          case reads list of
                [(x,s')] -> let (xs,s'') = parseList'' s' l in 
                                case xs of 
                                      Right err -> (Right err,s'')
                                      Left xs'  -> (Left (x:xs'),s'')
                _        -> (Right $ parsingError l parseErrorOnlyNumbers,"")

        parseList'' :: String -> Int -> (Either [Int] String,String)
        parseList'' (',':s) l = parseList' s l
        parseList'' (')':s) _ = (Left [],s)
        parseList'' _       l = (Right $ parsingError l parseErrorStateSeperator,"")
parseList _              line = (Right $ parsingError line parseErrorLeftBracket,"")

parseSymbol :: String -> Int -> (Either String String,String)
parseSymbol ('(' : rest) i = (Right $ parsingError i parseErrorInvalidSymbol,rest)
parseSymbol (')' : rest) i = (Right $ parsingError i parseErrorInvalidSymbol,rest)
parseSymbol ('"' : rest) i = (Right $ parsingError i parseErrorInvalidSymbol,rest)
parseSymbol (c : rest) _ = (Left [c],rest)
parseSymbol [] i = (Right $ parsingError i parseErrorNoTreeNode,"")
