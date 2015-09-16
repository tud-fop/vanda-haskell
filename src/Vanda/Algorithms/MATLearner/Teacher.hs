module Vanda.Algorithms.MATLearner.Teacher where

import Vanda.Algorithms.MATLearner.TreeAutomaton
import Vanda.Algorithms.MATLearner.Util
import Data.Tree

-- | A Teacher consisting of a Tree Corpus. 'isMember' is 'True', if the given tree is element of the corpus. 'conjecture' is 'True', if the given automaton accepts all trees in the corpus. 'getSigma' returns all occuring symbols with their respective rank.
newtype Corpus = Corpus [Tree String]

instance Teacher (Corpus) where
        isMember (Corpus corpus) tree = return $ elem tree corpus
        
        conjecture (Corpus []) _ = return Nothing
        conjecture (Corpus (x:xs)) automaton
          | accepts automaton x = conjecture (Corpus xs) automaton
          | otherwise = return $ Just x
        
        getSigma (Corpus []) = return $ []
        getSigma (Corpus (tree : corpus)) = do
          l1 <- extractSymbols tree
          l2 <- getSigma (Corpus corpus)
          return $ makeSet (l1 ++ l2)
              where
                extractSymbols :: Tree String -> IO [(String,Int)]
                extractSymbols (Node x list) = do
                  l <- getSigma (Corpus list)
                  return $ (x, length list) : l
                makeSet :: Eq a => [a] -> [a]
                makeSet [] = []
                makeSet (x : xs)
                  | elem x xs = makeSet xs
                  | otherwise = x : makeSet xs

-- | An interactive teacher with a fixed alphabet: 'getSigma' returns the alphabet 
-- @
--      [("sigma",2),("gamma",1),("alpha",0)]
-- @.
-- 'isMember' and 'conjecture' will ask the user for an answer.
data Interactive = Interactive
instance Teacher Interactive where
        isMember Interactive baum = do
          putStrLn "Is this tree part of the language?"
          putStrLn $ nicerShow baum
          putStrLn "y/n?"
          answer <- getLine
          return $ answer == "y"
        
        conjecture Interactive automat = do
          putStrLn "Is this Automaton correct?"
          putStrLn $ show automat
          putStrLn "y/n?"
          answer <- getLine
          if answer == "y" then return Nothing
                           else do
                             putStrLn "Please enter a counterexample:" 
                             tree <- getLine
                             return $ Just (parseTree (filter (/= ' ') tree,0))                                          
                             
        getSigma Interactive = return [("sigma",2),("gamma",1),("alpha",0)]

-- | An interactive teacher with a fixed string alphabet: 'getSigma' returns the alphabet 
-- @
--      [("a",1),("b",1),("0",0)]
-- @.
-- 'isMember' and 'conjecture' will ask the user for an answer, displaying the given trees as strings.
data InteractiveString = InteractiveString
instance Teacher InteractiveString where
        isMember InteractiveString baum = do
          putStrLn "Is this word part of the language?"
          putStrLn . reverse $ showAsString baum
          putStrLn "y/n?"
          answer <- getLine
          return $ answer == "y"
        
        conjecture InteractiveString automat = do
          putStrLn "Is this Automaton correct?"
          putStrLn $ show automat
          putStrLn "y/n?"
          answer <- getLine
          if answer == "y" then return Nothing
                           else do
                             putStrLn "Please enter a counterexample:"
                             tree <- getLine -- TODO reverse string or output of membership questions is reversed?
                             return $ Just (parseStringToTree (reverse tree) "0")
                             
        getSigma InteractiveString = return [("a",1),("b",1),("0",0)]
        
showAsString :: Tree String -> String
showAsString (Node a (t:_)) = a ++ showAsString t
showAsString (Node a []) = ""

instance (Ord a) => Teacher (Automaton a) where
        isMember automat baum = return $ accepts automat baum
        conjecture automat1 automat2 = return $ isEmpty (unite (intersect (complement automat1) automat2) (intersect automat1 (complement automat2)))
        getSigma automat = return $ getAlphabet automat

-- | The 'Teacher' class defines three methods which have to be implemented: 
--      * isMember receives a Tree and the teacher has to decide if the Tree is in the language, or not. 
--      * conjecture receives an 'Automaton' and the teacher has to decide if the Automaton is correct.
--      * getSigma returns the ranked alphabet of the language.
class Teacher t where 
        isMember :: t -> Tree String -> IO Bool
        conjecture :: (Ord b, Show b) => t -> Automaton b -> IO (Maybe (Tree String))
        getSigma :: t -> IO [(String,Int)]
        

        
        



        

                  