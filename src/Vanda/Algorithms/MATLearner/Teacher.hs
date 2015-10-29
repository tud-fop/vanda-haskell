module Vanda.Algorithms.MATLearner.Teacher where

import Vanda.Algorithms.MATLearner.TreeAutomaton
import Vanda.Algorithms.MATLearner.Util
import Vanda.Algorithms.MATLearner.Strings
import Data.Tree
import Graphics.UI.Gtk
import Control.Monad
import System.Exit


-- | The 'Teacher' class defines three methods which have to be implemented: 
--      * isMember receives a Tree and the teacher has to decide if the Tree is in the language, or not. 
--      * conjecture receives an 'Automaton' and the teacher has to decide if the Automaton is correct.
--      * getSigma returns the ranked alphabet of the language.
class Teacher t where 
        isMember :: t -> Tree String -> IO Bool
        conjecture :: (Ord b, Show b) => t -> Bool -> String -> Automaton b -> IO (Maybe (Either (Tree String) String))
        getSigma :: t -> IO [(String,Int)]



-- | An interactive teacher with a fixed alphabet: 'getSigma' returns the alphabet 
-- @
--      [("sigma",2),("gamma",1),("alpha",0)]
-- @.
-- 'isMember' and 'conjecture' will ask the user for an answer.
data Interactive = Interactive
instance Teacher Interactive where
        isMember Interactive baum = do
          -- create components
          dialog <- dialogNew
          set dialog [windowTitle := "isMember"]
          area <- dialogGetUpper dialog
          membershipQuestion <- labelNew (Just ("Is this tree part of the language?\n" ++ nicerShow baum))

          -- place components
          boxPackStart area membershipQuestion PackNatural 0
          dialogAddButton dialog "Yes" ResponseYes
          dialogAddButton dialog "No" ResponseNo

          -- display components
          widgetShowAll area

          -- ask for membership
          answer <- dialogRun dialog
          widgetDestroy dialog

          return $ answer == ResponseYes
        
        conjecture Interactive False oldTreeString automat = do
          -- create components
          dialog <- dialogNew
          set dialog [windowTitle := conjectureTitle]
          area <- dialogGetUpper dialog
          conjectureText <- labelNew (Just (conjectureTextQuestion++ show automat))

          -- place components
          boxPackStart area conjectureText PackNatural 0
          dialogAddButton dialog "Yes" ResponseYes
          dialogAddButton dialog "No" ResponseNo                                                 

          -- display components
          widgetShowAll area

          -- ask for membership
          answer <- dialogRun dialog
          widgetDestroy dialog

          case answer of
              ResponseYes -> return Nothing
              ResponseNo  -> askForCounterexample oldTreeString automat
              ResponseClose -> exitWith ExitSuccess
        conjecture Interactive True oldTreeString automat = askForCounterexample oldTreeString automat 


        getSigma Interactive = return [("s",2),("g",1),("a",0)]


askForCounterexample :: (Show t, Teacher t) => String -> t -> IO (Maybe (Either (Tree String) String))
askForCounterexample oldTreeString automat = do
            -- create components
          dialog <- dialogNew
          set dialog [windowTitle := conjectureTitle]
          counterexampleEntry <- entryNew
          area <- dialogGetUpper dialog
          conjectureText <- labelNew (Just (conjectureEnterCE ++ "\n" ++ show automat))

          -- place components
          boxPackStart area conjectureText PackNatural 0
          boxPackStart area counterexampleEntry PackNatural 0
          dialogAddButton dialog "Next" ResponseOk
          entrySetText counterexampleEntry oldTreeString
          -- autocompletion on enter
          onEntryActivate counterexampleEntry $ do
                                                    sigma <- getSigma automat
                                                    msg <- entryGetText counterexampleEntry
                                                    pos <- editableGetPosition counterexampleEntry
                                                    let (restMsg,symbol) = (take (pos-1) msg,last $ take pos msg)
                                                        symbols = filter (samePraefix ('\"':[symbol])) $ map (\(sym,arity) -> (show sym,arity)) sigma

                                                        samePraefix :: String -> (String,t) -> Bool
                                                        samePraefix (x:xs) ((y:ys),t)
                                                            | x == y    = samePraefix xs (ys,t)
                                                            | otherwise = False
                                                        samePraefix _      _      = True

                                                        --getLastSymbol :: (String,String) -> (String,String)
                                                        --getLastSymbol ([],ys)        = ([],ys)
                                                        --getLastSymbol (('\"':xs),ys) = (reverse xs,('\"':ys))
                                                        --getLastSymbol ((x:xs),ys)    = getLastSymbol (xs,ys ++ [x])
                                                        in when (length symbols == 1 && ((snd $ head symbols) /= 0))
                                                                (do
                                                                entrySetText counterexampleEntry $ restMsg ++ [symbol] ++ "()" ++ (drop pos msg)
                                                                editableSetPosition counterexampleEntry $ length $ restMsg ++ [symbol] ++ "(")
          -- display components
          widgetShowAll area

          -- ask for counterexample
          answer <- dialogRun dialog
          if answer /= ResponseOk
            then exitWith ExitSuccess
            else do
              counterexample <- entryGetText counterexampleEntry
              widgetDestroy dialog
              return $ Just (parseTree (filter (/= ' ') counterexample)) 


instance (Ord a) => Teacher (Automaton a) where
        isMember automat baum = return $ accepts automat baum
        conjecture automat1 _ _ automat2 = case isEmpty (unite (intersect (complement automat1) automat2) (intersect automat1 (complement automat2))) of
                                        Nothing -> return Nothing
                                        Just t  -> do
                                                    -- create components
                                                    dialog <- dialogNew
                                                    set dialog [windowTitle := conjectureTitle]
                                                    area <- dialogGetUpper dialog
                                                    conjectureText <- labelNew (Just (conjectureTextNotAutomaton ++ "\n" ++ show automat2))

                                                    -- place components
                                                    boxPackStart area conjectureText PackNatural 0
                                                    dialogAddButton dialog "Next Step" ResponseOk                                               

                                                    -- display components
                                                    widgetShowAll area

                                                    -- ask for membership
                                                    answer <- dialogRun dialog
                                                    widgetDestroy dialog
                                                    case answer of
                                                      ResponseOk    -> return $ Just (Left t)
                                                      ResponseClose -> exitWith ExitSuccess

        getSigma automat = return $ getAlphabet automat


data Automaton' a = A (Automaton a)
instance (Ord a, Show a) => Teacher (Automaton' a) where
        isMember (A automat) baum = return $ accepts automat baum
        conjecture (A automat1) False oldTreeString automat2 = case isEmpty (unite (intersect (complement automat1) automat2) (intersect automat1 (complement automat2))) of
                                        Nothing -> return Nothing
                                        Just t  -> do
                                                    ce <- askForCounterexample (nicerShow t) automat1
                                                    case ce of 
                                                      Just (Right _)   -> return $ ce
                                                      Just (Left tree) -> if accepts automat1 tree == accepts automat2 tree 
                                                        then return $ Just $ Right "Not CE in original Aut"
                                                        else return $ Just $ Left tree
        conjecture (A automat1) True oldTreeString automat2 = do
                                                    ce <- askForCounterexample oldTreeString automat1
                                                    case ce of 
                                                      Just (Right _)   -> return $ ce
                                                      Just (Left tree) -> if accepts automat1 tree == accepts automat2 tree 
                                                        then return $ Just $ Right "Not CE in original Aut"
                                                        else return $ Just $ Left tree

        getSigma (A automat) = return $ getAlphabet automat