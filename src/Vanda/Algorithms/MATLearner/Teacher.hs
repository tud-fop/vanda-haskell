module Vanda.Algorithms.MATLearner.Teacher where

import Vanda.Algorithms.MATLearner.TreeAutomaton
import Vanda.Algorithms.MATLearner.Util
import Data.Tree
import Graphics.UI.Gtk
import Control.Monad


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
          set dialog [windowTitle := "Conjecture"]
          area <- dialogGetUpper dialog
          conjectureText <- labelNew (Just ("Is this your Automaton?" ++ show automat))

          -- place components
          boxPackStart area conjectureText PackNatural 0
          dialogAddButton dialog "Yes" ResponseYes
          dialogAddButton dialog "No" ResponseNo                                                 

          -- display components
          widgetShowAll area

          -- ask for membership
          answer <- dialogRun dialog
          widgetDestroy dialog

          if answer == ResponseYes then return Nothing
                                   else askForCounterexample oldTreeString automat
        conjecture Interactive True oldTreeString automat = askForCounterexample oldTreeString automat 


        getSigma Interactive = return [("s",2),("g",1),("a",0)]


askForCounterexample oldTreeString automat = do
            -- create components
          dialog <- dialogNew
          set dialog [windowTitle := "Conjecture"]
          counterexampleEntry <- entryNew
          area <- dialogGetUpper dialog
          conjectureText <- labelNew (Just ("Please enter a counterexample\n" ++ show automat))

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
                                                    let (restMsg,symbol) = getLastSymbol $ (reverse $ take pos msg,[])
                                                        symbols = filter (samePraefix symbol) $ map (show . fst) sigma

                                                        samePraefix :: String -> String -> Bool
                                                        samePraefix (x:xs) (y:ys)
                                                            | x == y    = samePraefix xs ys
                                                            | otherwise = False
                                                        samePraefix _      _      = True

                                                        getLastSymbol :: (String,String) -> (String,String)
                                                        getLastSymbol ([],ys)        = ([],ys)
                                                        getLastSymbol (('\"':xs),ys) = (reverse xs,('\"':ys))
                                                        getLastSymbol ((x:xs),ys)    = getLastSymbol (xs,ys ++ [x])
                                                        in when (length symbols == 1)
                                                                (do
                                                                entrySetText counterexampleEntry $ restMsg ++ (head symbols) ++ " [ ]" ++ (drop pos msg)
                                                                editableSetPosition counterexampleEntry $ length $ restMsg ++ (head symbols) ++ " [")
          -- display components
          widgetShowAll area

          -- ask for counterexample
          answer <- dialogRun dialog
          counterexample <- entryGetText counterexampleEntry
          widgetDestroy dialog
          return $ Just (parseTree (filter (/= ' ') counterexample)) 


-- | diplay dialog with the given taxt and destroy it afterwards
displayDialog :: String -> String -> IO ()
displayDialog labelText buttonText = do
            dialog <- dialogNew
            set dialog [windowTitle := infoDialog]
            area <- dialogGetUpper dialog
            label <- labelNew (Just labelText)

            -- place components
            boxPackStart area label PackNatural 0
            dialogAddButton dialog buttonText ResponseOk

            -- display components
            widgetShowAll area

            -- wait for ok
            answer <- dialogRun dialog
            widgetDestroy dialog
            return ()


instance (Ord a) => Teacher (Automaton a) where
        isMember automat baum = return $ accepts automat baum
        conjecture automat1 _ _ automat2 = case isEmpty (unite (intersect (complement automat1) automat2) (intersect automat1 (complement automat2))) of
                                        Nothing -> return Nothing
                                        Just t  -> return $ Just (Left t)

        getSigma automat = return $ getAlphabet automat



infoDialog = "MATLearner"
tryAgain = "Try again."