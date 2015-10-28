module Vanda.Algorithms.MATLearner.Strings where

import Vanda.Algorithms.MATLearner.TreesContexts
import Graphics.UI.Gtk
import Data.Tree
import Data.List

inactiveColor = Color 52254 49220 49220

activeColor :: Int -> Color
activeColor 1 = colorUpdate
activeColor 2 = colorClosed
activeColor 3 = colorConsistent
activeColor 4 = Color 52254 10000 0
activeColor 5 = colorExtract

status :: Int -> String
status 1 = "Fill Table"
status 2 = "Closed"
status 3 = "Consistent"
status 4 = "Correct"
status 5 = "Extract"

helpText :: Int -> String
helpText 1 = "The algorithm is filling the Observation Table with information, whether given trees are part of the language or not."
helpText 2 = "The algorithm is determining whether the Observation Table is closed, i.e. if every bitstring which occurs in the lower part of the table, exists in the upper part."
helpText 3 = "The algorithm is determining whether the Observation Table is consistent, i.e. if for every pair of trees in the upper part holds: if their bitstrings are equal, the bitstrings of any context applied to these trees have to be equal." -- TODO: Besser erkären
helpText 4 = "The algorithm is determining, whether the teacher agrees with the current automaton and calculates a counterexample if not."
helpText 5 = "A counterexample has been given. The algorithm now reduces this counterexample to a minimal one," -- TODO: BEsser erklären

extractTableHead :: Int -> String
extractTableHead 1 = "Counterexample"
extractTableHead 2 = "Replaced subtree (s)"
extractTableHead 3 = "Inserted subtree (s')"

fileDialogTitle = "Choose Automaton"

fontObservationtable = "Courier 15"

buttonInteractiveText = "Interactive Teacher"
buttonAutomatonText   = "Automaton Teacher"

observationTableFrame       = "Observation Table"
statusFrame                 = "Status"
menueTitle                  = "MATLearner"
observationTableDialogTitle = "MATLearner"
extractTitle                = "Extract"

isClosedMsg        = "Observation Table is closed."
isNotClosedMsg     = "Observation Table is not closed."
isConsistentMsg    = "Observation Table is consistent."
isNotConsistentMsg = "Observation Table is not consistent."

nextStep           = "Next Step"
lastStep           = "Close"
addContext context = "The context\n" ++ showContext context ++ "\nwill be added to C."
addTree tree       = "The tree\n" ++ nicerShow tree ++ "\nwill be added to S."
notCorrect tree    = "The automaton is not correct. The given counterexample is\n" ++ nicerShow tree
extracted tree     = "The tree\n" ++ nicerShow tree ++ "\nwas extracted and will be added to S."

counterexampleNothing = "Please enter a counterexample"
counterexampleNoTree  = "The counterexample is not a valid tree."
counterexampleMember  = "Membership is already known and this tree is not a counterexample!"

parseErrorRightBracket   = "')' missing."
parseErrorLeftBracket   = "'(' missing."
parseErrorInvalidSymbol = "Node symbol can't be any of '(',')'."
parseErrorNoTreeNode    = "No Tree Node symbol given."


conjectureTitle = "Conjecture"
conjectureTextQuestion = "Is this your Automaton?\n"
conjectureEnterCE = "Please enter a counterexample\n"
conjectureTextNotAutomaton = "This is not the Automaton.\n"
infoDialog = "MATLearner"
tryAgain = "Try again."

-- colors for table
colorNormal = Color 0 0 0
colorConsistent = Color 65535 0 42668
colorClosed = Color 0 0 65535
colorUpdate = Color 65535 0 0
colorExtract = Color 6592 31310 3756



-- | Display a Tree as a String. Only display parantheses, if the number of children > 1.
nicerShow :: Tree String -> String
nicerShow (Node a []  ) = a 
nicerShow (Node a list) = a ++ "(" ++ (intercalate "," $ map nicerShow list) ++ ")"