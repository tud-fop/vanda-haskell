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
helpText 1 = "The algorithm is filling the Observation Table\nwith information, whether given trees are part\nof the language or not."
helpText 2 = "The algorithm is determining, whether the\nObservation Table is closed, i.e. if every\nbitstring which occurs in the lower part\nof the table, exists in the upper part."
helpText 3 = "The algorithm is determining, whether the\nObservation Table is consistent, i.e. if for\nevery pair of trees s1 and s2 in the upper\npart holds: if their bitstrings are equal, the\nbitstrings of any context applied to\nthese trees have to be equal."
helpText 4 = "The algorithm is determining, whether the\nteacher agrees with the current automaton and calculates\na counterexample if not."
helpText 5 = "A counterexample has been given.\nThe algorithm now reduces this\ncounterexample to a minimal one."

helpTextOT  = "Something helpfull."

extractTableHead :: Int -> String
extractTableHead 1 = "Counterexample"
extractTableHead 2 = "Replaced subtree (s)"
extractTableHead 3 = "Inserted subtree (s')"

fileDialogTitle = "Choose Automaton"

fontObservationtable = "Courier 15"

buttonInteractiveText  = "Interactive Teacher"
buttonAutomatonText    = "Automaton Teacher"
buttonAutomatonIntText = "Half Interactive Automaton Teacher"

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
extractIsMember tree isMember = "The tree\n" ++ tree ++ "\nis" ++ (if isMember then " " else " NOT ") ++ "a member."

counterexampleAutInt tree = "The tree\n" ++ tree ++ "\nis NOT a counterexample according to the original automaton."
counterexampleNothing     = "Please enter a counterexample"
counterexampleNoTree      = "The counterexample is not a valid tree."
counterexampleMember      = "Membership is already known and this tree is not a counterexample!"

parseErrorRightBracket   = "')' missing."
parseErrorLeftBracket    = "'(' missing."
parseErrorInvalidSymbol  = "Node symbol can't be any of '(',')',','."
parseErrorNoTreeNode     = "No Tree Node symbol given."
parseErrorOnlyNumbers    = "States must be integers."
parseErrorStateSeperator = "States have to be seperated by ','."
parseErrorArrowMissing   = "Arrow between child states and label is missing."

isMemberTitle         = "isMember"
isMemberQuestion tree = "Is this tree part of the language?\n" ++ tree
isMemberYes           = "Yes"
isMemberNo            = "No"

helpButtonLabel ="?"

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



-- | Display a Tree as a String. Only display parantheses, if the number of children > 0.
nicerShow :: Tree String -> String
nicerShow (Node a []  ) = a 
nicerShow (Node a list) = a ++ "(" ++ (intercalate "," $ map nicerShow list) ++ ")"