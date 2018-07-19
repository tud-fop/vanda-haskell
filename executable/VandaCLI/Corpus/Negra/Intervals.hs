{-|
Module:      VandaCLI.Corpus.Negra.Intevals
Description: Dealing with Predicates and Intevals when parsing the negra export format
Copyright:   Ⓒ Felix Völker, 2018
License:     BSD-style
Maintainer:  Felix.Voelker@tu-dresden.de
Stability:   unknown

This program decides the membership of /Dyck languages/ and /congruence multiple Dyck languages/ [cf. <http://fsmnlp2015.phil.hhu.de/wp-content/uploads/2015/06/denkinger-chomsky-schuetzenberger.pdf Tobias Denkinger: A Chomsky-Schützenberger result for weighted multiple context-free languages, 2015>].
-}
module VandaCLI.Corpus.Negra.Intervals (
    Intervals(..)
    ) where

data Intervals = Intervals String deriving (Eq, Show)
-- data Intervals = StringIntervals String | PredIntervals [[String]] deriving (Eq, Show)

myfoo = 5

