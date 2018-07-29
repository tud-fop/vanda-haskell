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
    Intervals,
    isInIntervals,
    isInPred,
    getPred
    ) where


import           Data.List.Split

type Intervals = String
-- data Intervals = StringIntervals String | PredIntervals [[String]] deriving (Eq, Show)
isInIntervals :: Intervals -> Int -> Bool
isInIntervals intervals x = any ($ x) (getPred intervals)
-- isInIntervals x intervals = any ($ x) (getPred intervals)

isInPred :: [Int->Bool] -> Int -> Bool
isInPred p x = any ($ x) p


getPred :: Intervals -> [Int->Bool]
getPred intervals = map getSinglePred (getPredList intervals)

getPredList :: Intervals -> [[String]]
getPredList intervals = map (splitOn "-") (splitOn "," intervals)

getSinglePred :: [String] -> Int->Bool
getSinglePred [""]    = const True
getSinglePred [x]     = \y -> y == read x
getSinglePred ["",x]  = \y -> y <= read x
getSinglePred [x, ""] = \y -> y >= read x
getSinglePred [x,y]   = \z -> (z <= read y) && (z >= read x)
getSinglePred _       = const True
