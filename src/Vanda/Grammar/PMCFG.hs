-- (c) 2016 Tobias Denkinger <Tobias.Denkinger@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

module Vanda.Grammar.PMCFG
  ( VarT (Var, T)
  , Rule (Rule)
  -- * Parallel multiple context-free grammars
  , PMCFG
  , fromRules
  , yield
  -- * Weighted parallel multiple context-free grammars
  , WPMCFG
  , fromWeightedRules
  -- * pretty printing
  , prettyPrintRule
  , prettyPrintComposition
  -- * examples
  , examplePMCFG
  , exampleWPMCFG
  , exampleDerivation
  , exampleRules
  , exampleCompositions
  ) where

import Control.Arrow (first)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Tree

data VarT t = T t | Var !Int !Int deriving (Eq, Ord, Show)

instance Functor VarT where
  fmap f (T t)     = T (f t)
  fmap _ (Var i j) = Var i j

-- | 'Rule' ((A, [A₁, …, Aₖ]), f) ~ A → f(A₁, …, Aₖ).
newtype Rule nt t = Rule ((nt, [nt]), [[VarT t]]) deriving (Eq, Ord, Show)

instance Functor (Rule nt) where
  fmap f (Rule (nts, varts)) = Rule (nts, map (map (fmap f)) varts)


data PMCFG nt t = PMCFG [nt] [Rule nt t] deriving Show

instance Functor (PMCFG nt) where
  fmap f (PMCFG ints rs) = PMCFG ints $ map (fmap f) rs

fromRules
  :: [nt] -- ^ initial non-terminals
  -> [Rule nt t] -- ^ rules
  -> PMCFG nt t
fromRules = PMCFG


data WPMCFG nt w t = WPMCFG [nt] [(Rule nt t, w)] deriving Show

instance Functor (WPMCFG nt w) where
  fmap f (WPMCFG ints rs) = WPMCFG ints $ map (first (fmap f)) rs

fromWeightedRules
  :: [nt] -- ^ initial non-terminals
  -> [(Rule nt t, w)] -- ^ weighted rules
  -> WPMCFG nt w t
fromWeightedRules = WPMCFG


yield :: Tree (Rule nt t) -> Maybe [t]
yield = fmap head . evaluate . fmap (\(Rule (_, f)) -> f)

evaluate :: Tree [[VarT t]] -> Maybe [[t]]
evaluate (Node f ts) = do
  tups <- mapM evaluate ts
  let lookUp (Var i j) = listToMaybe (drop i tups) >>= (listToMaybe . drop j)
      lookUp (T t)     = Just [t]
  mapM (fmap concat . mapM lookUp) f


prettyPrintRule :: (Show nt, Show t) => Rule nt t -> String
prettyPrintRule (Rule ((a, bs), f))
  = show a ++ " → " ++ prettyPrintComposition f ++ " (" ++ intercalate ", " (map show bs) ++ ")"

prettyPrintComposition :: Show t => [[VarT t]] -> String
prettyPrintComposition = show . map (unwords . map g)
  where g (Var i j) = "x[" ++ show i ++ ":" ++ show j ++ "]"
        g (T t)     = show t

exampleCompositions :: [[[VarT Char]]]
exampleCompositions = [ [[Var 0 0, Var 1 0, Var 0 1, Var 1 1]]
                      , [[T 'a', Var 0 0], [T 'c', Var 0 1]]
                      , [[T 'b', Var 0 0], [T 'd', Var 0 1]]
                      , [[], []]
                      ]

exampleRules :: [Rule Int Char]
exampleRules = [ Rule ((0, [1,2]), exampleCompositions !! 0)
               , Rule ((1, [1])  , exampleCompositions !! 1)
               , Rule ((1, [])   , exampleCompositions !! 3)
               , Rule ((2, [2])  , exampleCompositions !! 2)
               , Rule ((2, [])   , exampleCompositions !! 3)
               ]

exampleDerivation :: Tree (Rule Int Char)
exampleDerivation = Node (exampleRules !! 0)
                    [ Node (exampleRules !! 1)
                      [ Node (exampleRules !! 1)
                        [ Node (exampleRules !! 2) [] ]
                      ]
                    , Node (exampleRules !! 3)
                      [ Node (exampleRules !! 4) [] ]
                    ]

exampleWeights :: [Double]
exampleWeights = [1, 0.6, 0.4, 0.3, 0.7]

examplePMCFG :: PMCFG Int Char
examplePMCFG = fromRules [0] exampleRules

exampleWPMCFG :: WPMCFG Int Double Char
exampleWPMCFG = fromWeightedRules [0] $ zip exampleRules exampleWeights
