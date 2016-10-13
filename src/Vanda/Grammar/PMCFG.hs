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
  , prettyPrintWPMCFG
  -- * examples
  , examplePMCFG
  , exampleWPMCFG
  , exampleDerivation
  , exampleRules
  , exampleCompositions
  ) where

import Control.Arrow (first)
import qualified Control.Error
import qualified Data.Binary as B
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Tree

errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.Grammar.PMCFG"

data VarT t = T t | Var !Int !Int deriving (Eq, Ord, Show)

instance Functor VarT where
  fmap f (T t)     = T (f t)
  fmap _ (Var i j) = Var i j

instance B.Binary t => B.Binary (VarT t) where
  get = do x <- B.getWord8
           case x of
             0 -> do t <- B.get
                     return $ T t
             1 -> do i <- B.get
                     j <- B.get
                     return $ Var i j
             _ -> errorHere "get" $ "unexpected word" ++ show x
  put (T t) = B.putWord8 0 >> B.put t
  put (Var i j) = B.putWord8 1 >> B.put i >> B.put j


-- | 'Rule' ((A, [A₁, …, Aₖ]), f) ~ A → f(A₁, …, Aₖ).
newtype Rule nt t = Rule ((nt, [nt]), [[VarT t]]) deriving (Eq, Ord, Show)

instance Functor (Rule nt) where
  fmap f (Rule (nts, varts)) = Rule (nts, map (map (fmap f)) varts)

instance (B.Binary nt, B.Binary t) => B.Binary (Rule nt t) where
  get = B.get
  put (Rule x) = B.put x


data PMCFG nt t = PMCFG [nt] [Rule nt t] deriving Show

instance Functor (PMCFG nt) where
  fmap f (PMCFG ints rs) = PMCFG ints $ map (fmap f) rs

instance (B.Binary nt, B.Binary t) => B.Binary (PMCFG nt t) where
  get = do is <- B.get
           rs <- B.get
           return $ PMCFG is rs
  put (PMCFG is rs) = B.put is >> B.put rs

fromRules
  :: [nt] -- ^ initial non-terminals
  -> [Rule nt t] -- ^ rules
  -> PMCFG nt t
fromRules = PMCFG


data WPMCFG nt w t = WPMCFG [nt] [(Rule nt t, w)] deriving Show

instance Functor (WPMCFG nt w) where
  fmap f (WPMCFG ints rs) = WPMCFG ints $ map (first (fmap f)) rs

instance (B.Binary nt, B.Binary w, B.Binary t) => B.Binary (WPMCFG nt w t) where
  get = do is <- B.get
           wrs <- B.get
           return $ WPMCFG is wrs
  put (WPMCFG is wrs) = B.put is >> B.put wrs

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

prettyPrintWeightedRule :: (Show nt, Show w, Show t) => (Rule nt t, w) -> String
prettyPrintWeightedRule (r, w) = prettyPrintRule r ++ "\t# " ++ show w

prettyPrintComposition :: Show t => [[VarT t]] -> String
prettyPrintComposition = show . map (unwords . map g)
  where g (Var i j) = "x[" ++ show i ++ ":" ++ show j ++ "]"
        g (T t)     = show t

prettyPrintWPMCFG :: (Show nt, Show w, Show t) => WPMCFG nt w t -> String
prettyPrintWPMCFG (WPMCFG is rs)
  = "initial: " ++ show is ++ "\n\n"
    ++ unlines (map prettyPrintWeightedRule rs)

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
