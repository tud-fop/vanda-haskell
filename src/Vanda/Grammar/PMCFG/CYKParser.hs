-----------------------------------------------------------------------------
-- |
-- Module      :  CYKParser
-- Copyright   :  (c) Thomas Ruprecht 2017
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  thomas.ruprecht@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- This module provides two functions for parsing words using the 
-- CYK-parsing algorithm by Seki et al.
--
-- 'weightedParse' uses a weighted PMCFG to find a list of all possible
-- derivation trees ordered by minimal cost / maximum probability. The
-- rules' weights need to be instances of 'Monoid' and 
-- 'Vanda.Grammar.PMCFG.WeightedDeductiveSolver.Dividable'.
-- 'parse' uses an unweighted grammar to find a list of derivation trees
-- ordered by least rule applications.
--
-- The algorithm uses a deduction system to parse the word. Items of this 
-- deduction system are a tuple of a non-terminal, a vector of ranges in
-- the word we want to parse and a tree of applied derivation rules. Thus
-- every item represents a (partial) derivation step that generated parts
-- of the word by their ranges in it. There is exactly one deduction
-- rule for every rule of the grammar that uses already generated items
-- of all non-terminals in the rule and produces an item of the rule's lhs
-- non-terminal. To validate the generated sub-word of this step, the ranges
-- if all non-terminal and terminal symbols in the composition function of
-- the rule are replaced by the possible ranges in the word and concatenated,
-- s.t. they have to fit.
--
--
--
-----------------------------------------------------------------------------
module Vanda.Grammar.PMCFG.CYKParser
    ( parse
    , weightedParse
    -- * derivation trees
    , Derivation(Derivation)
    , node
    -- * ranges
    , Range(Epsilon)
    , singletons
    , entire
    , safeConc
    , Rangevector
    , prettyPrintRangevector
    , isNonOverlapping
    -- * ranges with variables
    , Function
    , InstantiatedFunction
    , instantiate
    , concVarRange
    , toRange
    , prettyPrintInstantiatedFunction
    ) where

import Vanda.Grammar.PMCFG.WeightedDeductiveSolver (solve, WeightedDeductiveSolver(..), DeductiveRule(..), Cost, cost)
import Vanda.Grammar.PMCFG
import Data.Tree (Tree(Node))
import Data.Hashable (Hashable, hashWithSalt)
import Data.List (elemIndices)
import Data.Maybe (mapMaybe)

-- | A range (i, j) in a word w.
-- Consider i \< j for i \>= 0 and j \<= |w|
-- and 'Epsilon' substitutes all (i, i) for 0 <= i <= |w|.
data Range = Range (Int, Int)
              | Epsilon -- ^ empty range of ε in w
              deriving (Show, Eq, Ord)

-- | A singleton range is a range of a single character in a word.
singletons :: (Eq t) => t -> [t] -> [Range]
singletons c w = map singleton $ c `elemIndices` w
  where singleton i = Range (i, i+1)

-- | Full range of a word.
entire :: [t] -> Range
entire [] = Epsilon
entire xs = Range (0, length xs)

-- | Concatenates two ranges. Fails if neighboring ranges do not fit.
safeConc :: Range -> Range -> Maybe Range
safeConc Epsilon r = Just r
safeConc r Epsilon = Just r
safeConc (Range (i,j)) (Range (k,l))
  | j == k = Just $ Range (i, l)
  | otherwise = Nothing

-- | A range vector is a non-overlapping sequence of ranges.
type Rangevector = [Range]

-- | Item of naive parsing deduction. 
-- Tuple of non-terminal, spanning range vector, derivation tree.
type DeductiveItem nt t = (nt, Rangevector, Derivation nt t)

-- | A composition function.
type Function t = [[VarT t]]

-- | An instantiated composition function. 
-- Terminals are substituted by their corresponding ranges in the word.
type InstantiatedFunction = Function Range

-- | A derivation tree.
newtype Derivation nt t = Derivation (Tree (Rule nt t)) deriving (Eq, Show)

-- | Wraps Node constructor of Tree for easy use of Derivation.
node :: Rule nt t -> [Derivation nt t] -> Derivation nt t
node r ds = Derivation $ Node r ts
  where
    ts = map (\ (Derivation t) -> t) ds

instance (Ord nt, Ord t) => Ord (Derivation nt t) where
  compare (Derivation (Node x xs)) (Derivation (Node y ys))
    | x < y = LT
    | x > y = GT
    | otherwise = map Derivation xs `compare` map Derivation ys

instance (Hashable t, Hashable nt) => Hashable (Derivation nt t) where
  salt `hashWithSalt` (Derivation (Node x xs)) = salt `hashWithSalt` x `hashWithSalt` map Derivation xs

-- | Top-level function to parse a word using a grammar.
parse :: (Ord t, Ord nt)
      => PMCFG nt t                               -- ^ the grammar
      -> [t]                                      -- ^ the word
      -> [Tree (Rule nt t)]                       -- ^ list of derivation trees 
parse (PMCFG s rules) = weightedParse $ WPMCFG s $ zip rules $ repeat (cost 1 :: Cost Int)

-- | Top-level function to parse a word using a weighted grammar.
weightedParse :: (Ord t, Ord nt, Monoid wt, Ord wt)
              => WPMCFG nt wt t             -- ^ weighted grammar
              -> [t]                        -- ^ word
              -> [Tree (Rule nt t)]   -- ^ parse trees and resulting weights
weightedParse (WPMCFG s rs) word = map (\ (_, _, Derivation t) -> t) 
                                    $ filter (\ (a, rho, _) -> (a `elem` s) && (rho == [entire word])) 
                                    $ solve 
                                    $ WeightedDeductiveSolver (makeWeightedRules word rs) id

-- | Constructs deduction rules using a weighted grammar.
-- Weights are stored in antecedent items and application functions of rules.
makeWeightedRules :: (Eq nt, Eq t) 
                  => [t]                                      -- ^ word 
                  -> [(Rule nt t, wt)]                        -- ^ weighted grammar rules
                  -> [(DeductiveRule (DeductiveItem nt t), wt)] -- ^ weighted deduction rules 
makeWeightedRules w rs =  [ (DeductiveRule (map itemFilter as) (application r $ instantiate w f), weight)
                          | (r@(Rule ((_, as), f)), weight) <- rs
                          ]
  where
    itemFilter :: (Eq nt) => nt -> DeductiveItem nt t -> Bool
    itemFilter a (a', _, _)= a == a'
    
    application :: Rule nt t -> [InstantiatedFunction] -> [DeductiveItem nt t] -> [DeductiveItem nt t]
    application r@(Rule ((a', _), _)) fs is = [ (a', rv, node r ts) 
                                              | rv <- mapMaybe (insert rvs) fs
                                              , isNonOverlapping rv 
                                              ]
      where
        (rvs, ts) = case  unzip3 is of
                          (_, rvs', ts') -> (rvs', ts')


-- | Returns a list of all possible instances of a composition function for a word.
instantiate :: (Eq t)
            => [t]                    -- ^ the word
            -> Function t             -- ^ the function to instantiate
            -> [InstantiatedFunction] -- ^ all possible combinations of instances with valid concatenated ranges
instantiate w' = mapM (mapMaybe concVarRange . sequence . instantiateComponent w')
  where
    instantiateComponent :: (Eq t) => [t] -> [VarT t] -> [[VarT Range]]
    instantiateComponent _ []         = [[ T Epsilon ]]
    instantiateComponent w fs         = map (instantiateCharacter w) fs
    instantiateCharacter :: (Eq t) => [t] -> VarT t -> [VarT Range]
    instantiateCharacter _ (Var i j)  = [Var i j]
    instantiateCharacter w (T c)      = map T $ singletons c w

-- | Checks for overlapping components in a range vector.
isNonOverlapping :: Rangevector -> Bool
isNonOverlapping = isNonOverlapping' []
  where
    isNonOverlapping' :: [(Int, Int)] -> Rangevector -> Bool
    isNonOverlapping' _ [] = True
    isNonOverlapping' cache (Range (i,j) : rs)
      | any (\ (k,l) -> (j > k && j < l) || (i > k && i < l)) cache = False
      | otherwise = isNonOverlapping' ((i, j):cache) rs
    isNonOverlapping' cache (Epsilon : rs) = isNonOverlapping' cache rs

-- | Replaces variables with corresponding ranges. Fails if ranges do not fit in their context.
insert :: [Rangevector] -> InstantiatedFunction -> Maybe Rangevector
insert rvs = mapM ((>>= toRange) . concVarRange . map (insert' rvs))
  where
    insert' :: [Rangevector] -> VarT Range -> VarT Range
    insert' rvs' (Var x y)  = T $ rvs' !! x !! y
    insert' _ r = r

-- | Tries to concatenate ranges in an instantiated function component.
-- Variables are left as they are, so they result does not need to be one range.
concVarRange :: [VarT Range] -> Maybe [VarT Range]
concVarRange (T r1 : T r2 : is) = case safeConc r1 r2 of
                                       Just r -> concVarRange $ T r : is
                                       Nothing -> Nothing
concVarRange (T Epsilon : (i : is)) = concVarRange $ i:is
concVarRange (i : (T Epsilon : is)) = concVarRange $ i:is
concVarRange (i:is) = (i:) <$> concVarRange is
concVarRange [] = Just []

-- | Tries to unpack a concatenated range vector.
toRange :: [VarT Range] -> Maybe Range
toRange [T r] = Just r
toRange _ = Nothing

prettyPrintInstantiatedFunction :: InstantiatedFunction -> String
prettyPrintInstantiatedFunction fs = show $ map go fs
  where
    go :: [VarT Range] -> String
    go [] = ""
    go (T (Range (i,j)) : f) = "(" ++ show i ++ "," ++ show j ++ ")" ++ go f
    go (T Epsilon : f) = "()" ++ go f
    go (Var i j : f) = "x[" ++ show i ++ ":" ++ show j ++ "]" ++ go f

prettyPrintRangevector :: Rangevector -> String
prettyPrintRangevector rs = "<" ++ unwords (map go rs) ++  ">"
  where
    go (Range r) = show r
    go Epsilon = "()"