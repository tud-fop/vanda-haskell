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
    ) where

import Vanda.Grammar.PMCFG.WeightedDeductiveSolver (solve, WeightedDeductiveSolver(..), DeductiveRule(..), Cost, cost)
import Vanda.Grammar.PMCFG
import Data.Tree (Tree)
import Data.Maybe (mapMaybe)


-- | Item of naive parsing deduction. 
-- Tuple of non-terminal, spanning range vector, derivation tree.
data DeductiveItem nt t = Item nt Rangevector (Derivation nt t) deriving (Eq, Ord, Show)

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
weightedParse (WPMCFG s rs) word = map (\ (Item _ _ (Derivation t)) -> t) 
                                    $ filter (\ (Item a rho _) -> (a `elem` s) && (rho == [entire word])) 
                                    $ solve 
                                    $ WeightedDeductiveSolver (makeWeightedRules word rs) 100


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
    itemFilter a (Item a' _ _)= a == a'
    
    application :: Rule nt t -> [InstantiatedFunction] -> [DeductiveItem nt t] -> [DeductiveItem nt t]
    application r@(Rule ((a', _), _)) fs is = [ Item a' rv $ node r ts 
                                              | rv <- mapMaybe (insert rvs) fs
                                              , isNonOverlapping rv 
                                              ]
      where
        (rvs, ts) = foldr (\ (Item _ rv t) (rvs, ts) -> (rv:rvs, t:ts)) ([], []) is

        insert :: [Rangevector] -> InstantiatedFunction -> Maybe Rangevector
        insert rvs = mapM ((>>= toRange) . concVarRange . map (insert' rvs))
          where
            insert' :: [Rangevector] -> VarT Range -> VarT Range
            insert' rvs' (Var x y)  = T $ rvs' !! x !! y
            insert' _ r = r
