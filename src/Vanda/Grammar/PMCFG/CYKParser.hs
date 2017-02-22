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
-- 'Vanda.Grammar.PMCFG.DeductiveSolver.Dividable'.
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

import Vanda.Grammar.PMCFG.DeductiveSolver
import Vanda.Grammar.PMCFG.Range
import Vanda.Grammar.PMCFG

import Data.Hashable (Hashable(hashWithSalt))
import qualified Data.HashMap.Lazy as Map
import Data.Tree (Tree)
import Data.Maybe (mapMaybe)


-- | Item of naive parsing deduction. 
-- Tuple of non-terminal, spanning range vector, derivation tree.
data DeductiveItem nt t = Item nt Rangevector (Derivation nt t) deriving (Eq, Ord, Show)

instance (Hashable nt, Hashable t) => Hashable (DeductiveItem nt t) where
  salt `hashWithSalt` (Item nt rv d) = salt `hashWithSalt` nt `hashWithSalt` rv `hashWithSalt` d


-- | Top-level function to parse a word using a grammar.
parse :: (Eq t, Eq nt, Hashable nt, Hashable t)
      => PMCFG nt t                               -- ^ the grammar
      -> Int
      -> [t]                                      -- ^ the word
      -> [Tree (Rule nt t)]                       -- ^ list of derivation trees 
parse (PMCFG s rules) = weightedParse $ WPMCFG s $ zip rules $ repeat (cost 1 :: Cost Int)


-- | Top-level function to parse a word using a weighted grammar.
weightedParse :: (Eq t, Eq nt, Hashable t, Hashable nt, Monoid wt, Ord wt)
              => WPMCFG nt wt t             -- ^ weighted grammar
              -> Int
              -> [t]                        -- ^ word
              -> [Tree (Rule nt t)]   -- ^ parse trees and resulting weights
weightedParse (WPMCFG s rs) bw word = map (\ (Item _ _ (Derivation t)) -> t) 
                                      $ filter (\ (Item a rho _) -> (a `elem` s) && (rho == singleton (entire word))) 
                                      $ solve 
                                      $ DeductiveSolver Map.empty update (map (deductiveRules word) rs) bw
  where
    update :: (Eq nt0, Hashable nt0)
           => Map.HashMap nt0 [(DeductiveItem nt0 t0)]
           -> [(DeductiveItem nt0 t0)]
           -> Map.HashMap nt0 [(DeductiveItem nt0 t0)]
    update m items = updateGroupsWith (\ (Item a _ _) -> a) items m


deductiveRules :: (Hashable nt, Eq nt, Eq t) 
               => [t] 
               -> (Rule nt t, wt)
               -> DeductiveRule (DeductiveItem nt t) wt (Map.HashMap nt [(DeductiveItem nt t)])
deductiveRules word (r@(Rule ((a, as), f)), w) = DeductiveRule (length as) gets complete w
  where
    -- gets :: (Map.HashMap nt [(DeductiveItem nt t)])
    --      -> DeductiveItem nt t
    --      -> [[DeductiveItem nt t]]
    gets m i@(Item a' _ _) =  if a' `elem` as
                              then filter (any (== i)) $ mapM (\ a'' -> Map.lookupDefault [] a'' m) as
                              else []
  
    complete antecedents = [ Item a rv $ node r ds
                           | (rvs, ds) <- return $ foldr (\ (Item _ rv t) (rvs', ts') -> (rv:rvs', t:ts')) ([], []) antecedents 
                           , rv <- mapMaybe (insert rvs) $ instantiate word f
                           ]

    insert :: [Rangevector] -> InstantiatedFunction -> Maybe Rangevector
    insert rvs' = (>>= fromList) . mapM ((>>= toRange) . concVarRange . map (insert' rvs'))
      
    insert' :: [Rangevector] -> VarT Range -> VarT Range
    insert' rvs' (Var x y)  = T $ rvs' !! x ! y
    insert' _ r' = r'
