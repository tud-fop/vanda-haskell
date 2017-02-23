-----------------------------------------------------------------------------
-- |
-- Module      :  DeductiveSolver
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
-- This module supplies the definition of a weighted deduction system and a 
-- function to find all generated items ordered by minimal costs.
--
-- Weights are considered a monoid, two example wrappers are the @Cost@ and
-- @Probabilistic@ newtypes. Whereas the Cost monoid uses addition to add up
-- all weights during deduction, the Probabilistic monoid assumes floating
-- point values in range [0,1] to calculate the probability of an item as
-- product of all antecedent items and the rule with it as consequence.
-- 
--
-----------------------------------------------------------------------------
module Vanda.Grammar.PMCFG.Wip.DeductiveSolverv2
  where
  -- ( DeductiveSolver(DeductiveSolver)
  -- , DeductiveRule(DeductiveRule)
  -- , solve
  -- -- * Weights
  -- , Dividable(divide)
  -- , Probabilistic( )
  -- , probabilistic
  -- , Cost( )
  -- , cost
  -- ) where

import qualified Data.PQueue.Prio.Max as Q
import qualified Data.HashMap.Lazy    as Map
import qualified Data.HashSet         as Set

import Data.Hashable
import Control.Monad.State (State, evalState, get, put)
import Data.Tuple (swap)


data DeductiveSolver it wt a = DeductiveSolver a (a -> it -> a) [DeductiveRule it wt a] Int
data DeductiveRule it wt a = DeductiveRule Int (a -> it -> [[it]]) (a -> [it] -> [(it, wt)])


updateGroup :: (Hashable b, Eq b) 
            => b 
            -> a
            -> Map.HashMap b [a] 
            -> Map.HashMap b [a]
updateGroup b a m = Map.alter addToList b m
  where
    addToList Nothing = Just [a]
    addToList (Just as) = Just (a:as)

updateGroups :: (Hashable b, Eq b) 
             => [b] 
             -> a
             -> Map.HashMap b [a] 
             -> Map.HashMap b [a]
updateGroups [] _ m = m
updateGroups (b:bs) a m = updateGroups bs a $ updateGroup b a m


solve :: (Ord wt, Eq it, Hashable it)
      => DeductiveSolver it wt a
      -> [it]
solve (DeductiveSolver container update rs b) = evalState (deductiveIteration rs' update b) 
                                                          (Q.fromList $ map swap inits, Set.fromList (map fst inits), container)
  where
    inits = rs >>= applyWithoutAntecedents
    
    applyWithoutAntecedents (DeductiveRule 0 _ app) = app container []
    applyWithoutAntecedents _ = []

    rs' = filter (\ (DeductiveRule antecedents _ _) -> antecedents > 0) rs


deductiveIteration  :: (Ord wt, Eq it, Hashable it)
                    => [DeductiveRule it wt a]
                    -> (a -> it -> a)
                    -> Int
                    -> State (Q.MaxPQueue wt it, Set.HashSet it, a) [it]
deductiveIteration rs update beam = do (agenda, olds, container) <- get
                                       if Q.null agenda
                                          then return []
                                          else do let ((_, item), agenda') = Q.deleteFindMax agenda
                                                      container' = update container item
                                                      newitems = filter (not . (`Set.member` olds) . fst) $ deductiveStep item container' rs
                                                      olds' = olds `Set.union` (Set.fromList $ map fst newitems)
                                                      agenda'' = Q.fromList $ Q.take beam $ agenda' `Q.union` Q.fromList (map swap newitems)
                                                  put (agenda'', olds', container')
                                                  is <- deductiveIteration rs update beam
                                                  return (item:is)


deductiveStep :: it 
              -> a
              -> [DeductiveRule it wt a]
              -> [(it, wt)]
deductiveStep item container rs = rs >>= ruleApplication item container


ruleApplication :: it 
                -> a
                -> DeductiveRule it wt a
                -> [(it, wt)]
ruleApplication item container (DeductiveRule _ gets app) = gets container item >>= app container