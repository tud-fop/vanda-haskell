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
module Vanda.Grammar.PMCFG.DeductiveSolver
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

import qualified Data.LimitedQueue as Q
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet      as Set

import Data.Hashable
import Control.Monad.State (State, evalState, execState, get, put)
import Data.Tuple (swap)
import Data.Semiring

-- | Instance of a deduction system. 
-- Consisits of a list of Rules, an instance-specific container with update function and a beam width to limit memory expense.
data DeductiveSolver it wt a = DeductiveSolver a (a -> it -> a) [DeductiveRule it wt a] Int
-- | A rule of a deduction system.
-- Consists of two functions: one prepares a list of antecedent combinations using the container of that instance, and one for the rule application itself.
data DeductiveRule it wt a = DeductiveRule Int (a -> it -> [[it]]) (a -> [it] -> [(it, wt)])


-- | Adds an item to a list of items.
updateGroup :: (Hashable b, Eq b) 
            => b 
            -> a
            -> Map.HashMap b [a] 
            -> Map.HashMap b [a]
updateGroup b a = Map.alter addToList b
  where
    addToList Nothing = Just [a]
    addToList (Just as) = Just (a:as)

-- | Adds an item to many lists of items.
updateGroups :: (Hashable b, Eq b) 
             => [b] 
             -> a
             -> Map.HashMap b [a] 
             -> Map.HashMap b [a]
updateGroups [] _ m = m
updateGroups (b:bs) a m = updateGroups bs a $ updateGroup b a m


chart :: (Ord wt, Eq it, Hashable it, Semiring wt)
      => a -> (a -> it -> (a, Bool)) -> [DeductiveRule it wt a] -> Int
      -> a
chart container update rs b = snd $ execState (chartIteration rs' update) 
                                              (Q.fromList b inits, container)
  where
    inits = rs >>= applyWithoutAntecedents
    
    applyWithoutAntecedents (DeductiveRule 0 _ app) = app container []
    applyWithoutAntecedents _ = []

    rs' = filter (\ (DeductiveRule antecedents _ _) -> antecedents > 0) rs

chartIteration :: (Ord wt, Semiring wt, Eq it, Hashable it)
               => [DeductiveRule it wt a]
               -> (a -> it -> (a, Bool))
               -> State (Q.Queue it wt, a) ()
chartIteration rs update = do (agenda, container) <- get
                              if Q.null agenda
                                 then return ()
                                 else do let (agenda', item) = Q.deq agenda
                                             (container', newConsequence) = update container item
                                         if newConsequence
                                            then do let newitems = filter ((/= zero) . snd) $ deductiveStep item container' rs
                                                        agenda'' = agenda' `Q.enqList` newitems
                                                    put (agenda'', container')
                                                    chartIteration rs update
                                            else do put (agenda', container')
                                                    chartIteration rs update

solve :: (Ord wt, Eq it, Hashable it)
      => DeductiveSolver it wt a
      -> [it]
solve (DeductiveSolver container update rs b) = evalState (deductiveIteration rs' update) 
                                                          (Q.fromList b inits, container)
  where
    inits = rs >>= applyWithoutAntecedents
    
    applyWithoutAntecedents (DeductiveRule 0 _ app) = app container []
    applyWithoutAntecedents _ = []

    rs' = filter (\ (DeductiveRule antecedents _ _) -> antecedents > 0) rs


deductiveIteration  :: (Ord wt, Eq it, Hashable it)
                    => [DeductiveRule it wt a]
                    -> (a -> it -> a)
                    -> State (Q.Queue it wt, a) [it]
deductiveIteration rs update = do (agenda, container) <- get
                                  if Q.null agenda
                                    then return []
                                    else do let (agenda', item) = Q.deq agenda
                                                container' = update container item
                                                agenda'' = agenda' `Q.enqList` deductiveStep item container' rs
                                            put (agenda'', container')
                                            fmap (item :) (deductiveIteration rs update)


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
