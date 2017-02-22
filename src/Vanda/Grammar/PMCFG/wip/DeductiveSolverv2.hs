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

import qualified Data.PQueue.Prio.Max as Q
import qualified Data.HashMap.Lazy    as Map
import Data.Hashable
import Control.Monad.State (State, evalState, get, put)
import Data.Tuple (swap)
import Data.Monoid ((<>))
import Numeric.Log (Log(Exp), Precise)


data DeductiveSolver it wt a = DeductiveSolver a (a -> it -> a) [DeductiveRule it wt a] Int
data DeductiveRule it wt a = DeductiveRule Int (a -> it -> [[it]]) (a -> [it] -> [it, wt])


solve :: (Ord wt, Monoid wt, Eq it, Hashable it)
      => DeductiveSolver it wt a
      -> [it]
solve (DeductiveSolver container update rs b) = evalState (deductiveIteration rs' update b) 
                                                          (Q.fromList $ map swap inits, Map.fromList inits, initcontainer)
  where
    inits = rs >>= applyWithoutAntecedents
    initcontainer = update container $ fst $ unzip inits
    
    applyWithoutAntecedents (DeductiveRule 0 _ app w) = zip (app []) (repeat w)
    applyWithoutAntecedents _ = []

    rs' = filter (\ (DeductiveRule antecedents _ _ _) -> antecedents > 0) rs


deductiveIteration  :: (Ord wt, Eq it, Hashable it, Monoid wt)
                    => [DeductiveRule it wt a]
                    -> (a -> [it] -> a)
                    -> Int
                    -> State (Q.MaxPQueue wt it, Map.HashMap it wt, a) [it]
deductiveIteration rs update beam = do (current, weightmap, container) <- get
                                       if Q.null current
                                          then return []
                                          else do let ((_, item), c') = Q.deleteFindMax current
                                                      newitems = filter (not . (`Map.member` weightmap) . fst) $ deductiveStep item container weightmap rs
                                                      a'' = weightmap `Map.union` Map.fromList newitems
                                                      c'' = Q.fromList $ Q.take beam $ c' `Q.union` Q.fromList (map swap newitems)
                                                      uc'' = update container $ map fst newitems
                                                  put (c'', a'', uc'')
                                                  is <- deductiveIteration rs update beam
                                                  return (item:is)


deductiveStep :: (Eq it, Hashable it, Monoid wt)
              => it 
              -> a
              -> Map.HashMap it wt
              -> [DeductiveRule it wt a]
              -> [(it, wt)]
deductiveStep item container weights rs = rs >>= ruleApplication item container weights


ruleApplication :: (Eq it, Hashable it, Monoid wt)
                => it 
                -> a
                -> Map.HashMap it wt
                -> DeductiveRule it wt a
                -> [(it, wt)]
ruleApplication item container weights (DeductiveRule _ gets app w) = [ (cs, mconcat (map (weights Map.!) as) <> w)
                                                                       | as <- gets container item
                                                                       , cs <- app as
                                                                       ]