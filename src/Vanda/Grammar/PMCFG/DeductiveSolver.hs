----------------------------------------------------
-- | DeductiveSolver
--
-----------------------------------------------------
module Vanda.Grammar.PMCFG.DeductiveSolver
  ( DeductiveSolver(DeductiveSolver)
  , DeductiveRule(DeductiveRule)
  , solve
  ) where

import Data.Set (Set, member, union, fromList, toList)
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe)
import Control.Monad.State (State, evalState, get, put)

-- | An instance for a deduction system.
-- Consists of a list of rules and a filter function that is applied in each step and on the result.
data DeductiveSolver it = DeductiveSolver [DeductiveRule it] ([it] -> [it])
-- | A rule of a deduction system.
-- Consists of a list of filters that are True for possible antecedents and an application function. 
data DeductiveRule it = DeductiveRule [it -> Bool] ([it] -> Maybe it)

instance (Show it) => Show (DeductiveRule it) where
  showsPrec _ (DeductiveRule filters _) = (++) ("Instance of DeductiveRule with " ++ show (length filters) ++ " anticidents" )

instance (Show it) => Show (DeductiveSolver it) where
  showsPrec _ (DeductiveSolver rs _) = (++) ("Instance of DeductiveSolver with:\n" ++ unlines (map show rs))

-- | Top-level function that solves a deduction system.
solve :: (Eq it, Ord it, Hashable it)
      => DeductiveSolver it           -- ^ the solver instance
      -> [it]                         -- ^ all (filtered) items, that were deducted
solve s@(DeductiveSolver rs f) = evalState (deductiveIteration s) (initSet, initSet)
  where
    initSet = fromList $ f $ deductiveStep [] rs

-- | Recursion that solves the deduction system.
-- Applies all valid combination of rules to all items as antecedents until there are no new items.
deductiveIteration  :: (Eq it, Ord it, Hashable it)
                    => DeductiveSolver it           -- ^ solver instance for rules and filter
                    -> State (Set it, Set it) [it]          -- ^ state to store previously deducted items, returns list of all items
deductiveIteration s@(DeductiveSolver rs f) = do  (c, newItems') <- get
                                                  let newItems = fromList $ filter (not . flip member c) $ deductiveStep (toList newItems') (toList c) rs
                                                  if null newItems
                                                  then return $ f $ toList c
                                                  else do put (fromList $ f $ toList (c `union` newItems), newItems)
                                                          deductiveIteration s

-- | A step to apply all rules for all possible antecedents.
deductiveStep :: [it] -> [it] -> [DeductiveRule it] -> [it]
deductiveStep forcedItems is rs = rs >>= ruleApplication forcedItems is

-- | Application of one rule to all possible antecedents.
ruleApplication :: [it] -> [it] -> DeductiveRule it -> [it]
ruleApplication forcedItems is (DeductiveRule fs app) = mapMaybe app candidates
  where
    candidates = filter (any . map (`elem` forcedItems)) $ mapM (`filter` is) fs

