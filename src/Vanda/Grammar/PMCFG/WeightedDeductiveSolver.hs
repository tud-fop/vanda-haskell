----------------------------------------------------
-- | DeductiveSolver
--
-----------------------------------------------------
module Vanda.Grammar.PMCFG.WeightedDeductiveSolver
  ( WeightedDeductiveSolver(WeightedDeductiveSolver)
  , solve
  ) where

import Data.PQueue.Prio.Max (MaxPQueue, fromList, toDescList, elems, union)
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe)
import Control.Monad.State (State, evalState, get, put)
import Vanda.Grammar.PMCFG.DeductiveSolver (DeductiveRule(DeductiveRule))

-- | An instance for a deduction system.
-- Consists of a list of rules and a filter function that is applied in each step and on the result.
data WeightedDeductiveSolver it wt = WeightedDeductiveSolver [(DeductiveRule it, wt)] ([(wt, it)] -> [(wt, it)])

instance (Show it, Show wt) => Show (WeightedDeductiveSolver wt it) where
  show (WeightedDeductiveSolver rs _) = "Instance of DeductiveSolver with:\n" ++ unlines (map show rs)

-- | Top-level function that solves a deduction system.
solve :: (Eq it, Ord it, Hashable it, Ord wt, Num wt)
      => WeightedDeductiveSolver it wt            -- ^ the solver instance
      -> [it]                                     -- ^ all (filtered) items, that were deducted
solve s@(WeightedDeductiveSolver rs f) = evalState (deductiveIteration s) (initQ, snd $ unzip initList)
  where
    initList = f $ deductiveStep [] [] rs
    initQ = fromList initList

-- | Recursion that solves the deduction system.
-- Applies all valid combination of rules to all items as antecedents until there are no new items.
deductiveIteration  :: (Eq it, Ord it, Hashable it, Ord wt, Num wt)
                    => WeightedDeductiveSolver it wt                  -- ^ solver instance for rules and filter
                    -> State (MaxPQueue wt it, [it]) [it]          -- ^ state to store previously deducted items, returns list of all items
deductiveIteration s@(WeightedDeductiveSolver rs f) = do  (c, newItems') <- get
                                                          let newItems = filter (not . (`elem` elems c) . snd) $ deductiveStep newItems' (toDescList c) rs
                                                          if null newItems
                                                            then return $ map snd $ f $ toDescList c
                                                            else do put (fromList $ f $ toDescList (c `union` fromList newItems), snd $ unzip newItems)
                                                                    deductiveIteration s

-- | A step to apply all rules for all possible antecedents.
deductiveStep :: (Num wt, Eq it) => [it] -> [(wt, it)] -> [(DeductiveRule it, wt)] -> [(wt, it)]
deductiveStep forcedItems is rs = rs >>= ruleApplication forcedItems is

-- | Application of one rule to all possible antecedents.
ruleApplication :: (Num wt, Eq it) => [it] -> [(wt, it)] -> (DeductiveRule it, wt) -> [(wt, it)]
ruleApplication forcedItems is (DeductiveRule fs app, w) = mapMaybe ruleApplication' candidates 
  where
    candidates =  if null fs
                  then [[]]
                  else filter (any ((`elem` forcedItems) . snd)) $ mapM ((`filter` is) . (. snd)) fs
    --ruleApplication' :: (Num wt, Eq it) =>[(wt, it)] -> Maybe (wt, it)
    ruleApplication' is' = case  app antecedents of
                                  Just c -> Just (w * product weights, c)
                                  Nothing -> Nothing
      where
        (weights, antecedents) = unzip is'