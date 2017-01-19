----------------------------------------------------
-- | DeductiveSolver
--
-----------------------------------------------------
module Vanda.Grammar.PMCFG.WeightedDeductiveSolver
  ( WeightedDeductiveSolver(DeductiveSolver)
  , solve
  ) where

import Data.PQueue.Prio.Max (MaxPQueue, fromList, toDescList, elems, union)
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe)
import Control.Monad.State (State, evalState, get, put)
import Vanda.Grammar.PMCFG.DeductiveSolver (DeductiveRule(DeductiveRule))

-- | An instance for a deduction system.
-- Consists of a list of rules and a filter function that is applied in each step and on the result.
data WeightedDeductiveSolver it wt = WeightedDeductiveSolver [(DeductiveRule it, wt)] ([(it, wt)] -> [(it, wt)])

instance (Show it, Show wt) => Show (WeightedDeductiveSolver wt it) where
  show (DeductiveSolver rs _) = "Instance of DeductiveSolver with:\n" ++ unlines (map show rs)

-- | Top-level function that solves a deduction system.
solve :: (Eq it, Ord it, Hashable it, Ord wt)
      => WeightedDeductiveSolver it wt            -- ^ the solver instance
      -> [it]                                     -- ^ all (filtered) items, that were deducted
solve s@(DeductiveSolver rs f) = evalState (deductiveIteration s) (initQ, initSet)
  where
    initList = fst $ unzip $ f $ deductiveStep [] rs
    initQ = fromList initList

-- | Recursion that solves the deduction system.
-- Applies all valid combination of rules to all items as antecedents until there are no new items.
deductiveIteration  :: (Eq it, Ord it, Hashable it, Ord wt)
                    => WeightedDeductiveSolver it wt                  -- ^ solver instance for rules and filter
                    -> State (MaxPQueue it wt, [it]) [it]          -- ^ state to store previously deducted items, returns list of all items
deductiveIteration s@(DeductiveSolver rs f) = do  (c, newItems') <- get
                                                  let newItems = filter (not . `elem` (elems c) . fst) $ deductiveStep newItems' (toDescList c) rs
                                                  if null newItems
                                                  then return $ f $ toDescList c
                                                  else do put (fromList $ f $ toDescList (c `union` $ fromList newItems), fst $ unzip newItems)
                                                          deductiveIteration s

-- | A step to apply all rules for all possible antecedents.
deductiveStep :: [it] -> [(it, wt)] -> ([DeductiveRule it, wt)] -> [(it, wt)]
deductiveStep forcedItems is rs = rs >>= ruleApplication forcedItems is

-- | Application of one rule to all possible antecedents.
ruleApplication :: [it] -> [(it, wt)] -> (DeductiveRule it, wt) -> [(it, wt)]
ruleApplication forcedItems is ((DeductiveRule fs app), wt) = mapMaybe (`ruleApplication'` d) candidates 
  where
    candidates = filter (any . map (`elem` forcedItems . fst)) $ mapM (`filter` is) $ map (. fst) fs
    ruleApplication' :: (Num wt) =>[(it, wt)] -> (DeductiveRule it, wt) -> Maybe (it, wt)
    ruleApplication' is (DeductiveRule fs app, w) = case app antecedents of
                                                      Just c -> Just (c, w * product weights)
                                                      Nothing -> Nothing
      where
        (antecedents, weights) = unzip is