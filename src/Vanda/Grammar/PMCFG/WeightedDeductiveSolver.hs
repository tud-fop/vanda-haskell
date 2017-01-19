module Vanda.Grammar.PMCFG.WeightedDeductiveSolver
  where

import Vanda.Grammar.PMCFG.DeductiveSolver (DeductiveRule(DeductiveRule))
import Data.Map (Map, member, union, fromList, toList, lookup, insert)
import Data.Maybe (mapMaybe)
import Data.Hashable (Hashable)
import Control.Monad.State (evalState, State, get, put)

data WeightedDeductiveSolver it wt = WeightedDeductiveSolver [(DeductiveRule it, wt)] ([(it, wt)] -> [(it, wt)])

solve :: (Ord it, Hashable it, Num wt) => WeightedDeductiveSolver it wt -> [(it, wt)]
solve s@(WeightedDeductiveSolver rs f) = evalState (deductiveIteration s) initSet
  where
    initSet = fromList $ f $ deductiveStep [] rs

deductiveIteration :: (Num wt, Ord it, Hashable it) => WeightedDeductiveSolver it wt -> State (Map it wt) [(it, wt)] 
deductiveIteration s@(WeightedDeductiveSolver rs f) = do  c <- get
                                                          let new_items = filter (not . flip member c . fst) $ deductiveStep (toList c) rs
                                                          case new_items of
                                                            [] -> return $ f $ toList c
                                                            is -> do  put $ fromList $ f $ toList (c `union` fromList is) 
                                                                      deductiveIteration s

deductiveStep :: (Num wt) => [(it, wt)] -> [(DeductiveRule it, wt)] -> [(it, wt)]
deductiveStep is rs = rs >>= ruleApplication is

ruleApplication :: (Num wt) => [(it, wt)] -> (DeductiveRule it, wt) -> [(it, wt)]
ruleApplication is d@(DeductiveRule fs app, w) = mapMaybe (`ruleApplication'` d) candidates 
  where
    candidates = mapM ((`filter` is) . (. fst)) fs
    ruleApplication' :: (Num wt) =>[(it, wt)] -> (DeductiveRule it, wt) -> Maybe (it, wt)
    ruleApplication' is (DeductiveRule fs app, w) = case app antecedents of
                                                      Just c -> Just (c, w * product weights)
                                                      Nothing -> Nothing
      where
        (antecedents, weights) = unzip is