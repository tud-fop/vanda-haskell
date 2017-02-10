-----------------------------------------------------------------------------
-- |
-- Module      :  WeightedDeductiveSolver
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
module Vanda.Grammar.PMCFG.WeightedDeductiveSolver
  ( WeightedDeductiveSolver(WeightedDeductiveSolver)
  , DeductiveRule(DeductiveRule)
  , solve
  -- * Weights
  , Dividable(divide)
  , Probabilistic( )
  , probabilistic
  , Cost( )
  , cost
  ) where

import qualified Data.PQueue.Prio.Max as Q
import qualified Data.Map.Strict      as Map
import Control.Monad.State (State, evalState, get, put)
import Data.Tuple (swap)
import Data.Monoid ((<>))


-- | Multiplicative monoid for probabilistic weights.
-- Values are stored in log domain.
newtype Probabilistic a = Probabilistic a deriving (Eq, Ord)


-- | Wraps constructor to check for correct ranged values.
probabilistic :: (Floating a, Ord a) => a -> Probabilistic a
probabilistic x
  | x > 0 && x <= 1 = Probabilistic $ log x
  | otherwise = error "probabilistic value out of range"


instance (Show a, Floating a) => Show (Probabilistic a) where
  show (Probabilistic x) = show (exp x)


-- | Additive monoid for costs.
newtype Cost a = Cost a deriving (Show, Eq)


-- | Wraps constructor to check for correct ranged values.
cost :: (Num a, Ord a) => a -> Cost a
cost x
  | x >= 0 = Cost x
  | otherwise = error "cost value out of range"


class (Monoid d) => Dividable d where
  -- | Divides an object into a given amount of subobjects.
  -- It should be divided s.t. mconcat (divide x n) = x.
  divide :: d -> Int -> [d]


-- | Instance of multiplicative monoid.
instance (Num a) => Monoid (Probabilistic a) where
  mempty = Probabilistic 0
  (Probabilistic x) `mappend` (Probabilistic y) = Probabilistic $ x + y


-- | Uses root to divide a probability into n subprobabilities.
instance (Floating a) => Dividable (Probabilistic a) where
  divide (Probabilistic x) rt = replicate rt $ Probabilistic $ x / fromIntegral rt


-- | Instance of additive monoid.
instance (Num a) => Monoid (Cost a) where
  mempty = Cost 0
  (Cost x) `mappend` (Cost y) = Cost $ x + y


-- | Divides by division.
instance (Fractional a) => Dividable (Cost a) where
  divide (Cost x) d = replicate d $ Cost $ x / fromIntegral d


-- | Uses inverted comparison to find best results with least cost.
instance (Ord a) => Ord (Cost a) where
  (Cost x) `compare` (Cost y) = y `compare` x


-- | An instance for a deduction system.
-- Consists of a list of rules and a limit for queue elements.
data WeightedDeductiveSolver it wt = WeightedDeductiveSolver [(DeductiveRule it, wt)] Int


-- | A rule of a deduction system.
-- Consists of a list of filters that are True for possible antecedents and an application function with multiple possible consequences. 
data DeductiveRule it = DeductiveRule [it -> Bool] ([it] -> [it])


instance (Show wt) => Show (WeightedDeductiveSolver it wt) where
  show (WeightedDeductiveSolver rs _) = "Instance of DeductiveSolver with:\n" ++ unlines (map show rs)

instance Show (DeductiveRule it) where
  showsPrec _ (DeductiveRule filters _) = (++) ("Instance of DeductiveRule with " ++ show (length filters) ++ " anticidents" )


-- | Top-level function that solves a deduction system.
solve :: (Ord wt, Monoid wt, Ord it)
      => WeightedDeductiveSolver it wt            -- ^ the solver instance
      -> [it]                                     -- ^ all (filtered) items, that were deducted
solve (WeightedDeductiveSolver rs b) = evalState (deductiveIteration s') (Q.fromList $ map swap inits, Map.fromList inits)
  where
    s' = WeightedDeductiveSolver (filter (\ (DeductiveRule fs _, _) -> not $ null fs) rs) b
    inits = rs >>= applyWithoutAntecedents
    
    applyWithoutAntecedents (DeductiveRule [] app, w) = zip (app []) (repeat w)
    applyWithoutAntecedents _ = []


-- | Recursion that solves the deduction system.
-- Applies all valid combination of rules to all items as antecedents until there are no new items.
deductiveIteration  :: (Ord wt, Ord it, Monoid wt)
                    => WeightedDeductiveSolver it wt                  -- ^ solver instance for rules and filter
                    -> State (Q.MaxPQueue wt it, Map.Map it wt) [it]          -- ^ state to store previously deducted items, returns
deductiveIteration s@(WeightedDeductiveSolver rs beam) = do (c, a) <- get
                                                            if Q.null c
                                                                then return []
                                                                else do let ((_, item), c') = Q.deleteFindMax c
                                                                            newitems = filter (not . (`Map.member` a) . fst) $ deductiveStep item (Map.toList a) rs
                                                                            a'' = a `Map.union` Map.fromList newitems
                                                                            c'' = Q.fromList $ Q.take beam $ c' `Q.union` Q.fromList (map swap newitems)
                                                                        put (c'', a'')
                                                                        is <- deductiveIteration s
                                                                        return (item:is)


-- | A step to apply all rules for all possible antecedents.
deductiveStep :: (Monoid wt, Eq it) => it -> [(it, wt)] -> [(DeductiveRule it, wt)] -> [(it, wt)]
deductiveStep item is rs = rs >>= ruleApplication item is


-- | Application of one rule to all possible antecedents.
ruleApplication :: (Monoid wt, Eq it) => it -> [(it, wt)] -> (DeductiveRule it, wt) -> [(it, wt)]
ruleApplication item is (DeductiveRule fs app, w) = candidates >>= ruleApplication'  
  where
    candidates =  if null fs
                  then [[]]
                  else filter (any ((== item) . fst)) $ mapM ((`filter` is) . (. fst)) fs
    
    ruleApplication' is' = zip (app antecedents) (repeat weight)
      where
        (antecedents, weights) = unzip is'
        weight = mconcat weights <> w