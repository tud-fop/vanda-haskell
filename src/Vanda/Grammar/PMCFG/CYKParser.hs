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

{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Grammar.PMCFG.CYKParser where

import Vanda.Grammar.PMCFG.DeductiveSolver
import Vanda.Grammar.PMCFG.Weights
import Vanda.Grammar.PMCFG.Range
import Vanda.Grammar.PMCFG

import Data.Hashable (Hashable(hashWithSalt))
import qualified Data.HashMap.Lazy  as Map
import qualified Data.Array         as A
import Data.Tree (Tree(Node))
import Data.Maybe (maybeToList)
import Data.Group (Group(invert))
import Data.Monoid ((<>))




data Item nt wt = Active  Int InstantiatedFunction wt wt
                | Passive nt Rangevector (Tree Int) wt

type Container nt wt =  ( Map.HashMap nt [Item nt wt], Map.HashMap nt [Item nt wt])

instance (Eq nt) => Eq (Item nt wt) where
  (Active r fw _ _) == (Active r' fw' _ _) = r == r'&& fw == fw' 
  (Passive a rv d _) == (Passive a' rv' d' _) = a == a' && rv == rv' && d == d'
  _ == _ = False

instance (Hashable nt) => Hashable (Item nt wt) where
    salt `hashWithSalt` (Active r _ _ _) = salt `hashWithSalt` r
    salt `hashWithSalt` (Passive a rho _ _) = salt `hashWithSalt` a `hashWithSalt` rho

instance (Show nt) => Show (Item nt wt) where
    show (Active r f _ _) = "[active] rule #" ++ show r ++ " " ++ prettyPrintInstantiatedFunction f
    show (Passive a rv _ _) = "[passive] " ++ show a ++ " " ++ show rv 



-- | Top-level function to parse a word using a grammar.
parse :: (Eq t, Eq nt, Hashable nt, Hashable t)
      => PMCFG nt t                               -- ^ the grammar
      -> Int
      -> [t]                                      -- ^ the word
      -> [Tree (Rule nt t)]                       -- ^ list of derivation trees 
parse (PMCFG s rules) = weightedParse $ WPMCFG s $ zip rules $ repeat (cost 1 :: Cost Int)


-- | Top-level function to parse a word using a weighted grammar.
weightedParse :: forall nt t wt. (Eq t, Eq nt, Hashable t, Hashable nt, Group wt, Ord wt)
              => WPMCFG nt wt t             -- ^ weighted grammar
              -> Int                        -- ^ beam width
              -> [t]                        -- ^ word
              -> [Tree (Rule nt t)]   -- ^ parse trees and resulting weights
weightedParse (WPMCFG s rs) bw word = map (\ (Passive _ _ t _) -> fmap (fst . (rs' A.!)) t) 
                                      $ filter resultFilter
                                      $ solve 
                                      $ DeductiveSolver (Map.empty, Map.empty) update deductiveRules bw
  where
    rs' = A.listArray (1, length rs) rs

    resultFilter (Passive a rho _ _) = (a `elem` s) && (rho == singleton (entire word))
    resultFilter _ = False
    
    insides = insideWeights rs

    deductiveRules = initialPrediction s rs' insides word 
                      : prediction rs' insides word 
                      : [completion rs']

    update :: (Eq nt, Hashable nt, Ord wt)
           => Container nt wt
           -> (Item nt wt)
           -> Container nt wt
    update (passives, actives) item@(Passive a _ _ _) = ( updateGroup a item passives
                                                        , actives
                                                        )
    update (passives, actives) item@(Active r _ _ _) =  ( passives
                                                        , updateGroups as item actives
                                                        )
      where as = antecedents $ rs' A.! r

initialPrediction :: forall nt t wt. (Eq nt, Eq t, Hashable nt, Monoid wt, Ord wt) 
                  => [nt]
                  -> A.Array Int (Rule nt t, wt)
                  -> Map.HashMap nt wt
                  -> [t]
                  -> DeductiveRule (Item nt wt) wt (Container nt wt)
initialPrediction s rs insides word = DeductiveRule 0 gets app
  where
    srules = filter (\ (_, (Rule ((a, _), _), _)) -> a `elem` s) $ A.assocs rs
    
    gets :: (Container nt wt) -> Item nt wt -> [[Item nt wt]]
    gets _ _ = [[]]
    
    app :: (Eq nt, Hashable nt, Monoid wt)
        => (Container nt wt) 
        -> [Item nt wt] 
        -> [(Item nt wt, wt)]
    app _ [] =  [ (Active r fw inside mempty, inside)
                | (r, (Rule ((_, as), f), w)) <- srules
                , fw <- instantiate word f
                , let inside = w <> mconcat (map (insides Map.!) as)
                ]
    app _ _ = []
    
prediction :: forall nt t wt. (Eq nt, Eq t, Hashable nt, Group wt) 
           => A.Array Int (Rule nt t, wt)
           -> Map.HashMap nt wt
           -> [t]
           -> DeductiveRule (Item nt wt) wt (Container nt wt)
prediction rs insides word = DeductiveRule 1 gets app
  where
    gets :: (Container nt wt) -> Item nt wt -> [[Item nt wt]]
    gets _ i@(Active _ _ _ _) = [[i]]
    gets _ _ = []
    
    app :: (Eq nt, Eq t, Hashable nt, Group wt) 
        => (Container nt wt) 
        -> [Item nt wt] 
        -> [(Item nt wt, wt)]
    app _ [Active r _ inside' outside'] = [ (Active r' fw inside outside, inside <> outside)
                                          | let as = antecedents $ rs A.! r
                                          , (r', (Rule ((a', as'), f'), w')) <- A.assocs rs
                                          , a' `elem` as
                                          , fw <- instantiate word f'
                                          , let inside = w' <> mconcat (map (insides Map.!) as')
                                                outside = inside' <> outside' <> invert (insides Map.! a')
                                          ]
    app _ _ = []
      
completion :: forall nt t wt. (Eq t, Eq nt, Hashable nt, Monoid wt) 
           => A.Array Int (Rule nt t, wt)
           -> DeductiveRule (Item nt wt) wt (Container nt wt)
completion rs = DeductiveRule 3 gets app
  where
    gets :: (Eq nt, Hashable nt) => (Container nt wt) -> Item nt wt -> [[Item nt wt]]
    gets (passives, _) i@(Active r _ _ _) = [ i:candidates
                                            | let as = antecedents $ rs A.! r
                                            , candidates <- mapM (\ a -> Map.lookupDefault [] a passives) as
                                            ]
    gets (passives, actives) i@(Passive a _ _ _) = [ active:candidates
                                                   | active@(Active r _ _ _) <- Map.lookupDefault [] a actives
                                                   , let as = antecedents $ rs A.! r
                                                   , candidates <- filter (any (== i)) $ mapM (\ nta -> Map.lookupDefault [] nta passives) as
                                                   ]
    
    app :: (Eq t, Monoid wt) => (Container nt wt) -> [Item nt wt] -> [(Item nt wt, wt)]
    app _ (Active r fw _ outside : pas) =  [ (Passive a rv (Node r ds) inside, inside <> outside)
                                           | rv <- maybeToList $ insert rvs fw
                                           , let (Rule ((a,_),_), w) = rs A.! r
                                                 inside = mconcat ws <> w
                                           ]
      where
        (rvs, ds, ws) = foldr (\ (Passive _ rv t iw) (rvs', ts', ws') -> (rv:rvs', t:ts', iw:ws')) ([], [], []) pas
        
        insert :: [Rangevector] -> InstantiatedFunction -> Maybe Rangevector
        insert rvs' = (>>= fromList) . mapM ((>>= toRange) . concVarRange . map (insert' rvs'))
          
        insert' :: [Rangevector] -> VarT Range -> VarT Range
        insert' rvs' (Var x y)  = T $ rvs' !! x ! y
        insert' _ r' = r'
    app _ _ = []