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
import qualified Data.HashMap.Lazy as Map
import Data.Tree (Tree)
import Data.Maybe (mapMaybe)
import Data.Group (Group(invert))
import Data.Monoid ((<>))




data Item nt t wt = Initial (Rule nt t, wt)   -- ^ grammar rule and weight
                            wt                -- ^ inside weight
                            wt                -- ^ outside weight
                  | Passive nt                -- ^ lhs's nonterminal
                            Rangevector       -- ^ spanned subword
                            (Derivation nt t) -- ^ grammar derivation
                            wt                -- ^ inside weight == weight of derivation
                  
type Container nt t wt =  ( Map.HashMap nt [Item nt t wt]  -- ^ passive items, maps a to all passive with a on lhs
                          , Map.HashMap nt [Item nt t wt]  -- ^ active items, maps a to all active items with a in as
                          , Map.HashMap nt wt              -- ^ inside weights for each nonterminal
                          )

instance (Eq nt, Eq t) => Eq (Item nt t wt) where
  (Initial (r, _) _ _) == (Initial (r', _) _ _) = r == r'
  (Passive a rv d _) == (Passive a' rv' d' _) = a == a' && rv == rv' && d == d'
  _ == _ = False

instance (Hashable nt, Hashable t) => Hashable (Item nt t wt) where
    salt `hashWithSalt` (Initial (r, _) _ _) = salt `hashWithSalt` r
    salt `hashWithSalt` (Passive a rho d _) = salt `hashWithSalt` a `hashWithSalt` rho `hashWithSalt` d

instance (Show nt, Show t) => Show (Item nt t wt) where
    show (Initial (r, _) _ _) = "[active] " ++ prettyPrintRule r
    show (Passive a rv _ _) = "[passive] " ++ show a ++ " " ++ show rv 



-- | Top-level function to parse a word using a grammar.
parse :: (Eq t, Eq nt, Hashable nt, Hashable t)
      => PMCFG nt t                               -- ^ the grammar
      -> Int
      -> [t]                                      -- ^ the word
      -> [Tree (Rule nt t)]                       -- ^ list of derivation trees 
parse (PMCFG s rules) = weightedParse $ WPMCFG s $ zip rules $ repeat (cost 1 :: Cost Int)


-- | Top-level function to parse a word using a weighted grammar.
weightedParse :: (Eq t, Eq nt, Hashable t, Hashable nt, Group wt, Ord wt)
              => WPMCFG nt wt t             -- ^ weighted grammar
              -> Int
              -> [t]                        -- ^ word
              -> [Tree (Rule nt t)]   -- ^ parse trees and resulting weights
weightedParse (WPMCFG s rs) bw word = map (\ (Passive _ _ (Derivation t) _) -> t) 
                                      $ filter resultFilter
                                      $ solve 
                                      $ DeductiveSolver (Map.empty, Map.empty, insideWeights rs) update deductiveRules bw
  where
    resultFilter (Passive a rho _ _) = (a `elem` s) && (rho == singleton (entire word))
    resultFilter _ = False

    deductiveRules = initialPrediction srules : prediction rs : [completion word]
    srules = filter (\ (Rule ((a,_),_), _) -> a `elem` s) rs

    update :: (Eq nt, Hashable nt, Ord wt)
           => Container nt t wt
           -> (Item nt t wt)
           -> Container nt t wt
    update (passives, actives, insides) item@(Passive a _ _ _) = ( updateGroup a item passives
                                                                 , actives
                                                                 , insides
                                                                 )
    update (passives, actives, insides) item@(Initial (Rule ((_, as), _), _) _ _) = ( passives
                                                                                    , updateGroups as item actives
                                                                                    , insides
                                                                                    )

initialPrediction :: forall nt t wt. (Eq nt, Hashable nt, Monoid wt, Ord wt) 
                  => [(Rule nt t, wt)] 
                  -> DeductiveRule (Item nt t wt) wt (Container nt t wt)
initialPrediction rs = DeductiveRule 0 gets app
  where
    gets :: (Container nt t wt) -> Item nt t wt -> [[Item nt t wt]]
    gets _ _ = [[]]
    
    app :: (Eq nt, Hashable nt, Monoid wt)
        => (Container nt t wt) 
        -> [Item nt t wt] 
        -> [(Item nt t wt, wt)]
    app (_, _, insides) [] =  [ (Initial r inside mempty, inside)
                              | r@(Rule ((_, as), _), w) <- rs
                              , inside <- return $ w <> mconcat (map (\ a -> Map.lookupDefault mempty a insides) as)
                              ]
    app _ _ = []
    
prediction :: forall nt t wt. (Eq nt, Hashable nt, Group wt) 
           => [(Rule nt t, wt)] 
           -> DeductiveRule (Item nt t wt) wt (Container nt t wt)
prediction rs = DeductiveRule 1 gets app
  where
    gets :: (Eq nt) 
         => (Container nt t wt) -> Item nt t wt -> [[Item nt t wt]]
    gets _ i@(Initial _ _ _) = [[i]]
    gets _ _ = []
    
    app :: (Eq nt, Hashable nt, Group wt) 
        => (Container nt t wt) 
        -> [Item nt t wt] 
        -> [(Item nt t wt, wt)]
    app (_, _, insides) [Initial (Rule ((_, as), _), _) inside' outside'] = [ (Initial r' inside outside, inside <> outside)
                                                                            | r'@(Rule ((a', as'), _), w') <- rs
                                                                            , a' `elem` as
                                                                            , inside <- return $ w' <> mconcat (map (\ a -> Map.lookupDefault mempty a insides) as')
                                                                            , outside <- return $ inside' <> outside' <> invert (Map.lookupDefault mempty a' insides)
                                                                            ]
    app _ _ = []
      
completion :: forall nt t wt. (Eq t, Eq nt, Hashable nt, Monoid wt) 
           => [t] 
           -> DeductiveRule (Item nt t wt) wt (Container nt t wt)
completion word = DeductiveRule 3 gets app
  where
    gets :: (Eq nt, Hashable nt) => (Container nt t wt) -> Item nt t wt -> [[Item nt t wt]]
    gets (passives, _, _) i@(Initial (Rule ((_, as), _), _) _ _) = [ i:candidates
                                                                   | candidates <- mapM (\ a -> Map.lookupDefault [] a passives) as
                                                                   ]
    gets (passives, actives, _) i@(Passive a _ _ _) = [ active:candidates
                                                      | active@(Initial (Rule ((_, as), _), _) _ _) <- Map.lookupDefault [] a actives
                                                      , candidates <- filter (any (== i)) $ mapM (\ nta -> Map.lookupDefault [] nta passives) as
                                                      ]
                                                    
    app :: (Eq t, Monoid wt) => (Container nt t wt) -> [Item nt t wt] -> [(Item nt t wt, wt)]
    app _ (Initial (r@(Rule ((a, _), f)) , w) _ outside : pas) =  [ (Passive a rv (node r ds) inside, inside <> outside)
                                                                  | rv <- mapMaybe (insert rvs) $ instantiate word f
                                                                  , inside <- return $ mconcat ws <> w
                                                                  ]
      where
        (rvs, ds, ws) = foldr (\ (Passive _ rv t iw) (rvs', ts', ws') -> (rv:rvs', t:ts', iw:ws')) ([], [], []) pas
        insert :: [Rangevector] -> InstantiatedFunction -> Maybe Rangevector
        insert rvs' = (>>= fromList) . mapM ((>>= toRange) . concVarRange . map (insert' rvs'))
          
        insert' :: [Rangevector] -> VarT Range -> VarT Range
        insert' rvs' (Var x y)  = T $ rvs' !! x ! y
        insert' _ r' = r'
    app _ _ = []