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
-- naive (active) parsing algorithm by Burden and Ljunglöf.
-- @weightedParse@ uses a weighted PMCFG to find a list of all possible
-- derivation trees ordered by minimal cost / maximum probability. The
-- rules' weights need to be instances of @Monoid@ and @Dividable@.
-- @parse@ uses an unweighted grammar to find a list of derivation trees
-- ordered by least rule applications.
--
-- 
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Grammar.PMCFG.NaiveParser
    where
    -- ( weightedParse
    -- , parse
    -- ) where

import Vanda.Grammar.PMCFG.DeductiveSolver
import Vanda.Grammar.PMCFG.Weights
import Vanda.Grammar.PMCFG.Range
import Vanda.Grammar.PMCFG

import Data.Hashable (Hashable(hashWithSalt))
import qualified Data.HashMap.Lazy as Map

import Data.Tree (Tree)
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Group (Group(invert))

data Item nt t wt = Initial (Rule nt t, wt)      -- ^ grammar rule
                            wt                   -- ^ inside weight
                            wt                   -- ^ outside weight
                  | Active  (Rule nt t, wt)      -- ^ grammar rule
                            [nt]                 -- ^ q of nonterminals to complete
                            Int                  -- ^ offset for insertion
                            [Derivation nt t]    -- ^ completet derivations (reversed)
                            InstantiatedFunction -- ^ instantiated context to insert in
                            wt                   -- ^ inside weight
                            wt                   -- ^ outside weight
                  | Passive nt                   -- ^ lhs's nonterminal
                            Rangevector          -- ^ spanned subword
                            (Derivation nt t)    -- ^ derivation
                            wt                   -- ^ inside weight

type Container nt t wt =  ( Map.HashMap nt [Item nt t wt]         -- ^ maps rules lhs non-terminal to passive items
                          , Map.HashMap (Maybe nt) [Item nt t wt] -- ^ maps nt to active items with it to complete next
                          , Map.HashMap nt wt                     -- ^ maps nt to inside weight
                          )

instance (Eq nt, Eq t) => Eq (Item nt t wt) where
  (Initial (r, _) _ _) == (Initial (r', _) _ _) = r == r'
  (Active (r, _) _ _ ds fs _ _) == (Active (r', _) _ _ ds' fs' _ _) = r == r' && ds == ds' && fs == fs'
  (Passive a rv d _) == (Passive a' rv' d' _) = a == a' && rv == rv' && d == d'
  _ == _ = False
 
instance (Hashable nt, Hashable t) => Hashable (Item nt t wt) where
    salt `hashWithSalt` (Initial (r, _) _ _) = salt `hashWithSalt` r
    salt `hashWithSalt` (Active (r, _) _ _ ds _ _ _) = salt `hashWithSalt` r `hashWithSalt` ds
    salt `hashWithSalt` (Passive a rho d _) = salt `hashWithSalt` a `hashWithSalt` rho `hashWithSalt` d

instance (Show nt, Show t) => Show (Item nt t wt) where
  show (Initial (r, _) _ _) = "[init] " ++ prettyPrintRule r 
  show (Active (r, _) as i _ fs _ _) = "[active] " ++ prettyPrintRule r ++ " " ++ show as ++ "+" ++ show i ++ " " ++ prettyPrintInstantiatedFunction fs
  show (Passive a rv _ _) = "[passive] " ++ show a ++ " " ++ show rv 

-- | Top-level function to parse a word using a PMCFG.
parse :: (Hashable nt, Hashable t, Eq nt, Eq t) 
      => PMCFG nt t         -- ^ unweighted grammar
      -> Int                -- ^ beam width
      -> [t]                -- ^ terminal word
      -> [Tree (Rule nt t)] -- ^ derivation tree of applied rules
parse (PMCFG s rs) = weightedParse $ WPMCFG s $ zip rs $ repeat (cost 1 :: Cost Double)


-- | Top-level function to parse a word using a weighted PMCFG.
weightedParse :: forall nt t wt. (Hashable nt, Hashable t, Eq nt, Eq t, Ord wt, Group wt) 
              => WPMCFG nt wt t     -- ^ weighted grammar
              -> Int                -- ^ beam width
              -> [t]                -- ^ terminal word
              -> [Tree (Rule nt t)] -- ^ derivation tree of applied rules
weightedParse (WPMCFG s rs) bw w = map (\ (Passive _ _ (Derivation t) _) -> t) 
                                    $ filter resultfilter
                                    $ solve ds
    where
        ds = DeductiveSolver (Map.empty, Map.empty, insideWeights rs) 
                             update 
                             (initialPrediction srules : predictionRule w rs : conversionRule : [completionRule]) 
                             bw
        srules = filter (\ (Rule ((a,_),_), _) -> a `elem` s) rs
        
        resultfilter :: (Eq nt) => Item nt t wt -> Bool
        resultfilter (Passive a rho _ _) = a `elem` s && rho == singleton (entire w)
        resultfilter _                   = False 

        update :: (Eq nt, Hashable nt)
               => Container nt t wt
               -> Item nt t wt 
               -> Container nt t wt
        update (p, a, i) (Initial _ _ _) = (p, a, i)
        update (p, a, i) item@(Passive nta _ _ _) = (updateGroup nta item p, a, i)
        update (p, a, i) item@(Active _ as _ _ _ _ _) = (p, updateGroup (safehead as) item a, i)
          where 
            safehead [] = Nothing
            safehead (x:_) = Just x

initialPrediction :: forall nt t wt. (Hashable nt, Eq nt, Monoid wt)
                  => [(Rule nt t, wt)]
                  -> DeductiveRule (Item nt t wt) wt (Container nt t wt)
initialPrediction rs = DeductiveRule 0 gets app
  where
    gets :: Container nt t wt -> Item nt t wt -> [[Item nt t wt]]
    gets _ _ = [[]]
    
    app :: (Hashable nt, Eq nt, Monoid wt) 
        => Container nt t wt 
        -> [Item nt t wt] 
        -> [(Item nt t wt, wt)]
    app (_, _, insides) [] =  [ (Initial r inside mempty, inside)
                              | r@(Rule ((_, as), _), w) <- rs
                              , let inside = w <> mconcat (map (\ a -> Map.lookupDefault mempty a insides) as)
                              ]
    app _ _ = []

-- | Constructs deductive rules using one rule of a grammar.
-- * prediction: initializes an active item without using antecendent items
predictionRule :: forall nt t wt. (Eq t, Eq nt, Hashable nt, Group wt) 
               => [t] 
               -> [(Rule nt t, wt)] 
               -> (DeductiveRule (Item nt t wt) wt (Container nt t wt))
predictionRule word rs = DeductiveRule 1 gets app
  where
    gets :: Container nt t wt -> Item nt t wt -> [[Item nt t wt]]
    gets _ i@(Initial _ _ _) = [[i]]
    gets _ _ = []
    
    app :: (Group wt, Eq nt, Hashable nt) 
        => Container nt t wt 
        -> [Item nt t wt] 
        -> [(Item nt t wt, wt)]
    app (_, _, insides) [Initial r@(Rule ((_, as), f), _) inside' outside'] = [ (Active r as 0 [] inst inside' outside', inside' <> outside')
                                                                              | inst <- instantiate word f
                                                                              ]
                                                                              ++
                                                                              [ (Initial (r', w') inside outside, inside <> outside)
                                                                              | (r'@(Rule ((a', as'), _)), w') <- rs
                                                                              , a' `elem` as
                                                                              , inside <- return $ w' <> mconcat (map (\ a -> Map.lookupDefault mempty a insides) as')
                                                                              , outside <- return $ outside' <> inside' <> invert (Map.lookupDefault mempty a' insides)
                                                                              ]
    app _ _ = []


-- | Constructs deductive rules using one rule of a grammar.
-- * conversion: converts an active item into a passive one, if there are no variables left
conversionRule :: forall nt t wt. (Monoid wt) 
               => DeductiveRule (Item nt t wt) wt (Container nt t wt)
conversionRule = DeductiveRule 1 gets app
  where
    gets :: Container nt t wt -> Item nt t wt -> [[Item nt t wt]]
    gets _ i@(Active _ [] _ _ _ _ _) = [[i]]
    gets _ _ = []

    app :: Container nt t wt -> [Item nt t wt] -> [(Item nt t wt, wt)]
    app _ [Active (r@(Rule ((a, _), _)), _) [] _ ds fs inside outside] = [ (Passive a rv' (node r $ reverse ds) inside, inside <> outside)
                                                                        | rv' <- maybeToList $ mapM ((>>= toRange) . concVarRange) fs >>= fromList
                                                                        ]
    app _ _ = []


-- | Constructs deductive rules using one rule of a grammar.
-- * completion: step-by-step substituting of variables in instantiated function using ranges of passive items
completionRule :: forall nt t wt. (Hashable nt, Eq nt, Eq t, Group wt)
               => DeductiveRule (Item nt t wt) wt (Container nt t wt)
completionRule = DeductiveRule 2 gets app
  where
    gets :: Container nt t wt -> Item nt t wt -> [[Item nt t wt]]
    gets (passives, _, _)  i@(Active _ (next:_) _ _ _ _ _) =  [ [i, passive]
                                                              | passive <- Map.lookupDefault [] next passives
                                                              ]
    gets (_, actives, _) i@(Passive next _ _ _) = [ [active, i]
                                                  | active <- Map.lookupDefault [] (Just next) actives
                                                  ]
    gets _ _ = []

    app :: Container nt t wt -> [Item nt t wt] -> [(Item nt t wt, wt)]
    app (_, _, insides) [Active rule (a:as) offset ds fs ainside outside, Passive _ rv d pinside] =  [ (Active rule as (offset+1) (d:ds) fs' inside outside, inside <> outside)
                                                                                                     | fs' <- maybeToList $ mapM concVarRange $ insert offset rv fs
                                                                                                     , inside <- return $ ainside <> invert (Map.lookupDefault mempty a insides) <> pinside
                                                                                                     ]
    app _ _ = []


-- | substitutes variables with index 'off' with a ranges of a vector
insert :: Int -> Rangevector -> InstantiatedFunction -> InstantiatedFunction
insert off rv = map (map (substitute off rv))
    where
        substitute :: Int -> Rangevector -> VarT Range -> VarT Range
        substitute i rv' (Var i' j)
            | i' == i =  T $ rv' ! j
            | otherwise = Var i' j
        substitute _ _ r = r


isActive :: Item nt t wt -> Bool
isActive (Passive _ _ _ _) = False
isActive _ = True