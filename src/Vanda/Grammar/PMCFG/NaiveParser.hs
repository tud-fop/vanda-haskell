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
import qualified Data.Array as A

import Data.Tree (Tree(Node))
import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import Data.Group (Group(invert))

data Item nt wt = Active  Int [nt] Int [Tree Int] InstantiatedFunction wt
                | Passive nt Rangevector (Tree Int) wt

type Container nt wt =  (Map.HashMap nt [Item nt wt] , Map.HashMap nt [Item nt wt])

instance (Eq nt) => Eq (Item nt wt) where
  (Active r _ _ ds fs _) == (Active r' _ _ ds' fs' _) = r == r' && ds == ds' && fs == fs'
  (Passive a rv d _) == (Passive a' rv' d' _) = a == a' && rv == rv' && d == d'
  _ == _ = False
 
instance (Hashable nt) => Hashable (Item nt wt) where
    salt `hashWithSalt` (Active r _ off _ _ _) = salt `hashWithSalt` r `hashWithSalt` off
    salt `hashWithSalt` (Passive a rho _ _) = salt `hashWithSalt` a `hashWithSalt` rho

instance (Show nt) => Show (Item nt wt) where
  show (Active r as i _ fs _) = "[active] Rule #" ++ show r ++ " " ++ show i ++ ":" ++ show as ++ " // " ++ prettyPrintInstantiatedFunction fs
  show (Passive a rv _ _) = "[passive] " ++ show a ++ " " ++ show rv 

-- | Top-level function to parse a word using a PMCFG.
parse :: (Hashable nt, Eq nt, Eq t) 
      => PMCFG nt t         -- ^ unweighted grammar
      -> Int                -- ^ beam width
      -> [t]                -- ^ terminal word
      -> [Tree (Rule nt t)] -- ^ derivation tree of applied rules
parse (PMCFG s rs) = weightedParse $ WPMCFG s $ zip rs $ repeat (cost 1 :: Cost Double)


-- | Top-level function to parse a word using a weighted PMCFG.
weightedParse :: forall nt t wt. (Hashable nt, Eq nt, Eq t, Ord wt, Group wt) 
              => WPMCFG nt wt t     -- ^ weighted grammar
              -> Int                -- ^ beam width
              -> [t]                -- ^ terminal word
              -> [Tree (Rule nt t)] -- ^ derivation tree of applied rules
weightedParse (WPMCFG s rs) bw w = map (\ (Passive _ _ t _) -> fmap (fst . (rs' A.!)) t) 
                                    $ filter resultfilter
                                    $ solve ds
  where
    rs' = A.listArray (1, length rs) rs
    
    ds = DeductiveSolver (Map.empty, Map.empty) 
                         update 
                         ( initialPrediction rs' s w insides
                         : predictionRule w rs' insides outsides
                         : conversionRule rs' outsides
                         : [completionRule rs' insides outsides] ) 
                         bw
    
    insides = insideWeights rs
    outsides = outsideWeights insides rs s
    
    resultfilter :: Item nt wt -> Bool
    resultfilter (Passive a rho _ _) = a `elem` s && rho == singleton (entire w)
    resultfilter _                   = False 

    update :: Container nt wt
            -> Item nt wt 
            -> Container nt wt
    update (p, a) item@(Passive nta _ _ _) = (updateGroup nta item p, a)
    update (p, a) item@(Active _ (nta:_) _ _ _ _) = (p, updateGroup nta item a)
    update (p, a) _ = (p, a)

initialPrediction :: forall nt t wt. (Hashable nt, Eq nt, Eq t, Monoid wt)
                  => A.Array Int (Rule nt t, wt)
                  -> [nt]
                  -> [t]
                  -> Map.HashMap nt wt
                  -> DeductiveRule (Item nt wt) wt (Container nt wt)
initialPrediction rs s word insides = DeductiveRule 0 gets app
  where
    srules = filter (\ (_, (Rule ((a,_),_),_)) -> a `elem` s) $ A.assocs rs
    
    gets :: Container nt wt -> Item nt wt -> [[Item nt wt]]
    gets _ _ = [[]]
    
    app :: Container nt wt 
        -> [Item nt wt] 
        -> [(Item nt wt, wt)]
    app _ [] =  [ (Active r as 0 [] fw inside, inside)
                | (r, (Rule ((_, as), f), w)) <- srules
                , fw <- instantiate word f
                , let inside = w <> mconcat (map (insides Map.!) as)
                ]
    app _ _ = []

-- | Constructs deductive rules using one rule of a grammar.
-- * prediction: initializes an active item without using antecendent items
predictionRule :: forall nt t wt. (Eq t, Eq nt, Hashable nt, Group wt) 
               => [t] 
               -> A.Array Int (Rule nt t, wt)
               -> Map.HashMap nt wt
               -> Map.HashMap nt wt
               -> DeductiveRule (Item nt wt) wt (Container nt wt)
predictionRule word rs insides outsides = DeductiveRule 1 gets app
  where
    gets :: Container nt wt -> Item nt wt -> [[Item nt wt]]
    gets _ i@(Active _ (_:_) _ _ _ _) = [[i]]
    gets _ _ = []
    
    app :: Container nt wt 
        -> [Item nt wt] 
        -> [(Item nt wt, wt)]
    app _ [Active _ (a:_) _ _ _ _] = [ (Active r' as' 0 [] fw inside, inside <> outside)
                                     | (r', (Rule ((a', as'), f'), w')) <- A.assocs rs
                                     , a' == a
                                     , fw <- instantiate word f'
                                     , let inside = w' <> mconcat (map (insides Map.!) as')
                                           outside = outsides Map.! a
                                     ]
    app _ _ = []


-- | Constructs deductive rules using one rule of a grammar.
-- * conversion: converts an active item into a passive one, if there are no variables left
conversionRule :: forall nt t wt. (Monoid wt, Eq nt, Hashable nt) 
               => A.Array Int (Rule nt t, wt)
               -> Map.HashMap nt wt
               -> DeductiveRule (Item nt wt) wt (Container nt wt)
conversionRule rs outsides = DeductiveRule 1 gets app
  where
    gets :: Container nt wt -> Item nt wt -> [[Item nt wt]]
    gets _ i@(Active _ [] _ _ _ _ ) = [[i]]
    gets _ _ = []

    app :: Container nt wt -> [Item nt wt] -> [(Item nt wt, wt)]
    app _ [Active r [] _ ds fs inside] = [ (Passive a rv' (Node r $ reverse ds) inside, inside <> outside)
                                         | rv' <- maybeToList $ mapM ((>>= toRange) . concVarRange) fs >>= fromList
                                         , let a = lhs $ rs A.! r
                                               outside = outsides Map.! a
                                         ]
    app _ _ = []


-- | Constructs deductive rules using one rule of a grammar.
-- * completion: step-by-step substituting of variables in instantiated function using ranges of passive items
completionRule :: forall nt wt t. (Hashable nt, Eq nt, Group wt)
               => A.Array Int (Rule nt t, wt)
               -> Map.HashMap nt wt
               -> Map.HashMap nt wt
               -> DeductiveRule (Item nt wt) wt (Container nt wt)
completionRule rules insides outsides = DeductiveRule 2 gets app
  where
    gets :: Container nt wt -> Item nt wt -> [[Item nt wt]]
    gets (passives, _)  i@(Active _ (next:_) _ _ _ _) =  [ [i, passive]
                                                         | passive <- Map.lookupDefault [] next passives
                                                         ]
    gets (_, actives) i@(Passive next _ _ _) = [ [active, i]
                                               | active <- Map.lookupDefault [] next actives
                                               ]
    gets _ _ = []

    app :: Container nt wt -> [Item nt wt] -> [(Item nt wt, wt)]
    app _ [Active r (a:as) offset ds fs ain, Passive _ rv d pin] = [ (Active r as (offset+1) (d:ds) fs' inside, inside <> outside)
                                                                   | fs' <- maybeToList $ mapM concVarRange $ insert offset rv fs
                                                                   , let inside = ain <> invert (insides Map.! a) <> pin
                                                                         nta = lhs $ rules A.! r
                                                                         outside = outsides Map.! nta 
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