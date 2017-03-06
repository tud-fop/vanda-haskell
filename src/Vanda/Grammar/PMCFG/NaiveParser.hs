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

data Item nt wt = Active  Int [nt] Int [Tree Int] InstantiatedFunction wt wt
                | Passive nt Rangevector (Tree Int) wt

type Container nt wt =  ( Map.HashMap nt [Item nt wt] , Map.HashMap (Maybe nt) [Item nt wt])

instance (Eq nt) => Eq (Item nt wt) where
  (Active r _ _ ds fs _ _) == (Active r' _ _ ds' fs' _ _) = r == r' && ds == ds' && fs == fs'
  (Passive a rv d _) == (Passive a' rv' d' _) = a == a' && rv == rv' && d == d'
  _ == _ = False
 
instance (Hashable nt) => Hashable (Item nt wt) where
    salt `hashWithSalt` (Active r _ _ _ fw _ _) = salt `hashWithSalt` r `hashWithSalt` fw
    salt `hashWithSalt` (Passive a rho _ _) = salt `hashWithSalt` a `hashWithSalt` rho

instance (Show nt) => Show (Item nt wt) where
  show (Active r as i _ fs _ _) = "[active] Rule #" ++ show r ++ " " ++ show i ++ ":" ++ show as ++ " // " ++ prettyPrintInstantiatedFunction fs
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
weightedParse (WPMCFG s rs) bw w = map (\ (Passive _ _ t _) -> fmap (fst . (rs' A.!)) t) 
                                    $ filter resultfilter
                                    $ solve ds
  where
    rs' = A.listArray (1, length rs) rs
    
    ds = DeductiveSolver (Map.empty, Map.empty) 
                         update 
                         (initialPrediction rs' s w insides : predictionRule w rs' insides : conversionRule rs' : [completionRule insides]) 
                         bw
    
    insides = insideWeights rs
    
    resultfilter :: (Eq nt) => Item nt wt -> Bool
    resultfilter (Passive a rho _ _) = a `elem` s && rho == singleton (entire w)
    resultfilter _                   = False 

    update :: (Eq nt, Hashable nt)
            => Container nt wt
            -> Item nt wt 
            -> Container nt wt
    update (p, a) item@(Passive nta _ _ _) = (updateGroup nta item p, a)
    update (p, a) item@(Active _ as _ _ _ _ _) = (p, updateGroup (safehead as) item a)
      where 
        safehead [] = Nothing
        safehead (x:_) = Just x

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
    
    app :: (Hashable nt, Eq nt, Eq t, Monoid wt) 
        => Container nt wt 
        -> [Item nt wt] 
        -> [(Item nt wt, wt)]
    app _ [] =  [ (Active r as 0 [] fw inside mempty, inside)
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
               -> (DeductiveRule (Item nt wt) wt (Container nt wt))
predictionRule word rs insides = DeductiveRule 1 gets app
  where
    gets :: Container nt wt -> Item nt wt -> [[Item nt wt]]
    gets _ i@(Active _ (_:_) _ _ _ _ _) = [[i]]
    gets _ _ = []
    
    app :: (Group wt, Eq nt, Hashable nt) 
        => Container nt wt 
        -> [Item nt wt] 
        -> [(Item nt wt, wt)]
    app _ [Active _ (a:_) _ _ _ inside' outside'] = [ (Active r' as' 0 [] fw inside outside, inside <> outside)
                                                    | (r', (Rule ((a', as'), f'), w')) <- A.assocs rs
                                                    , a' == a
                                                    , fw <- instantiate word f'
                                                    , let inside = w' <> mconcat (map (insides Map.!) as')
                                                          outside = outside' <> inside' <> invert (insides Map.! a') 
                                                    ]
    app _ _ = []


-- | Constructs deductive rules using one rule of a grammar.
-- * conversion: converts an active item into a passive one, if there are no variables left
conversionRule :: forall nt t wt. (Monoid wt) 
               => A.Array Int (Rule nt t, wt)
               -> DeductiveRule (Item nt wt) wt (Container nt wt)
conversionRule rs = DeductiveRule 1 gets app
  where
    gets :: Container nt wt -> Item nt wt -> [[Item nt wt]]
    gets _ i@(Active _ [] _ _ _ _ _) = [[i]]
    gets _ _ = []

    app :: Container nt wt -> [Item nt wt] -> [(Item nt wt, wt)]
    app _ [Active r [] _ ds fs inside outside] = [ (Passive a rv' (Node r $ reverse ds) inside, inside <> outside)
                                                 | rv' <- maybeToList $ mapM ((>>= toRange) . concVarRange) fs >>= fromList
                                                 , let a = lhs $ rs A.! r
                                                 ]
    app _ _ = []


-- | Constructs deductive rules using one rule of a grammar.
-- * completion: step-by-step substituting of variables in instantiated function using ranges of passive items
completionRule :: forall nt wt. (Hashable nt, Eq nt, Group wt)
               => Map.HashMap nt wt
               -> DeductiveRule (Item nt wt) wt (Container nt wt)
completionRule insides = DeductiveRule 2 gets app
  where
    gets :: Container nt wt -> Item nt wt -> [[Item nt wt]]
    gets (passives, _)  i@(Active _ (next:_) _ _ _ _ _) =  [ [i, passive]
                                                           | passive <- Map.lookupDefault [] next passives
                                                           ]
    gets (_, actives) i@(Passive next _ _ _) = [ [active, i]
                                               | active <- Map.lookupDefault [] (Just next) actives
                                               ]
    gets _ _ = []

    app :: Container nt wt -> [Item nt wt] -> [(Item nt wt, wt)]
    app _ [Active r (a:as) offset ds fs ain aout, Passive _ rv d pin] = [ (Active r as (offset+1) (d:ds) fs' inside aout, inside <> aout)
                                                                        | fs' <- maybeToList $ mapM concVarRange $ insert offset rv fs
                                                                        , let inside = ain <> invert (insides Map.! a) <> pin
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


isActive :: Item nt wt -> Bool
isActive (Passive _ _ _ _) = False
isActive _ = True