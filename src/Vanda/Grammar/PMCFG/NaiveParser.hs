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
import Data.Weight
import Vanda.Grammar.PMCFG.Range
import Vanda.Grammar.PMCFG
import qualified Vanda.Grammar.PMCFG.Chart as C

import Data.Hashable (Hashable(hashWithSalt))
import qualified Data.HashMap.Lazy as Map

import Data.Tree (Tree)
import Data.Maybe (maybeToList)
import Data.Semiring

data Item nt t wt = Active (Rule nt t) wt [nt] Int [Rangevector] InstantiatedFunction wt
                | Passive nt Rangevector (C.Backtrace nt t wt) wt

type Container nt t wt =  (C.Chart nt t wt, Map.HashMap nt [Item nt t wt])

instance (Eq nt, Eq t) => Eq (Item nt t wt) where
  (Active r _ _ _ rhos fs _) == (Active r' _ _ _ rhos' fs' _) = r == r'&& rhos == rhos' && fs == fs'
  (Passive a rv _ _) == (Passive a' rv' _ _) = a == a' && rv == rv'
  _ == _ = False
 
instance (Hashable nt, Hashable t) => Hashable (Item nt t wt) where
    salt `hashWithSalt` (Active r _ _ off _ _ _) = salt `hashWithSalt` r `hashWithSalt` off
    salt `hashWithSalt` (Passive a rho _ _) = salt `hashWithSalt` a `hashWithSalt` rho

instance (Show nt, Show t) => Show (Item nt t wt) where
  show (Active r _ as i _ fs _) = "[active] " ++ show r ++ " " ++ show i ++ ":" ++ show as ++ " // " ++ prettyPrintInstantiatedFunction fs
  show (Passive a rv _ _) = "[passive] " ++ show a ++ " " ++ show rv 

-- | Top-level function to parse a word using a PMCFG.
parse :: (Hashable nt, Hashable t, Eq nt, Eq t) 
      => PMCFG nt t         -- ^ unweighted grammar
      -> Int                -- ^ beam width
      -> [t]                -- ^ terminal word
      -> [Tree (Rule nt t)] -- ^ derivation tree of applied rules
parse (PMCFG s rs) = weightedParse $ WPMCFG s $ zip rs $ repeat (cost 1 :: Cost Double)


-- | Top-level function to parse a word using a weighted PMCFG.
weightedParse :: forall nt t wt. (Hashable nt, Hashable t, Eq nt, Eq t, Ord wt, Weight wt) 
              => WPMCFG nt wt t     -- ^ weighted grammar
              -> Int                -- ^ beam width
              -> [t]                -- ^ terminal word
              -> [Tree (Rule nt t)] -- ^ derivation tree of applied rules
weightedParse (WPMCFG s rs) bw word = C.readoff s (singleton $ entire word)
                                        $ fst $ chart (C.empty, Map.empty) update rules bw
  where
    rules = initialPrediction word (filter ((`elem` s) . lhs) rs) insides
            : predictionRule word rs insides outsides
            : conversionRule outsides
            : [completionRule insides outsides]
    
    insides = insideWeights rs
    outsides = outsideWeights insides rs s

    update :: Container nt t wt
            -> Item nt t wt 
            -> (Container nt t wt, Bool)
    update (p, a) (Passive nta rho bt w) = case C.insert p nta rho bt w of 
                                                (p', isnew) -> ((p', a), isnew)
    update (p, a) item@(Active _ _ (nta:_) _ _ _ _) = ((p, updateGroup nta item a), True)
    update (p, a) _ = ((p, a), True)

initialPrediction :: forall nt t wt. (Hashable nt, Eq nt, Eq t, Semiring wt)
                  => [t]
                  -> [(Rule nt t, wt)]
                  -> Map.HashMap nt wt
                  -> DeductiveRule (Item nt t wt) wt (Container nt t wt)
initialPrediction word srules insides = DeductiveRule 0 gets app
  where
    gets :: Container nt t wt -> Item nt t wt -> [[Item nt t wt]]
    gets _ _ = [[]]
    
    app :: Container nt t wt 
        -> [Item nt t wt] 
        -> [(Item nt t wt, wt)]
    app _ [] =  [ (Active r w as 0 [] fw inside, inside)
                | (r@(Rule ((_, as), f)), w) <- srules
                , fw <- instantiate word f
                , let inside = w <.> foldl (<.>) one (map (insides Map.!) as)
                ]
    app _ _ = []

-- | Constructs deductive rules using one rule of a grammar.
-- * prediction: initializes an active item without using antecendent items
predictionRule :: forall nt t wt. (Eq t, Eq nt, Hashable nt, Semiring wt) 
               => [t] 
               -> [(Rule nt t, wt)]
               -> Map.HashMap nt wt
               -> Map.HashMap nt wt
               -> DeductiveRule (Item nt t wt) wt (Container nt t wt)
predictionRule word rs insides outsides = DeductiveRule 1 gets app
  where
    gets :: Container nt t wt -> Item nt t wt -> [[Item nt t wt]]
    gets _ i@(Active _ _ (_:_) _ _ _ _) = [[i]]
    gets _ _ = []
    
    app :: Container nt t wt 
        -> [Item nt t wt] 
        -> [(Item nt t wt, wt)]
    app _ [Active _ _ (a:_) _ _ _ _] = [ (Active r' w' as' 0 [] fw inside, inside <.> outside)
                                       | (r'@(Rule ((a', as'), f')), w') <- rs
                                       , a' == a
                                       , fw <- instantiate word f'
                                       , let inside = w' <.> foldl (<.>) one (map (insides Map.!) as')
                                             outside = outsides Map.! a
                                       ]
    app _ _ = []


-- | Constructs deductive rules using one rule of a grammar.
-- * conversion: converts an active item into a passive one, if there are no variables left
conversionRule :: forall nt t wt. (Semiring wt, Eq nt, Hashable nt) 
               => Map.HashMap nt wt
               -> DeductiveRule (Item nt t wt) wt (Container nt t wt)
conversionRule outsides = DeductiveRule 1 gets app
  where
    gets :: Container nt t wt -> Item nt t wt -> [[Item nt t wt]]
    gets _ i@(Active _ _ [] _ _ _ _ ) = [[i]]
    gets _ _ = []

    app :: Container nt t wt -> [Item nt t wt] -> [(Item nt t wt, wt)]
    app _ [Active r w [] _ rss fs inside] = [ (Passive a rv' (C.Backtrace r w (reverse rss)) inside, inside <.> outside)
                                            | rv' <- maybeToList $ mapM ((>>= toRange) . concVarRange) fs >>= fromList
                                            , let (Rule ((a,_),_)) = r
                                                  outside = outsides Map.! a
                                            ]
    app _ _ = []


-- | Constructs deductive rules using one rule of a grammar.
-- * completion: step-by-step substituting of variables in instantiated function using ranges of passive items
completionRule :: forall nt wt t. (Hashable nt, Eq nt, Weight wt)
               => Map.HashMap nt wt
               -> Map.HashMap nt wt
               -> DeductiveRule (Item nt t wt) wt (Container nt t wt)
completionRule insides outsides = DeductiveRule 2 gets app
  where
    gets :: Container nt t wt -> Item nt t wt -> [[Item nt t wt]]
    gets (passives, _)  i@(Active _ _ (next:_) _ _ _ _) =  [ [i, passive]
                                                           | passive <- C.lookupWith Passive passives next
                                                           ]
    gets (_, actives) i@(Passive next _ _ _) = [ [active, i]
                                               | active <- Map.lookupDefault [] next actives
                                               ]
    gets _ _ = []

    app :: Container nt t wt -> [Item nt t wt] -> [(Item nt t wt, wt)]
    app _ [Active r w (a:as) offset rss fs ain, Passive _ rv _ pin] = [ (Active r w as (offset+1) (rv:rss) fs' inside, inside <.> outside)
                                                                      | fs' <- maybeToList $ mapM concVarRange $ insert offset rv fs
                                                                      , let inside = ain <.> (pin </> (insides Map.! a))
                                                                            (Rule ((s,_),_)) = r 
                                                                            outside = outsides Map.! s
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