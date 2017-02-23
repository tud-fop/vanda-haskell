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
module Vanda.Grammar.PMCFG.NaiveParser
    where
    -- ( weightedParse
    -- , parse
    -- ) where

import Vanda.Grammar.PMCFG.DeductiveSolver
import Vanda.Grammar.PMCFG.Range
import Vanda.Grammar.PMCFG

import Data.Hashable (Hashable(hashWithSalt))
import qualified Data.HashMap.Lazy as Map
import Data.Tree (Tree)
import Data.Maybe (maybeToList)
import Data.List (partition)


data Item nt t = Initial (Rule nt t, wt)      -- ^ grammar rule
                         wt                   -- ^ inside weight
                         wt                   -- ^ outside weight
               | Active (Rule nt t, wt)       -- ^ grammar rule
                        wt                    -- ^ inside weight
                        wt                    -- ^ outside weight
                        [nt]                  -- ^ q of nonterminals to complete
                        Int                   -- ^ offset for insertion
                        [Derivation nt t]     -- ^ completet derivations (reversed)
                        InstantiatedFunction  -- ^ instantiated context to insert in
               | Passive nt                   -- ^ lhs's nonterminal
                         wt                   -- ^ inside weight
                         Rangevector          -- ^ spanned subword
                         (Derivation nt t)    -- ^ derivation
                deriving (Eq, Ord)

type Container nt t = ( Map.HashMap nt [Item nt t]          -- ^ maps rules lhs non-terminal to passive items
                      , Map.HashMap (Maybe nt) [Item nt t]  -- ^ maps nt to active items with it to complete next
                      , Map.HashMap nt wt                   -- ^ maps nt to inside weight
                      )

instance (Hashable nt, Hashable t) => Hashable (Item nt t) where
    salt `hashWithSalt` (ActiveItem r _ _ ds _) = salt `hashWithSalt` r `hashWithSalt` ds
    salt `hashWithSalt` (PassiveItem a rho d) = salt `hashWithSalt` a `hashWithSalt` rho `hashWithSalt` d

instance (Show nt, Show t) => Show (Item nt t) where
    show (ActiveItem r as i _ fs) = "[active] " ++ prettyPrintRule r ++ " " ++ show as ++ "+" ++ show i ++ " " ++ prettyPrintInstantiatedFunction fs
    show (PassiveItem a rv _) = "[passive] " ++ show a ++ " " ++ show rv 

-- | Top-level function to parse a word using a PMCFG.
parse :: (Hashable nt, Hashable t, Eq nt, Eq t) 
      => PMCFG nt t         -- ^ unweighted grammar
      -> Int
      -> [t]                -- ^ terminal word
      -> [Tree (Rule nt t)] -- ^ derivation tree of applied rules
parse (PMCFG s rs) = weightedParse $ WPMCFG s $ zip rs $ repeat (cost 1 :: Cost Double)


-- | Top-level function to parse a word using a weighted PMCFG.
weightedParse :: (Hashable nt, Hashable t, Eq nt, Eq t, Ord wt, Dividable wt) 
              => WPMCFG nt wt t     -- ^ weighted grammar
              -> Int
              -> [t]                -- ^ terminal word
              -> [Tree (Rule nt t)] -- ^ derivation tree of applied rules
weightedParse (WPMCFG s rs) bw w = map (\ (PassiveItem _ _ (Derivation t)) -> t) 
                                    $ filter (resultfilter s $ singleton $ entire w)
                                    $ solve ds
    where
        ds = DeductiveSolver (Map.empty, Map.empty) update (conversionRule : (rs >>= (\ r -> predictionRule w r : completionRules r))) bw

        resultfilter :: (Eq nt) => [nt] -> Rangevector -> Item nt t -> Bool
        resultfilter start target (PassiveItem a rho _) = a `elem` start && rho == target
        resultfilter _ _ _                              = False 

        update :: (Eq nt0, Hashable nt0)
               => Container nt0 t0 
               -> [Item nt0 t0] 
               -> Container nt0 t0
        update (passives, actives) items = ( updateGroupsWith (\ (PassiveItem a _ _) -> a) passiveItems passives 
                                           , updateGroupsWith (\ (ActiveItem _ as _ _ _) -> safehead as) activeItems actives
                                           )
            where 
                (activeItems, passiveItems) = partition isActive items 
                safehead [] = Nothing
                safehead (x:_) = Just x

-- | Constructs deductive rules using one rule of a grammar.
-- * prediction: initializes an active item without using antecendent items
predictionRule :: (Eq t, Monoid wt) 
               => [t] 
               -> (Rule nt t, wt) 
               -> (DeductiveRule (Item nt t) wt (Container nt t))
predictionRule w (r@(Rule ((_, as), f)), _) = DeductiveRule 0 (\ _ _ -> [[]]) (const prediction) mempty
  where
    prediction = [ ActiveItem r as 0 [] inst 
                 | inst <- instantiate w f 
                 ]


-- | Constructs deductive rules using one rule of a grammar.
-- * conversion: converts an active item into a passive one, if there are no variables left
conversionRule :: (Monoid wt) 
               => (DeductiveRule (Item nt t) wt (Container nt t))
conversionRule = DeductiveRule 1 gets app mempty
  where
    gets :: Container nt t -> Item nt t -> [[Item nt t]]
    gets _ i@(ActiveItem _ [] _ _ _) = [[i]]
    gets _ _ = []

    app :: [Item nt t] -> [Item nt t]
    app [ActiveItem r@(Rule ((a, _), _)) [] _ ds fs] = [ PassiveItem a rv' (node r $ reverse ds)
                                                       | rv' <- maybeToList $ mapM ((>>= toRange) . concVarRange) fs >>= fromList
                                                       ]
    app _ = []


-- | Constructs deductive rules using one rule of a grammar.
-- * completion: step-by-step substituting of variables in instantiated function using ranges of passive items
completionRules :: (Hashable nt, Eq nt, Eq t, Dividable wt) 
                => (Rule nt t, wt)
                -> [(DeductiveRule (Item nt t) wt (Container nt t))]
completionRules (r@(Rule ((_, as), _)), weight) = [ DeductiveRule 2 (gets r a) app w
                                                  | (w, a) <- zip singleNTWeight as
                                                  ] 
    where
        singleNTWeight = divide weight $ length as

        gets :: (Hashable nt, Eq nt, Eq t) => (Rule nt t) -> nt -> Container nt t -> Item nt t -> [[Item nt t]]
        gets rule next (passives, _) i@(ActiveItem rule' (n:_) _ _ _) = [ [i, passive]
                                                                        | n == next
                                                                        , rule' == rule
                                                                        , passive <- Map.lookupDefault [] next passives 
                                                                        ]
        gets rule next (_, actives) i@(PassiveItem n _ _) = [ [active, i]
                                                            | n == next
                                                            , active@(ActiveItem rule' _ _ _ _) <- Map.lookupDefault [] (Just next) actives
                                                            , rule' == rule
                                                            ] 
        gets _ _ _ _ = []
        
        app :: [Item nt t] -> [Item nt t]
        app [ActiveItem rule' (_:as') offset ds fs, PassiveItem _ rv d] = [ ActiveItem rule' as' (offset+1) (d:ds) fs'
                                                                          | fs' <- maybeToList $ mapM concVarRange $ insert offset rv fs
                                                                          ]
        app _ = []


-- | substitutes variables with index 'off' with a ranges of a vector
insert :: Int -> Rangevector -> InstantiatedFunction -> InstantiatedFunction
insert off rv = map (map (substitute off rv))
    where
        substitute :: Int -> Rangevector -> VarT Range -> VarT Range
        substitute i rv' (Var i' j)
            | i' == i =  T $ rv' ! j
            | otherwise = Var i' j
        substitute _ _ r = r


isActive :: Item nt t -> Bool
isActive (PassiveItem _ _ _) = False
isActive _ = True