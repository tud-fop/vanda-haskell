{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Grammar.PMCFG.Wip.CYKParserv2 where

import Vanda.Grammar.PMCFG.Wip.DeductiveSolverv2
import Vanda.Grammar.PMCFG.Wip.Weights
import Vanda.Grammar.PMCFG.Range
import Vanda.Grammar.PMCFG

import Data.Hashable (Hashable(hashWithSalt))
import qualified Data.HashMap.Lazy as Map
import Data.List (partition)
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
                          , Map.HashMap nt wt           -- ^ inside weights for each nonterminal
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
                                      $ DeductiveSolver (Map.empty, Map.empty, Map.empty) update deductiveRules bw
  where
    resultFilter (Passive a rho _ _) = (a `elem` s) && (rho == singleton (entire word))

    deductiveRules = completion word : (map initialPrediction ins) ++ (map prediction notins)
    (ins, notins) = partition (\ (Rule ((a,_),_), _) -> a `elem` s) rs

    update :: (Eq nt, Hashable nt, Ord wt)
           => Container nt t wt
           -> (Item nt t wt)
           -> Container nt t wt
    update (passives, actives, insides) item@(Passive a _ _ w) = ( updateGroup a item passives
                                                                 , actives
                                                                 , Map.alter updateins a insides
                                                                 )
      where 
        updateins Nothing = Just w
        updateins (Just w')
          | w' >= w = Just w'
          | otherwise = Just w
    update (passives, actives, insides) item@(Initial (Rule ((_, as), _), _) _ _) = ( passives
                                                                                    , updateGroups as item actives
                                                                                    , insides
                                                                                    )
                  
initialPrediction :: forall nt t wt. (Eq nt, Hashable nt, Monoid wt, Ord wt) 
                  => (Rule nt t, wt) 
                  -> DeductiveRule (Item nt t wt) wt (Container nt t wt)
initialPrediction r@(Rule ((s, as), f), w) = DeductiveRule 0 gets app
  where
    gets :: (Container nt t wt) -> Item nt t wt -> [[Item nt t wt]]
    gets _ _ = [[]]
    
    app :: (Eq nt, Hashable nt, Monoid wt) => (Container nt t wt) -> [Item nt t wt] -> [(Item nt t wt, wt)]
    app (_, _, insides) [] = [(Initial r inside outside, inside <> outside)]
      where
        inside = w <> mconcat (map (insides Map.!) as)
        outside = mempty
    
prediction :: forall nt t wt. (Eq nt, Hashable nt, Group wt) 
           => (Rule nt t, wt) 
           -> DeductiveRule (Item nt t wt) wt (Container nt t wt)
prediction r@(Rule ((a, as), f), w) = DeductiveRule 1 gets app
  where
    gets :: (Eq nt) => (Container nt t wt) -> Item nt t wt -> [[Item nt t wt]]
    gets _ i@(Initial (Rule ((_, as'), _), _) _ _)
      | a `elem` as' = [[i]]
      | otherwise = []
    
    app :: (Eq nt, Hashable nt, Group wt) => (Container nt t wt) -> [Item nt t wt] -> [(Item nt t wt, wt)]
    app (_, _, insides) [Initial _ inside' outside'] = [(Initial r inside outside, inside <> outside)]
        where
          inside = w <> mconcat (map (insides Map.!) as)
          outside = inside' <> invert (insides Map.! a) <> outside'
      
completion :: forall nt t wt. (Eq t, Eq nt, Hashable nt, Monoid wt) 
           => [t] 
           -> DeductiveRule (Item nt t wt) wt (Container nt t wt)
completion word = DeductiveRule 3 gets app
  where
    gets :: (Eq nt, Hashable nt) => (Container nt t wt) -> Item nt t wt -> [[Item nt t wt]]
    gets (passives, _, _) i@(Initial (Rule ((_, as), _), _) inside' outside') = [ i:candidates
                                                                                | candidates <- mapM (passives Map.!) as
                                                                                ]
    gets (passives, actives, _) i@(Passive a _ _ _) = [ active:candidates
                                                      | active@(Initial (Rule ((_, as), _), _) _ _) <- Map.lookupDefault [] a actives
                                                      , candidates <- filter (any (== i)) $ mapM (passives Map.!) as
                                                      ]
                                                    
    app :: (Eq t, Monoid wt) => (Container nt t wt) -> [Item nt t wt] -> [(Item nt t wt, wt)]
    app _ (Initial (r@(Rule ((a, _), f)) , w) _ outside : pas) =  [ (Passive a rv (node r ds) inside, inside <> outside)
                                                                  | rv <- mapMaybe (insert rvs) $ instantiate word f
                                                                  , inside <- return $ mconcat ws <> w
                                                                  ]
      where
        (rvs, ds, ws) = foldr (\ (Passive _ rv t w) (rvs', ts', ws') -> (rv:rvs', t:ts', w:ws')) ([], [], []) pas
        insert :: [Rangevector] -> InstantiatedFunction -> Maybe Rangevector
        insert rvs' = (>>= fromList) . mapM ((>>= toRange) . concVarRange . map (insert' rvs'))
          
        insert' :: [Rangevector] -> VarT Range -> VarT Range
        insert' rvs' (Var x y)  = T $ rvs' !! x ! y
        insert' _ r' = r'