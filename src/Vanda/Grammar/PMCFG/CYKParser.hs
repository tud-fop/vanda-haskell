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
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Grammar.PMCFG.CYKParser 
  ( weightedParse
  , parse
  ) where


import Data.Converging (Converging)
import Data.Hashable (Hashable(hashWithSalt))
import Data.Maybe (maybeToList, catMaybes)
import Data.Range
import Data.Semiring
import Data.Tree (Tree)
import Data.Weight
import Vanda.Grammar.PMCFG

import qualified Data.MultiHashMap          as MMap
import qualified Data.HashMap.Lazy          as Map
import qualified Vanda.Grammar.PMCFG.Chart  as C
import qualified Data.HashSet               as Set



-- | Active and passive items for cyk parser.
data Item nt t wt = Active (Rule nt t) wt InstantiatedFunction wt
                  | Passive nt Rangevector (C.Backtrace nt t wt) wt


-- | Container type contains a chart for produced passive items and a map for
-- active items that maps each rhs nonterminal of a rule to its corresponding
-- passive item.
type Container nt t wt = ( C.Chart nt t wt
                         , MMap.MultiMap nt (Item nt t wt)
                         , Set.HashSet nt
                         )


-- | Eq ignores weights, since they can be derived from the rule.
instance (Eq nt, Eq t) => Eq (Item nt t wt) where
  (Active r _ fw _) == (Active r' _ fw' _) = r == r' && fw == fw' 
  (Passive a rv bt _) == (Passive a' rv' bt' _) = a == a' && rv == rv' && bt' == bt
  _ == _ = False


instance (Hashable nt, Hashable t) => Hashable (Item nt t wt) where
    salt `hashWithSalt` (Active r _ _ _) = salt `hashWithSalt` r
    salt `hashWithSalt` (Passive a rho _ _) = salt `hashWithSalt` a `hashWithSalt` rho


instance (Show nt) => Show (Item nt t wt) where
    show (Active (Rule ((a,as),_)) _ f _)
      = "[active] " ++ show a ++ " → " ++ prettyPrintInstantiatedFunction f ++ show as
    show (Passive a rv _ _)
      = "[passive] " ++ show a ++ " → " ++ show rv 


-- | Top-level function to parse a word using a grammar.
-- The grammar rules are implicitly annotated with additive weigts to encurage
-- derivations with least rule applications.
parse :: (Eq t, Hashable nt, Hashable t, Ord nt)
      => PMCFG nt t                               -- ^ the grammar
      -> Int                                      -- ^ approximation parameter 1 &
      -> Int                                      -- ^ approximation parameter 2
      -> [t]                                      -- ^ the word
      -> [Tree (Rule nt t)]                       -- ^ list of derivation trees 
parse (PMCFG s rules) = weightedParse 
                      $ WPMCFG s 
                      $ zip rules 
                      $ repeat (cost 1 :: Cost Int)


-- | Top-level function to parse a word using a weighted grammar.
weightedParse :: forall nt t wt. (Eq t, Hashable nt, Hashable t, Semiring wt, Ord wt, Ord nt, Converging wt)
              => WPMCFG nt wt t             -- ^ weighted grammar
              -> Int                        -- ^ beam width
              -> Int                        -- ^ maximum number of returned trees
              -> [t]                        -- ^ word
              -> [Tree (Rule nt t)]         -- ^ parse trees
weightedParse (WPMCFG s rs) bw trees word
  = C.parseTrees trees s (singleton $ entire word)
  $ (\ (e, _, _) -> e)
  $ C.chartify (C.empty, MMap.empty, nset) update deductiveRules bw trees
    where
      rmap = instantiableRules word rs

      nset = Set.fromList $ filter (not . (`elem` s)) $ Map.keys rmap
      iow = ioWeights s $ MMap.elems rmap

      deductiveRules = initialPrediction word (s >>= (`MMap.lookup` rmap)) iow
                        : prediction word rmap iow
                        : [completion iow]

      update :: Container nt t wt
            -> Item nt t wt
            -> (Container nt t wt, Bool)
      update (passives, actives, ns) (Passive a rho bt iw) 
        = case C.insert passives a rho bt iw of
              (passives', isnew) -> ((passives', actives, ns), isnew)
      update (passives, actives, ns) item@(Active (Rule ((_, as), _)) _ _ _)
        = (( passives
           , foldl (flip $ uncurry MMap.insert) actives $ zip as $ repeat item
           , foldl (flip Set.delete) ns as
           ), True )


-- | Prediction rule for rules of initial nonterminals
initialPrediction :: forall nt t wt. (Eq nt, Eq t, Hashable nt, Semiring wt) 
                  => [t]
                  -> [(Rule nt t, wt)]
                  -> Map.HashMap nt (wt, wt)
                  -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
initialPrediction word srules ios 
  = Left 
  $ catMaybes [ implicitConversion (Active r w fw inside, inside)
              | (r@(Rule ((_, as), f)), w) <- srules
              , fw <- instantiate word f
              , let inside = w <.> foldl (<.>) one (map (fst . (ios Map.!)) as)
              ]


-- | Prediction rule of the cyk parser. Initializes a passive item
-- for each rhs nonterminal of a rule that needs to be applied.
prediction :: forall nt t wt. (Eq nt, Eq t, Hashable nt, Semiring wt) 
           => [t]
           -> MMap.MultiMap nt (Rule nt t, wt)
           -> Map.HashMap nt (wt, wt)
           -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
prediction word rs ios = Right app
  where
    app (Active (Rule ((_, as), _)) _ _ _) _
      = catMaybes 
        [ implicitConversion (Active r' w' fw inside, inside <.> outside)
        | (r'@(Rule ((a', as'), f')), w') <- concatMap (`MMap.lookup` rs) as
        , fw <- instantiate word f'
        , let inside = w' <.> foldl (<.>) one (map (fst . (ios Map.!)) as')
              outside = snd $ ios Map.! a'
        ]
    app _ _ = []


implicitConversion :: (Item nt t wt, wt) -> Maybe (Item nt t wt, wt)
implicitConversion (Active r@(Rule ((a, []), _)) wr fw inside, weight)
  = mapM toRange fw
  >>= fromList
  >>= (\ rho -> Just (Passive a rho (C.Backtrace r wr []) inside, weight))
implicitConversion i = Just i

-- | Completion rule of the cyk parser. Applies instantiated function
-- on a sequence of 'Rangevector's to yield a range vector.
-- The result is wrapped in a passive item.
completion :: forall nt t wt. (Eq nt, Eq t, Hashable nt, Semiring wt) 
           => Map.HashMap nt (wt, wt)
           -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
completion ios = Right app
  where
    app item@(Active (Rule ((_, as), _)) _ _ _) (ps, _, _) 
      = [ consequence
        | pas <- mapM (C.lookupWith Passive ps) as
        , consequence <- consequences (item:pas)
        ]
    app item@(Passive a _ _ _) (ps, acts, _) 
      = [ consequence
        | act@(Active (Rule ((_, as), _)) _ _ _ ) <- MMap.lookup a acts
        , pas <- filter (elem item) 
                  $ mapM (\ nta -> if nta == a 
                                   then item : C.lookupWith Passive ps nta 
                                   else C.lookupWith Passive ps nta) as
        , consequence <- consequences (act:pas)
        ]

    consequences (Active r@(Rule ((a, _), _)) w fw _: pas) 
      = [ (Passive a rv (C.Backtrace r w rvs) inside, inside <.> outside)
        | rv <- maybeToList $ insert rvs fw
        , let inside = w <.> foldl (<.>) one ws
              outside = snd $ ios Map.! a
        ]
      where
        (rvs, ws) = foldr (\ (Passive _ rv _ iw) (rvs', ws') -> (rv:rvs', iw:ws')) ([], []) pas
    consequences _ = []

-- | Substitutes variables in an instantiated function with ranges out of a
-- list of range vectors, yields a Rangevector on success.
insert :: [Rangevector] -> InstantiatedFunction -> Maybe Rangevector
insert rvs = (>>= fromList) 
            . mapM ((>>= toRange) 
            . concVarRange 
            . map (insert' rvs))
  where
    insert' :: [Rangevector] -> VarT Range -> VarT Range
    insert' rvs' (Var x y)  = T $ rvs' !! x ! y
    insert' _ r' = r'