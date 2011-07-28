-- (c) 2011 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

-- | This module implements a data structure for weighted context-free
-- grammars.
module Data.WCFG (
-- * Types
  Production()
, WCFG()
-- * Construction
, production
, wcfg, wcfgFromHypergraph
-- * Decomposition
-- ** Decomposition of 'Production's
, pLeft, pRight, pWeight, pId
-- ** Decomposition of 'WCFG's
, initial
, productions, productionsHypergraph
, terminals, nonterminals
-- * Properties of 'WCFG's and related structures
, cnf
, validProductionHypergraph
-- * Pretty Printing
, drawWCFG
) where


import Data.Hypergraph
import Tools.FastNub

import Data.Either (lefts, rights)


data Production v t w i = Production
  { pLeft   :: v
  , pRight  :: [Either v t]
  , pWeight :: w
  , pId     :: i
  } deriving (Read, Show)


data WCFG v t w i = WCFG
  { -- | Get the initial nonterminal of a 'WCFG'.
    initial               :: v
  , -- | Get the 'Hypergraph' which represents the 'Production's of a 'WCFG'.
    productionsHypergraph :: Hypergraph v [Either v t] w i
  } deriving (Read, Show)


-- | Create a 'WCFG' from an initial nonterminal and a list of 'Production's.
wcfg :: (Ord v) => v -> [Production v t w i] -> WCFG v t w i
wcfg ini ps
  = WCFG ini
  $ hypergraph
      [ hyperedge l (lefts r) r w i
      | Production l r w i <- ps
      ]


-- | Create a 'WCFG' from an initial nonterminal and a 'Hypergraph'
-- representing the 'Production's of the 'WCFG'.
-- It is /not/ checked, if the given 'Hypergraph' is valid (see
-- 'validProductionHypergraph').
wcfgFromHypergraph :: v -> Hypergraph v [Either v t] w i -> WCFG v t w i
wcfgFromHypergraph i g = WCFG i g


-- | Create a 'Production'.
production
  :: v             -- ^ left hand side nonterminal
  -> [Either v t]  -- ^ list of right hand side symbols. Terminals are
                   --   encapsulated in 'Right', nonterminals in 'Left'.
  -> w             -- ^ weight
  -> i             -- ^ id
  -> Production v t w i
production = Production

-- ---------------------------------------------------------------------------

-- | Get a list of all 'Production's of a 'WCFG'.
productions :: WCFG v t w i -> [Production v t w i]
productions
  = map (\ e -> Production (eHead e) (eLabel e) (eWeight e) (eId e))
  . edges
  . productionsHypergraph


-- | Get a list of all terminals of a 'WCFG'.
terminals ::  (Ord t) => WCFG v t w i -> [t]
terminals g
  = nub
  . concatMap (rights . eLabel)
  . edges
  $ productionsHypergraph g


-- | Get a list of all nonterminals of a 'WCFG'.
nonterminals :: (Eq v) => WCFG v t w i -> [v]
nonterminals g
  = let i   = initial g
        nts = vertices $ productionsHypergraph g
    in if elem i nts
    then nts
    else i : nts


-- | Test if the given 'Hypergraph' is a valid representation for a list of
-- 'Production's, i. e. @eTail e == lefts (eLabel e)@ for every edge @e@.
validProductionHypergraph :: (Eq v) => Hypergraph v [Either v t] w i -> Bool
validProductionHypergraph g
  = all (\ e -> eTail e == lefts (eLabel e))
  . edges
  $ g


-- | 'True' iff the given 'WCFG' is in Chomsky Normal Form and epsilon-free.
cnf :: (Eq v) => WCFG v t w i -> Bool
cnf g
  = all
      (\ p ->
        case pRight p of
          []               -> pLeft p == initial g
          [Right _]        -> True
          [Left _, Left _] -> True
          _                -> False
      )
  $ productions g


-- | Pretty print a 'WCFG'.
drawWCFG ::  (Show v, Show t, Show w, Show i) => WCFG v t w i -> [Char]
drawWCFG g
  = "initial: " ++ show (initial g) ++ "\n"
  ++ drawHypergraph (productionsHypergraph g)
