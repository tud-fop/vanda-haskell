-- (c) 2010 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

-- |
-- Maintainer  :  Toni Dietze
-- Stability   :  unknown
-- Portability :  portable

{-- snippet types --}
module Vanda.Algorithms.Earley.WSA
(   
    -- * Types 
    Transition(Transition)
  , WSA
    -- * Construction
  , create
  , fromList
  , fromListCyclic
    -- * Decomposition
  , transTerminal
  , transStateIn
  , transStateOut
  , transWeight
  , states
  , transitions
  , initialWeights
  , finalWeights
    -- * Transformation
  , justTerminals
) where

import Vanda.Algorithms.Earley.FastNub(nub)

import Control.DeepSeq


data Transition state terminal weight = Transition
    { transTerminal :: terminal   
    , transStateIn  :: state
    , transStateOut :: state
    , transWeight   :: weight
    } deriving (Eq, Ord, Show)

data WSA state terminal weight = WSA
    { states         :: [state]
    , transitions    :: [Transition state terminal weight]
    , initialWeights :: [(state, weight)]
    , finalWeights   :: [(state, weight)]
    } deriving Show

-- | Create a 'WSA' from a 'List' of 'Transition's, initial weights and final weights.
create ::
  (Ord p) => [Transition p t w] -> [(p, w)] -> [(p, w)] -> WSA p t w
create ts is fs
  = let ss = nub $
             map fst is ++
             map fst fs ++
             map transStateIn ts ++
             map transStateOut ts
    in WSA ss ts is fs
{-- /snippet types --}

-- | Create a 'WSA' from a word (list of terminals).
fromList :: (Num w) => w -> [t] -> WSA Int t w
fromList w ts
  = let l = length ts
    in WSA
        [0 .. l]
        (zipWith3 (\t i j -> Transition t i j 1) ts [0 ..] [1 ..])
        ((0, w): [] {-(zip [1 .. l] (repeat 0))-})
        ({-(zip [0 .. l-1] (repeat 0)) ++-} [(l, 1)])

-- | Create a 'WSA' from a word (list of terminals). The 'WSA' represents the Kleene-Star of the word.
fromListCyclic :: (Num w) => [t] -> WSA Int t w
fromListCyclic ts
  = let l = length ts
    in WSA
        (0:[1 .. l-1])
        (zipWith3 (\t i j -> Transition t i j 1) ts [0 ..] ([1 .. l-1] ++ [0]))
        [(0, 1)]
        [(0, 1)]

-- | transform a WSA to an equivalent WSA where the transitions are a 'Maybe'-Type
justTerminals :: WSA p t w -> WSA p (Maybe t) w
justTerminals wsa
  = wsa { transitions = 
              map (\t -> t {transTerminal = Just (transTerminal t)})
                  (transitions wsa)
        }

-- ---------------------------------------------------------------------------

instance (NFData p, NFData t, NFData w) => NFData (Transition p t w) where
  rnf (Transition t s s' w) = rnf t `seq` rnf s `seq` rnf s' `seq` rnf w

instance (NFData p, NFData t, NFData w) => NFData (WSA p t w) where
  rnf (WSA ss ts is fs) = rnf ts `seq` rnf is `seq` rnf fs `seq` rnf ss
