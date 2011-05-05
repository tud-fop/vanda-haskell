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

{-- snippet types --}
module Data.WSA(Transition(Transition), transTerminal, transStateIn,
    transStateOut, transWeight, WSA, states, transitions, initialWeights,
    finalWeights, create, fromList, fromListCyclic, justTerminals) where

import Tools.FastNub(nub)

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


fromList :: (Num w) => w -> [t] -> WSA Int t w
fromList w ts
  = let l = length ts
    in WSA
        [0 .. l]
        (zipWith3 (\t i j -> Transition t i j 1) ts [0 ..] [1 ..])
        [(0, w)]
        [(l, 1)]


fromListCyclic :: (Num w) => [t] -> WSA Int t w
fromListCyclic ts
  = let l = length ts
    in WSA
        (0:[1 .. l-1])
        (zipWith3 (\t i j -> Transition t i j 1) ts [0 ..] ([1 .. l-1] ++ [0]))
        [(0, 1)]
        [(0, 1)]


justTerminals :: WSA p t w -> WSA p (Maybe t) w
justTerminals wsa
  = wsa { transitions = 
              map (\t -> t {transTerminal = Just (transTerminal t)})
                  (transitions wsa)
        }
