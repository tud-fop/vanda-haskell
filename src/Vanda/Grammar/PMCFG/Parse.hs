-- (c) 2016 Tobias Denkinger <Tobias.Denkinger@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

module Vanda.Grammar.PMCFG.Parse
  ( parse ) where

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import Data.Interner (emptyInterner, inMap, internList)
import Data.Maybe (maybeToList)
import Data.Tree
import Vanda.Grammar.PMCFG
import Vanda.Grammar.AutomataStorage
import Vanda.Grammar.AutomataStorage.SparseTreeStackStorage

parse :: (Ord t, Ord nt, Hashable t, Hashable nt) => PMCFG nt t -> [t] -> [Tree (State (Rule nt t))]
parse g = map (fmap (fmap f) . fromSparseTree . fromTreeStack . storage) . runAutomaton automaton
  where (automaton, f) = fromPMCFG g


data State a = Box | RuleName a | RulePos a Int Int deriving (Eq, Ord, Show)

instance Functor State where
  fmap _ Box = Box
  fmap f (RuleName a) = RuleName (f a)
  fmap f (RulePos a i j) = RulePos (f a) i j

type AutomatonStorage    a = TreeStack Int (State a)
type AutomatonTransition a t = Transition (State a) (AutomatonStorage a) t

fromPMCFG :: (Eq nt, Eq t, Hashable nt, Hashable t) => PMCFG nt t -> (Automaton (State Int) (AutomatonStorage Int) t, Int -> Rule nt t)
fromPMCFG (PMCFG ints rules)
  = (Automaton (Box, emptyTreeStack Box) τs ((== Box) . fst), f)
  where
    rs = HM.toList . inMap . fst $ internList emptyInterner rules
    f i = fst . head $ filter ((== i) . snd) rs
    τs = concat
         [ [initialize r, suspend r 0 (length (head uss) - 1) Box]
         | (Rule ((q, _), uss), r) <- rs
         , q `elem` ints
         ]
      ++ [ readTrans r α i j
         | (Rule (_, varts), r) <- rs
         , (i, vartsi) <- zip [0 ..] varts
         , (j, symb  ) <- zip [0 ..] vartsi
         , α <- maybeToList $ fromT symb
         ]
      ++ concat
         [ [call r i j l m r', resume r i j l m r', suspend r' m (length (uss !! m) - 1) (RulePos r i j)]
         | (Rule ((_, nts), varts), r) <- rs
         , (i, vartsi) <- zip [0 ..] varts
         , (j, symb  ) <- zip [0 ..] vartsi
         , (l, m) <- maybeToList $ fromVar symb
         , (Rule ((q', _), uss), r') <- rs
         , q' == nts !! l
         ]

initialize :: a -> AutomatonTransition a t
initialize r
  = Transition
      Box
      []
      (const True)
      (pushTreeStack 0 Box)
      (RulePos r 0 (-1))

readTrans :: a -> t -> Int -> Int -> AutomatonTransition a t
readTrans r α i j
  = Transition
      (RulePos r i (j - 1))
      [α]
      (const True)
      (:[])
      (RulePos r i j)

-- l == length $ snd r'
suspend :: (Eq a) => a -> Int -> Int -> State a -> AutomatonTransition a t
suspend r' m l q
  = Transition
    (RulePos r' m l)
    []
    (checkTreeStack (== q))
    (concatMap downTreeStack . stayTreeStack (const [RuleName r']))
    q

call :: a -> Int -> Int -> Int -> Int -> a -> AutomatonTransition a t
call r i j κ m r'
  = Transition
      (RulePos r i (j - 1))
      []
      (const True)
      (pushTreeStack κ (RulePos r i j))
      (RulePos r' m (-1))

resume :: (Eq a) => a -> Int -> Int -> Int -> Int -> a -> AutomatonTransition a t
resume r i j κ m r'
  = Transition
      (RulePos r i (j - 1))
      []
      (const True)
      (concatMap (stayTreeStack $ const [RulePos r i j]) . filter (checkTreeStack (== RuleName r'))
                                                         . upTreeStack κ )
      (RulePos r' m (-1))
