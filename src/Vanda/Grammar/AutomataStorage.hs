#!/usr/bin/env runhaskell

{-|
Module:      Vanda.Grammar.AutomataStorage
Description: functions to work with /automata with storage/
Copyright:   Ⓒ Tobias Denkinger, 2015
Maintainer:  Tobias.Denkinger@tu-dresden.de
Stability:   experimental

This module contains functions to work with /automata with storage/ [close to the notation of Luisa Herrmann and Heiko Vogler: A Chomsky-Schützenberger Theorem for Weighted Automata with Storage, 2015].
-}
module Vanda.Grammar.AutomataStorage
  ( Automaton
  , runAutomaton
-- * plumbing
  , Transition
  , Configuration
-- * examples
  , exampleTrivAutomaton
  ) where

import qualified Data.Map as M
import qualified Data.Set as S

-- | An 'Automaton' contains an initial 'Configuration', a set of
--   'Transition's, and a predicate to distinguish accepting configurations.
type Automaton q σ s = (Configuration q s, [Transition q σ s], Configuration q s -> Bool)

-- | A simple automaton without storage (i.e., with the trivial storage '()')
--   and three states [cf. <http://infolab.stanford.edu/~ullman/ialc/slides/slides2.pdf>]
exampleTrivAutomaton :: Automaton Int Char ()
exampleTrivAutomaton = ((0, ()), τs, flip S.member final . fst)
  where final = S.singleton 2
        τs = [ (q, σ, const True, const [()], q')
             | (q, σ, q') <- [(0,'0',0), (0,'1',1), (1,'0',0), (1,'1',2), (2,'0',2), (2,'1',2)]
             ]

-- | Runs a given 'Automaton' on a given word and returns the 'L.List' of
--   accepting 'Configuration's.
runAutomaton
  :: (Ord q, Ord σ)
  => Automaton q σ s
  -> [σ]
  -> [Configuration q s]
runAutomaton (c, τs, f) = filter f
                        . runTransitions
                            (flip (M.findWithDefault [])
                               . M.fromListWith (++)
                               $ map (\ τ@(p, σ, _, _, _) -> ((p,σ), [τ])) τs
                            ) c

-- | A 'Transition' contains a source state, a symbol, a (unary) predicate (on
--   the storage), a (unary) nondeterministic function (on the storage), and a
--   target state.
type Transition q σ s = (q, σ, (s -> Bool), (s -> [s]), q)

-- | A 'Configuration' contains the automatons current state, and the current
--   value of the storage.
type Configuration q s = (q, s)

-- | Applies the given 'Transition' to the given 'Configuration'.
apply
  :: Configuration q s            -- ^ 'Configuration' before the 'Transition'
  -> Transition q σ s                  -- ^ 'Transition' that is being applied
  -> [Configuration q s]           -- ^ 'Configuration' after the 'Transition'
apply (_, s) (_, _, p, f, q)
  = [ (q, s') | p s, s' <- f s ]

-- | Reads the given word applying a 'L.List' of 'Transition's starting from
--   the given 'Configuration'.
runTransitions
  :: ((q, σ) -> [Transition q σ s])
                       -- ^ 'L.List' of 'Transition's for some terminal symbol
  -> Configuration q s                            -- ^ initial 'Configuration'
  -> [σ]                                                  -- ^ word to be read
  -> [Configuration q s]               -- ^ 'L.List' of final 'Configuration's
runTransitions τs = foldl step . return
  where step cs σ = do c@(q, _) <- cs
                       τ <- τs (q, σ)
                       apply c τ
