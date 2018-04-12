#!/usr/bin/env runhaskell

{-|
Module:      Vanda.Grammar.AutomataStorage
Description: functions to work with /automata with storage/
Copyright:   Ⓒ Tobias Denkinger, 2015
License:     BSD-style
Maintainer:  Tobias.Denkinger@tu-dresden.de
Stability:   experimental

This module contains functions to work with /automata with storage/ [close to the notation of Luisa Herrmann and Heiko Vogler: A Chomsky-Schützenberger Theorem for Weighted Automata with Storage, 2015].
-}
module Vanda.Grammar.AutomataStorage
  ( Automaton (..)
  , runAutomaton
-- * plumbing
  , Transition (..)
  , Configuration (..)
-- * examples
  , exampleTrivAutomaton
  ) where

import Data.List (stripPrefix)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import qualified Data.Set as S

-- | An 'Automaton' contains an initial 'Configuration', a set of
--   'Transition's, and a predicate to distinguish accepting configurations.
data Automaton q s σ = Automaton { initial :: (q, s)
                                 , transitions :: [Transition q s σ]
                                 , accepting :: (q, s) -> Bool
                                 }

-- | A simple automaton without storage (i.e., with the trivial storage '()')
--   and three states [cf. <http://infolab.stanford.edu/~ullman/ialc/slides/slides2.pdf>]
exampleTrivAutomaton :: Automaton Int () Char
exampleTrivAutomaton = Automaton (0, ()) τs (flip S.member final . fst)
  where final = S.singleton 2
        τs = [ Transition q [σ] (const True) (const [()]) q'
             | (q, σ, q') <- [(0,'0',0), (0,'1',1), (1,'0',0), (1,'1',2), (2,'0',2), (2,'1',2)]
             ]

-- | Runs a given 'Automaton' on a given word and returns the 'L.List' of
--   accepting 'Configuration's.
runAutomaton
  :: (Ord q, Ord σ)
  => Automaton q s σ
  -> [σ]
  -> [Configuration q s σ]
runAutomaton a w
  = filter (\(Configuration q s v) -> null v && accepting a (q, s))
    . concat
    . takeWhile (not . null)
    . iterate (concatMap (runTransitions τs))
    $ [Configuration q' s' w]
  where (q', s') = initial a
        τm = M.fromListWith (++)
           . map (\τ -> (sState τ, [τ]))
           $ transitions a
        τs q = M.findWithDefault [] q τm

-- | A 'Transition' contains a source state, a symbol, a (unary) predicate (on
--   the storage), a (unary) nondeterministic function (on the storage), and a
--   target state.
data Transition q s σ = Transition { sState :: q
                                   , _word :: [σ]
                                   , _predicate :: s -> Bool
                                   , _instruction :: s -> [s]
                                   , _tState :: q
                                   }

-- | A 'Configuration' contains the automatons current state, the current value
--   of the storage, and the remaining word to be read.
data Configuration q s σ = Configuration { state :: q
                                         , storage :: s
                                         , _remainingWord :: [σ]
                                         } deriving (Show)

-- | Computes all the successor 'Configuration's reachable from the given
--   'Configuration' using the given 'Transition's (categorized by state).
runTransitions
  :: Eq σ
  => (q -> [Transition q s σ])
                       -- ^ 'L.List' of 'Transition's for some terminal symbol
  -> Configuration q s σ                          -- ^ initial 'Configuration'
  -> [Configuration q s σ]             -- ^ 'L.List' of final 'Configuration's
runTransitions τs c = concatMap (apply c) . τs $ state c
  where apply (Configuration _ s w) (Transition _ v p i q')
          = [Configuration q' s' w' | w' <- maybeToList (v `stripPrefix` w), p s, s' <- i s]
