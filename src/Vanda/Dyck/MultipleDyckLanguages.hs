{-|
Module:      Vanda.Dyck.MultipleDyckLanguages
Description: functions to work with /congruence multiple Dyck languages/
Copyright:   Ⓒ Tobias Denkinger, 2015
Maintainer:  Tobias.Denkinger@tu-dresden.de
Stability:   experimental

This module contains functions to work with /congruence multiple Dyck languages/ [cf. <http://fsmnlp2015.phil.hhu.de/wp-content/uploads/2015/06/denkinger-chomsky-schuetzenberger.pdf Tobias Denkinger: A Chomsky-Schützenberger result for weighted multiple context-free languages, 2015>].
-}
module Vanda.Dyck.MultipleDyckLanguages
  ( isMultipleDyck
  , multipleDyckTreeStackAutomaton
  ) where

import Control.Arrow ((***))
import Control.Monad.State ((>=>), (<=<))
import Data.List.Split (splitOneOf)
import Data.Map ((!), fromList)
import qualified Data.Set as S (Set, delete, fromList, member, null, empty)

import Vanda.Grammar.AutomataStorage
import Vanda.Grammar.AutomataStorage.TreeStackStorage

-- | Checks whether a string is in the multiple Dyck language given by the
--   given partitioning of parentheses using an 'Automaton' with 'TreeStack'-
--   storage.
isMultipleDyck
  :: Ord a
  => [a]                                        -- ^ list of separator symbols
  -> [a]               -- ^ list of left parentheses partitioned by separators
  -> [a]       -- ^ list of right parentheses symbols (separators are ignored)
  -> [a]                             -- ^ word whose membership is in question
  -> Bool
isMultipleDyck sep l r
  = let b = (!) ( fromList
                  $ zip (filter (not . (`elem` sep)) l)
                        (filter (not . (`elem` sep)) r)
                  )
        ass = splitOneOf sep l
    in  not . null . runAutomaton (multipleDyckTreeStackAutomaton ass b)

-- | An automaton with 'TreeStack'-storage for the recognition of multiple
--   Dyck languages.
multipleDyckTreeStackAutomaton
  :: Ord a
  => [[a]]                                  -- ^ partition of left parentheses
  -> (a -> a)                    -- ^ bijection from left to right parentheses
  -> Automaton () (TreeStack (Maybe a, S.Set a)) a
multipleDyckTreeStackAutomaton ass bij
  = Automaton
      ((), emptyTreeStack (Nothing, S.empty))
      τs
      (bottomTreeStack . snd)
  where as = concat ass
        bs = map bij as
        τs = [ Transition () [b] p popTreeStack ()
             | b <- bs
             , let p = checkTreeStack
                     $ uncurry (&&)
                       . ((== Just b) *** S.null)
             ]
          ++ [ Transition () [b] p f ()
             | b <- bs
             , let p = checkTreeStack
                     $ uncurry (&&)
                       . ((== Just b) *** (not . S.null))
             , let f = stayTreeStack
                         (\ (_, s) -> [(Nothing, s)] )
                         >=> downTreeStack
             ]
          ++ [ Transition () [a] (const True) f ()
             | a <- as
             , let p (Nothing, s) = a `S.member` s
                   p _            = False
             , let f = stayTreeStack (\ (Nothing, s) -> [(Just (bij a), S.delete a s)])
                         <=< filter (checkTreeStack p)
                         . upTreeStack
             ]
          ++ [ Transition () [a] (const True) f ()
             | a <- as
             , let f = pushTreeStack
                       ( Just (bij a)
                       , S.delete a
                         . S.fromList
                         $ head [ as' | as' <- ass, a `elem` as']
                       )
             ]

