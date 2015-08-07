#!/usr/bin/env runhaskell

{-# LANGUAGE RecordWildCards #-}

{-|
Module:      Vanda.Dyck.MultipleDyckLanguages
Description: functions to work with /congruence multiple Dyck languages/
Copyright:   Ⓒ Toni Dietze and Tobias Denkinger, 2015
Maintainer:  Tobias.Denkinger@tu-dresden.de
Stability:   experimental

This module contains functions to work with /congruence multiple Dyck languages/ [cf. <http://fsmnlp2015.phil.hhu.de/wp-content/uploads/2015/06/denkinger-chomsky-schuetzenberger.pdf Tobias Denkinger: A Chomsky-Schützenberger result for weighted multiple context-free languages, 2015>].
-}
module Vanda.Dyck.MultipleDyckLanguages
  ( isMultipleDyck
  , multipleDyckTreeStackAutomaton
-- * stack tree storage
  , TreeStack
  , emptyTreeStack
-- ** predicates
  , checkTreeStack
  , bottomTreeStack
-- ** functions
  , pushTreeStack
  , downTreeStack
  , upTreeStack
  , stayTreeStack
  ) where

import Control.Monad.State
import qualified Data.List.Split as LS
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Tree

import Vanda.Grammar.AutomataStorage

-- | Checks whether a string is in the multiple Dyck language given by the
--   given partitioning of parentheses using an 'Automaton' with 'TreeStack'-
--   storage.
isMultipleDyck
  :: (Eq a, Ord a)
  => [a]                                        -- ^ list of separator symbols
  -> [a]               -- ^ list of left parentheses partitioned by separators
  -> [a]                                -- ^ list of right parentheses symbols
  -> [a]                             -- ^ word whose membership is in question
  -> Bool
isMultipleDyck sep l r
  = let b = (M.!) ( M.fromList
                  $ zip (filter (not . (`elem` sep)) l)
                        (filter (not . (`elem` sep)) r)
                  )
        ass = LS.splitOneOf sep l
    in  not . null . runAutomaton (multipleDyckTreeStackAutomaton ass b)

-- | An automaton with 'TreeStack'-storage for the recognition of multiple
--   Dyck languages.
multipleDyckTreeStackAutomaton
  :: (Eq a, Ord a)
  => [[a]]                                  -- ^ partition of left parentheses
  -> (a -> a)                                   -- ^ right parentheses mapping
  -> Automaton () a (TreeStack (Maybe (Maybe a, S.Set a)))
multipleDyckTreeStackAutomaton ass bij = (((), emptyTreeStack Nothing), τs, bottomTreeStack . snd)
  where as = concat ass
        bs = map bij as
        τs = [ ((), b, p, popTreeStack, ())
             | b <- bs
             , let p = checkTreeStack
                         (((== Just b) . fst . fromJust)
                          &&& (S.null . snd . fromJust))
             ]
          ++ [ ((), b, p, f, ())
             | b <- bs
             , let p = checkTreeStack
                         (((== Just b) . fst. fromJust)
                          &&& (not . S.null . snd . fromJust))
             , let f = stayTreeStack (\ x -> case x of {(Just (_, s)) -> [Just (Nothing, s)]; Nothing -> []})
                         >=> downTreeStack
             ]
          ++ [ ((), a, const True, f, ())
             | a <- as
             , let {p (Just (Nothing, s)) = a `S.member` s; p _ = False}
             , let f = stayTreeStack (\ (Just (Nothing, s)) -> [Just (Just (bij a), S.delete a s)])
                         <=< filter (checkTreeStack p)
                         . upTreeStack
             ]
          ++ [ ((), a, const True, f, ())
             | a <- as
             , let f = pushTreeStack (Just
                        ( Just (bij a)
                        , S.fromList $ head [L.delete a as' | as' <- ass, a `elem` as'])
                        )
             ]
        (f &&& g) x = f x && g x


-- | 'Tree' plus stack pointer; the bottom of the stack is the root of the tree.
--   The data structure has the form [(cₙ, tₙ), …, (c₁, t₁)] where cᵢ are, intuitively,
--   contexts and tᵢ are trees. The tree stack can be obtaines from that data structure
--   as follows: the tree is cₙ . … . c₂ $ c₁ t₁ and the pointer points to the root of
--   t₁. The data structure optimises the expensive operation of "going to a specific
--   position in the tree".
newtype TreeStack a = TreeStack [(Tree a -> Tree a, Tree a)]


emptyTreeStack :: a -> TreeStack a
emptyTreeStack x = TreeStack [(id, Node x [])]


-- | Checks whether the node at the stack pointer fulfills a certain predicate.
checkTreeStack :: (a -> Bool) -> TreeStack a -> Bool
checkTreeStack _ (TreeStack []) = error "checkTreeStack: the stack should never be empty"
checkTreeStack p (TreeStack ((_, Node x _) : _)) = p x


-- | Checks whether the tree only has a root node.
bottomTreeStack :: TreeStack a -> Bool
bottomTreeStack (TreeStack [(f, ξ)])
  | L.null . subForest $ f ξ = True
  | otherwise                = False
bottomTreeStack _            = False


-- | Adds the given stack symbol above the current stack pointer.
pushTreeStack :: a -> TreeStack a -> [TreeStack a]
pushTreeStack _ (TreeStack []) = error "pushTreeStack: the stack should never be empty"
pushTreeStack x (TreeStack cs@((_, Node a ts) : _))
  = [ TreeStack $ (\ t' -> Node a (t' : ts), Node x []) : cs ]


-- | Removes the node of the tree currently under the stack pointer (if that tree is a
--   leaf) and moves the stack pointer to the parent of its previous position.
popTreeStack :: TreeStack a -> [TreeStack a]
popTreeStack (TreeStack []) = error "popTreeStack: the stack should never be empty"
popTreeStack (TreeStack ((_, Node _ []) : cs)) = [TreeStack cs]
popTreeStack _ = []


-- | Moves the stack pointer to the parent node of its current position.
downTreeStack :: TreeStack a -> [TreeStack a]
downTreeStack (TreeStack []) = error "downTreeStack: the stack should never be empty"
downTreeStack (TreeStack [_]) = []
downTreeStack (TreeStack ((f0, t0) : (f1, _) : ts))
  = [ TreeStack $ (f1, f0 t0) : ts ]


-- | (Nondeterministically) moves the stack pointer to the child nodes.
upTreeStack :: TreeStack a -> [TreeStack a]
upTreeStack (TreeStack []) = error "upTreeStack: the stack should never be empty"
upTreeStack (TreeStack ((f, Node a ts) : cs))
  = [ TreeStack $ (Node a . g, t) : (f, Node a ts') : cs | (g, t, ts') <- contexts ts ]


contexts :: [a] -> [(a -> [a], a, [a])]
contexts [] = []
contexts [x] = return (return, x, [])
contexts (x : xs) = ((: xs), x, xs) : map (\ (f, y, ys) -> ((x :) . f, y, x:ys)) (contexts xs)


-- | Applies a function at the node below the stack pointer.
stayTreeStack :: (a -> [a]) -> TreeStack a -> [TreeStack a]
stayTreeStack _ (TreeStack []) = error "stayTreeStack: the stack should never be empty"
stayTreeStack g (TreeStack ((f, Node a ts) : cs))
  = [ TreeStack $ (f, Node a' ts) : cs | a' <- g a ]
