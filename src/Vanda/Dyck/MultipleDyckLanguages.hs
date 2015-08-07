#!/usr/bin/env runhaskell

{-# LANGUAGE RecordWildCards #-}

{-|
Module:      Vanda.Dyck.MultipleDyckLanguages
Description: functions to work with /congruence multiple Dyck languages/
Copyright:   Ⓒ Tobias Denkinger and Toni Dietze (clipList, clipTree), 2015
Maintainer:  Tobias.Denkinger@tu-dresden.de
Stability:   experimental

This module contains functions to work with /congruence multiple Dyck languages/ [cf. <http://fsmnlp2015.phil.hhu.de/wp-content/uploads/2015/06/denkinger-chomsky-schuetzenberger.pdf Tobias Denkinger: A Chomsky-Schützenberger result for weighted multiple context-free languages, 2015>].
-}
module Vanda.Dyck.MultipleDyckLanguages
  ( isMultipleDyck
  , multipleDyckTreeStackAutomaton
-- * stack tree storage
  , TreeStack
  , createTreeStack
-- ** predicates
  , checkTreeStack
  , bottomTreeStack
-- ** functions
  , pushTreeStack
  , downTreeStack
  , upTreeStack
  , stayTreeStack
-- * plumbing
  , clipList
  , clipTree
-- * examples
  , exampleTreeStack1
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
multipleDyckTreeStackAutomaton ass bij = (((), c₀), τs, bottomTreeStack . snd)
  where c₀ = createTreeStack (Node Nothing []) []
        as = concat ass
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


-- | 'Tree' plus stack pointer.
--   Children are numbered starting from 0 and, intuitively, the 'TreeStack'
--   has its root at the bottom.
data TreeStack a = TreeStack (Tree a) [Int] deriving (Eq, Show)

-- | Returns the symbol of the given tree at the given position.
symbolAt :: [Int] -> Tree a -> a
symbolAt ρ = snd . clipTree ρ

-- | Wrapper for the value constructor 'TreeStack' that checks whether the
--   given stack pointer is valid within the given 'Tree'.
createTreeStack :: Tree a -> [Int] -> TreeStack a
createTreeStack ξ ρ
  | ρ `addressIn` ξ = TreeStack ξ ρ
  | otherwise       = error $ "position does not occur in tree"
  where addressIn [] _ = True
        addressIn (i:is) (Node _ ξs)
          = i < length ξs  &&  is `addressIn` (ξs !! i)

exampleTreeStack1 :: TreeStack Char
exampleTreeStack1
  = createTreeStack
    (Node 'a' [
      Node 'b' [
        Node 'c' []
      ]
      , Node 'd' []
    ]) [0, 0]

-- | Checks whether the node at the stack pointer fulfills a certain predicate.
checkTreeStack :: (a -> Bool) -> TreeStack a -> Bool
checkTreeStack f (TreeStack ξ ρ) = f $ symbolAt ρ ξ

-- | Checks whether the tree only has a root node.
bottomTreeStack :: TreeStack a -> Bool
bottomTreeStack (TreeStack (Node _ []) _) = True
bottomTreeStack _ = False

rankAt :: Tree a -> [Int] -> Int
rankAt (Node _ ξs) []     = length ξs
rankAt (Node _ ξs) (i:is) = rankAt (ξs !! i) is

-- | Adds the given stack symbol above the current stack pointer.
pushTreeStack :: a -> TreeStack a -> [TreeStack a]
pushTreeStack σ (TreeStack ξ ρ) = [TreeStack ξ₁ (ρ ++ [i])]
  where (ξ₁, i) = trav ξ ρ σ
        trav (Node δ ξs) [] σ₁
          = (Node δ (ξs ++ [Node σ₁ []]), length ξs)
        trav (Node δ ξs) (j:js) σ₁
          = (Node δ (take j ξs ++ ξi : drop (j + 1) ξs), i')
              where (ξi, i') = trav (ξs !! j) js σ₁

-- | Removes the node of the tree currently under the stack pointer and moves
--   the stack pointer to the parent of its previous position.
popTreeStack :: TreeStack a -> [TreeStack a]
popTreeStack (TreeStack _ []) = []
popTreeStack (TreeStack ξ ρ)
  | rankAt ξ ρ == 0 = [TreeStack ξ₁ $ take (length ρ - 1) ρ]
  | otherwise       = []
  where ξ₁ = ξ `deleteAt` ρ
        deleteAt _ []
          = error "can not delete root node"
        deleteAt (Node δ ts) [i]
          = Node δ $ take i ts ++ drop (i + 1) ts
        deleteAt (Node δ ts) (i:is)
          = Node δ $ take i ts ++ deleteAt (ts !! i) is : drop (i + 1) ts

-- | Moves the stack pointer to the parent node of its current position.
downTreeStack :: TreeStack a -> [TreeStack a]
downTreeStack (TreeStack _ []) = []
downTreeStack (TreeStack ξ ρ ) = [TreeStack ξ $ take (length ρ - 1) ρ]

-- | (Nondeterministically) moves the stack pointer to the child nodes.
upTreeStack :: TreeStack a -> [TreeStack a]
upTreeStack (TreeStack ξ ρ) = [TreeStack ξ (ρ ++ [i]) | i <- [0 .. rank ξ ρ - 1]]
  where rank (Node _ ξs) [] = length ξs
        rank (Node _ ξs) (i:is) = rank (ξs !! i) is

-- | Applies a function at the node below the stack pointer.
stayTreeStack :: (a -> [a]) -> TreeStack a -> [TreeStack a]
stayTreeStack f (TreeStack ξ ρ)
  = [TreeStack (t' a') ρ | let (t', a) = clipTree ρ ξ, a' <- f a]

-- | Removes the symbol at the given position from the given 'Tree' and
--   returns a tuple containing a 'Tree' with a "hole" at the given position
--   and the label that has been removed.
clipTree :: [Int] -> Tree a -> (a -> Tree a, a)
clipTree [] (Node a ξs) = (flip Node ξs, a)
clipTree (i:is) (Node a ξs) = let (lf, ξ₁) = clipList i ξs
                                  (ξf, a₁) = clipTree is ξ₁
                              in  (Node a . lf . ξf, a₁)

-- | Removes the symbol at the given position from the given 'List' and
--   returns a tuple containing a 'List' with a "hole" at the given position
--   and the label that has been removed.
clipList :: Int -> [a] -> (a -> [a], a)
clipList 0 (a:as) = ((: as), a)
clipList i (a:as) = let (f, a') = clipList (i - 1) as
                    in  ((a :) . f, a')
clipList _ _      = error "the index exceeds the length of the list"
