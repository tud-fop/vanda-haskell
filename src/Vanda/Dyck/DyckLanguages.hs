#!/usr/bin/env runhaskell

{-|
Module:      Vanda.Dyck.DyckLanguages
Description: functions to work with /Dyck languages/ languages
Copyright:   Ⓒ Tobias Denkinger, 2015
Maintainer:  Tobias.Denkinger@tu-dresden.de
Stability:   experimental

This module contains functions to work with /Dyck languages/.
-}
module Vanda.Dyck.DyckLanguages
  ( isDyck
  , dyckPushdownAutomaton
-- * Dyck language generator
-- | The following is taken from <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.37.7262&rep=rep1&type=pdf Bruce McKenzie: The generation of strings from a CFG using a functional language, 1997> and instanciated for the task.
  , constructDyckGrammar
  , intersectDyckLangs
  ) where


import qualified Data.Bimap as BM
import qualified Data.List as L
import qualified Data.List.Utils as LU
import qualified Data.Map as M

import Vanda.Grammar.AutomataStorage


-- | Checks whether a string is well-bracketed.
_isDyck :: (Eq a, Ord a) => [a] -> [a] -> [a] -> Bool
_isDyck l r x =
  foldl (\ x' a -> x' >>= step a) (Just []) x == Just []
  where
    m = BM.fromList $ zip l r
    step a (y:ys)
      = if   elem a $ BM.keysR m
        then if   a == y
             then Just ys
             else Nothing
        else Just $ (m BM.! a):y:ys
    step a []
      = if   elem a $ BM.keys m
        then Just $ [m BM.! a]
        else Nothing


-- | Checks whether a string is well-bracketed using an 'Automaton' with
--   pushdown storage.
isDyck
  :: (Eq a, Ord a)
  => [a]                                         -- ^ list of left parentheses
  -> [a]                                        -- ^ list of right parentheses
  -> [a]                             -- ^ word whose membership is in question
  -> Bool
isDyck l r = let b = (M.!) (M.fromList $ zip l r)
             in  not . null . runAutomaton (dyckPushdownAutomaton l b)

-- | A pushdown automaton for the recognition of Dyck languages.
dyckPushdownAutomaton
  :: Eq a
  => [a]                                                 -- ^ left parentheses
  -> (a -> a)                                   -- ^ right parentheses mapping
  -> Automaton () a [a]
dyckPushdownAutomaton lps b = (((), []), τs, null . snd)
  where τs = [((), l, const True, (: []) . (b l :), ()) | l <- lps]
          ++ [((), b l, (not . null) &&& ((b l ==) . head), (: []) . drop 1, ()) | l <- lps]
        (f &&& g) x = f x && g x


-- | Compares the lengths of two 'L.List's.
cmpSymLen :: [a] -> [a] -> Ordering
cmpSymLen [] [] = EQ
cmpSymLen [] _ = LT
cmpSymLen _ [] = GT
cmpSymLen (_ : as) (_ : bs) = cmpSymLen as bs

-- | Models the /option/ in CFG productions.
(|||) :: [[a]] -> [[a]] -> [[a]]
(|||) = LU.mergeBy cmpSymLen

infixl 5 |||

-- | Models the /concatenation/ in CFG productions.
(+++) :: [[a]] -> [[a]] -> [[a]]
(+++) = cmpProd cmpSymLen

infixl 6 +++

type Corner a = ([a], Int, Int)

cmpVal :: ([a] -> [a] -> Ordering) -> Corner a -> Corner a -> Ordering
cmpVal comp (u, _, _) (v, _, _) = comp u v

cmpProd :: ([a] -> [a] -> Ordering) -> [[a]] -> [[a]] -> [[a]]
cmpProd comp l1 l2 = getFrontier initFrontier where
  initFrontier = [(initval, 0, 0)]
  initval = head l1 ++ head l2
  getFrontier [] = []
  getFrontier frontier = a : getFrontier frontier''' where
    ((a, r, c) : frontier') = frontier
    frontier'' = addrow frontier' (r+1) c
    frontier''' = addcol frontier'' r (c+1)
  addrow fs r c = if r `elem` rs then fs else insert r c fs where
    rs = map (\(_, r', _) -> r') fs
  addcol fs r c = if c `elem` cs then fs else insert r c fs where
    cs = map (\(_, _, c') -> c') fs
  insert r c frontier = case nth r l1 of
    Nothing -> frontier
    Just a -> case nth c l2 of
      Nothing -> frontier
      Just b -> L.insertBy (cmpVal comp) (a ++ b, r, c) frontier

-- | If the 'L.List' has an /n/-th element /e/, return 'Just' /e/ otherwise
--   return 'Nothing'.
nth :: Int -> [a] -> Maybe a
nth 0 (x : _) = Just x
nth n (_ : xs) = nth (n-1) xs
nth _ [] = Nothing

-- | Constructs a Dyck grammar for a given bijection.
constructDyckGrammar :: ([Char], [Char]) -> [String]
constructDyckGrammar (u, v)
  = foldl (|||) [""]
  $ [ [[x]] +++ constructDyckGrammar (u, v)
            +++ [[y]]
            +++ constructDyckGrammar (u, v)
    | (x, y) <- zip u v
    ]

-- | Intersects two Dyck languages (given by their characteristic bijections)
--   and outputs a sub-'L.List' of the resulting language.
intersectDyckLangs
  :: ([String] -> [String])          -- ^ function that selects a sub-'L.List'
  -> ([Char], [Char])     -- ^ bijection that determines the 1st Dyck language
  -> ([Char], [Char])     -- ^ bijection that determines the 2nd Dyck language
  -> [String]                            -- ^ sub-'L.List' of the intersection
intersectDyckLangs tk t1 (u, v)
  = filter (isDyck u v) . tk $ constructDyckGrammar t1
