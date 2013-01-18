-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Grammar.NGrams
-- Copyright   :  (c) Technische Universität Dresden 2013
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Tobias.Denkinger@mailbox.tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- This module implements a representation for n-grams.
--
-----------------------------------------------------------------------------

module Vanda.Grammar.NGrams
  ( NGrams
  , empty
  , addNGram
  , find
  , evaluate
  ) where

import qualified Data.Map as M
import qualified Data.List as L

data NGrams v
  = NGrams
    { dict    :: (M.Map v Int, Int)
    , weights :: M.Map [Int] (Double, Maybe Double)
    }

hasWeight
  :: (Ord v, Show v)
  => NGrams v
  -> [v]
  -> Bool
hasWeight _ []
  = True
hasWeight (NGrams { dict = (d, _), weights = wt }) vs
  = let vi = L.map (\x -> M.findWithDefault (-1) x d) vs
    in  M.member vi wt


getWeight
  :: (Show v, Ord v)
  => NGrams v
  -> [v]
  -> (Double, Double)
getWeight _ []
  = (0, 0)
getWeight (NGrams { dict = (d, _), weights = wt }) vs
  = let vi = L.map (\x -> M.findWithDefault (-1) x d) vs
    in  case (M.lookup vi $ wt) of
          Nothing            -> (0, 0)
          Just (a, Nothing)  -> (a, 0)
          Just (a, Just b)   -> (a, b)

empty
  :: NGrams v
empty
  = NGrams (M.empty, 0) M.empty

addWord
  :: (Show v, Ord v)
  => NGrams v    -- original NGrams
  -> v           -- word
  -> NGrams v    -- new NGrams
addWord n ve
  = let (m, c) = dict n
    in  if   M.notMember ve m
        then n { dict = (M.insert ve c m, c + 1) }
        else n

addNGram
  :: (Show v, Ord v)
  => NGrams v              -- original NGrams
  -> [v]                   -- new NGram
  -> Double                -- NGram-weight
  -> Maybe Double          -- backoff-weight
  -> NGrams v              -- new NGrams
addNGram n@(NGrams { weights = wt }) vs w1 w2
  = let n' = L.foldl' addWord n vs
        vi = L.map (\ x -> fst (dict n') M.! x) vs
    in  n' { weights = M.insert vi (w1, w2) wt }

--  P_katz(w0...wn) = / P(w0...wn)                    , if C(w0...wn) > 0
--                    \ b(w0...wn-1) * P_katz(w1...wn), otherwise.
find            -- uses Katz Backoff
  :: (Show v, Ord v)
  => NGrams v   -- NGrams on which to base the evaluation
  -> [v]        -- sequence to evaluate
  -> Double     -- single NGram probability
find n vs
  = if   hasWeight n vs                             -- if C(w0...wn) > 0
    then fst . getWeight n $ vs
    else let vs1    = L.take (L.length vs - 1) vs   -- w0...wn-1
             vs2    = L.drop 1 vs                   -- w1...wn
             (_, b) = getWeight n vs1
         in  b + find n vs2

evaluate
  :: (Show v, Ord v)
  => NGrams v   -- NGrams on which to base the evaluation
  -> Int        -- maximum length of NGram
  -> [v]        -- sequence to evaluate
  -> Double     -- n-gram model's probability for the sequence
evaluate n i vs
  = if   i >= L.length vs
    then find n vs
    else find n (L.take i vs) + evaluate n i (L.drop 1 vs)
