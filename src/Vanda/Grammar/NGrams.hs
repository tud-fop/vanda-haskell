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
import Data.Maybe

data NGrams v w
  = NGrams
    { dict    :: (M.Map v Int, Int)
    , weights :: M.Map [Int] (w, Maybe w)
    }

hasWeight
  :: (Ord v)
  => NGrams v w
  -> [v]
  -> Bool
hasWeight (NGrams { dict = (d, _), weights = wt }) vs
  = let vi = L.map (d M.!) vs
    in  M.member vi wt


getWeight
  :: (Show v, Ord v)
  => NGrams v w
  -> [v]
  -> (w, Maybe w)
getWeight (NGrams { dict = (d, _), weights = wt }) vs
  = let vi = L.map (d M.!) vs
    in  fromJust . M.lookup vi $ wt

empty
  :: NGrams v w
empty
  = NGrams (M.empty, 0) M.empty

addWord
  :: (Show v, Ord v)
  => NGrams v w  -- original NGrams
  -> v           -- word
  -> NGrams v w  -- new NGrams
addWord n ve
  = let (m, c) = dict n
    in  if   M.notMember ve m
        then n { dict = (M.insert ve c m, c + 1) }
        else n

addNGram
  :: (Show v, Show w, Ord v)
  => NGrams v w -- original NGrams
  -> [v]        -- new NGram
  -> w          -- NGram-weight
  -> Maybe w          -- backoff-weight
  -> NGrams v w -- new NGrams
addNGram n@(NGrams { weights = wt }) vs w1 w2
  = let n' = L.foldl' addWord n vs
        vi = L.map (\ x -> fst (dict n') M.! x) vs
    in  n' { weights = M.insert vi (w1, w2) wt }

--  P_katz(w0...wn) = / P(w0...wn)                    , if C(w0...wn) > 0
--                    \ b(w0...wn-1) * P_katz(w1...wn), otherwise.
find            -- uses Katz Backoff
  :: (Show v, Ord v, Eq w, Num w)
  => NGrams v w -- NGrams on which to base the evaluation
  -> [v]        -- sequence to evaluate
  -> w          -- single NGram probability
find n vs
  = if   hasWeight n vs                             -- if C(w0...wn) > 0
    then fst . getWeight n $ vs
    else let vs1    = L.take (L.length vs - 1) vs   -- w0...wn-1
             vs2    = L.drop 1 vs                   -- w1...wn
             (_, b) = getWeight n vs1
         in  (fromJust b) + find n vs2

evaluate
  :: (Show v, Ord v, Eq w, Num w)
  => NGrams v w -- NGrams on which to base the evaluation
  -> Int        -- maximum length of NGram
  -> [v]        -- sequence to evaluate
  -> w          -- n-gram model's probability for the sequence
evaluate n i vs
  = if   i >= L.length vs
    then find n vs
    else find n (L.take i vs) + evaluate n i (L.drop 1 vs)
