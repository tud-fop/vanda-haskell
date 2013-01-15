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
  ( empty
  , addNGram
  , lookup
  , evaluate
  ) where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe

data NGrams v w
  = NGrams
    { dict    :: (M.Map v Int, Int)
    , weights :: M.Map [Int] (w, Maybe w)
    }

empty :: NGrams v w
empty = NGrams (M.empty, 0) M.empty

addWord
  :: NGram v w  -- original NGrams
  -> v          -- word
  -> NGram v w  -- new NGrams
addWord n ve
  = let (m, c) = dict n
    in  if   M.notMember v m
        then n { dict = (M.insert v c m, c + 1) }
        else n

addNGram
  :: NGram v w  -- original NGrams
  -> [v]        -- new NGram
  -> w          -- NGram-weight
  -> w          -- backoff-weight
  -> Ngram v w  -- new NGrams
addNGram n@(NGram { dict = d, weights = wt }) vs w1 w2
  = let n' = foldl' addWord n vs
    in  n' { weights = M.insert vs (w1, w2) wt }

--  P_katz(w0...wn) = / P(w0...wn)                    , if C(w0...wn) > 0
--                    \ b(w0...wn-1) * P_katz(w1...wn), otherwise.
lookup          -- uses Katz Backoff
  :: NGram v w  -- NGrams on which to base the evaluation
  -> [v]        -- sequence to evaluate
  -> w          -- single NGram probability
lookup n vs
  = if   M.member vs . weights $ n                  -- if C(w0...wn) > 0
    then fst . M.findWithDefault (0, Nothing) vs n
    else let vs1    = L.take (L.length vs - 1) vs   -- w0...wn-1
             vs2    = L.drop 1 vs                   -- w1...wn
             (_, b) = M.findWithDefault (0, Nothing) vs1 . weights $ n
         in  if   b == Nothing
             then undefined
             else b * lookup n vs2

evaluate
  :: NGram v w  -- NGrams on which to base the evaluation
  -> Int        -- maximum length of NGram
  -> [v]        -- sequence to evaluate
  -> w          -- n-gram model's probability for the sequence
evaluate n i vs
  = if   i <= L.length vs
    then lookup n vs
    else lookup n (L.take i vs) * evaluate n i (L.drop 1 vs)
