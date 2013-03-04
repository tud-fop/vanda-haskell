-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Grammar.NGrams.VandaNGrams
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
-- Implements a representation for NGrams language model.
--
-----------------------------------------------------------------------------

module Vanda.Grammar.NGrams.VandaNGrams
  ( NGrams
  , empty
  , order
  , indexOf
  , addNGram
  , evaluate
  , evaluateInt
  ) where

import qualified Data.Map as M
import qualified Data.List as L

{-- snippet NGrams --}
data NGrams v
  = NGrams
    { dict    :: M.Map v Int
    , dLength :: Int
    , order   :: Int
    , weights :: M.Map [Int] (Double, Maybe Double)
    }
{-- /snippet NGrams --}

hasWeight
  :: (Ord v, Show v)
  => NGrams v
  -> [v]
  -> Bool
hasWeight lm vs
  = flip M.member (weights lm)
  . L.map (indexOf lm)
  $ vs

hasWeightInt
  :: NGrams v
  -> [Int]
  -> Bool
hasWeightInt _ []
  = True
hasWeightInt lm is
  = M.member is . weights $ lm

getWeight
  :: (Ord v)
  => NGrams v
  -> [v]
  -> (Double, Double)
getWeight lm vs
  = getWeightInt lm
  . L.map (indexOf lm)
  $ vs

getWeightInt
  :: NGrams v
  -> [Int]
  -> (Double, Double)
getWeightInt _ []
  = (0, 0)
getWeightInt lm is
  = case (M.lookup is . weights $ lm) of
          Nothing            -> (0, 0)
          Just (a, Nothing)  -> (a, 0)
          Just (a, Just b)   -> (a, b)

-- | Returns an empty NGrams language model.
empty
  :: Int                   -- ^ order
  -> NGrams v              -- ^ empty NGrams model
empty n
  = NGrams M.empty 0 n M.empty

indexOf
  :: Ord v
  => NGrams v
  -> v
  -> Int
indexOf lm x
  = M.findWithDefault (-1) x . dict $ lm

addWord
  :: (Show v, Ord v)
  => NGrams v              -- ^ original NGrams
  -> v                     -- ^ word
  -> NGrams v              -- ^ new NGrams
addWord lm ve
  = let (m, c) = (dict lm, dLength lm)
    in  if   M.notMember ve m
        then lm{ dict = M.insert ve c m, dLength = c + 1 }
        else lm

-- | Adds an n-gram to the model.
addNGram
  :: (Show v, Ord v)
  => NGrams v              -- ^ original NGrams
  -> [v]                   -- ^ new NGram
  -> Double                -- ^ NGram-weight
  -> Maybe Double          -- ^ backoff-weight
  -> NGrams v              -- ^ new NGrams
addNGram n@(NGrams { weights = wt }) vs w1 w2
  = let n' = L.foldl' addWord n vs
        vi = L.map (\ x -> (dict n') M.! x) vs
    in  n' { weights = M.insert vi (w1, w2) wt }

-- | Determines the weight of a single n-gram using Katz Backoff.
--   P_katz(w0...wn) = / P(w0...wn)                    , if C(w0...wn) > 0
--                     \ b(w0...wn-1) * P_katz(w1...wn), otherwise.
{-- snippet KatzBackoff --}
find
  :: (Show v, Ord v)
  => NGrams v              -- ^ NGrams on which to base the evaluation
  -> [v]                   -- ^ sequence to evaluate
  -> Double                -- ^ single NGram probability
find n vs
  = if   hasWeight n vs                             -- if C(w0...wn) > 0
    then fst . getWeight n $ vs
    else let vs1    = L.take (L.length vs - 1) vs   -- w0...wn-1
             vs2    = L.drop 1 vs                   -- w1...wn
             (_, b) = getWeight n vs1
         in  b + find n vs2
{-- /snippet KatzBackoff --}

findInt
  :: NGrams v              -- ^ NGrams on which to base the evaluation
  -> [Int]                 -- ^ sequence to evaluate
  -> Double                -- ^ single NGram probability
findInt n is
  = if   hasWeightInt n is                          -- if C(w0...wn) > 0
    then fst . getWeightInt n $ is
    else let is1    = L.take (L.length is - 1) is   -- w0...wn-1
             is2    = L.drop 1 is                   -- w1...wn
             (_, b) = getWeightInt n is1
         in  b + findInt n is2

-- | Scores a sentence.
evaluate
  :: (Show v, Ord v)
  => NGrams v              -- ^ NGrams on which to base the evaluation
  -> [v]                   -- ^ sentence to evaluate
  -> Double                -- ^ score
evaluate lm vs
  = if   (order lm) >= L.length vs
    then find lm vs
    else find lm (L.take (order lm) vs) + evaluate lm (L.drop 1 vs)

evaluateInt
  :: (Show v, Ord v)
  => NGrams v
  -> [Int]
  -> Double
evaluateInt lm is
  = if   (order lm) >= L.length is
    then findInt lm is
    else findInt lm (L.take (order lm) is) + evaluateInt lm (L.drop 1 is)