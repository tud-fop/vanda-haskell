-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Grammar.NGrams.WTA
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
-- Queries an Automaton that represents an n-gram model.
--
-----------------------------------------------------------------------------


module Vanda.Grammar.NGrams.WTA 
  ( emptyNState
  , mkNState
  , mergeNState
  , mergeNStates
  , NState
  ) where

import Vanda.Grammar.NGrams.KenLM

data NState v
  = Unary  [v]
  | Binary [v] [v]
  deriving (Eq, Ord)

instance Show v => Show (NState v) where
  show (Unary x)
    = show x
  show (Binary x y)
    = (show x) ++ "*" ++ (show y) 

emptyNState :: NState i
emptyNState = Unary []

mkNState :: KenLM -> [Int] -> (NState Int, Double)
mkNState lm s
  = let n = order lm
    in  if   n <= (length s)
        then ( Binary (take (n - 1) s) (last' (n - 1) s)
             , evaluateInt lm (nullContextState lm) s
             )
        else (Unary s, 1)

mergeNState
  :: KenLM
  -> (NState Int, Double)
  -> NState Int
  -> (NState Int, Double)
mergeNState lm (Unary s1, w1) (Unary s2)
  = (\ (x, w2) -> (x, w1 * w2))
  . mkNState lm
  $ (s1 ++ s2)
mergeNState lm (Unary s1, w1) (Binary s2 s3)
  = ( Binary (take ((order lm) - 1) (s1 ++ s2)) s3
    , w1 * (evaluateInt lm (nullContextState lm) (s1 ++ s2))
    )
mergeNState lm (Binary s1 s2, w1) (Unary s3)
  = ( Binary s1 (last' ((order lm) - 1) (s2 ++ s3))
    , w1 * (evaluateInt lm (nullContextState lm) (s2 ++ s3))
    )
mergeNState lm (Binary s1 s2, w1) (Binary s3 s4)
  = ( Binary (take ((order lm) - 1) (s1 ++ s2))
             (last' ((order lm) - 1) (s3 ++ s4))
    , w1 * (evaluateInt lm (nullContextState lm) (s1 ++ s2))
         * (evaluateInt lm (nullContextState lm) (s3 ++ s4))
    )

mergeNStates
  :: KenLM
  -> [NState Int]
  -> (NState Int, Double)
mergeNStates lm (x:xs)
  = foldl (mergeNState lm) (x, 1) xs
mergeNStates _ []
  = (Unary [], 1)

last' :: Int -> [v] -> [v]
last' n xs = drop ((length xs) - n) xs