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
  deriving (Eq, Ord, Show)            

emptyNState :: NState i
emptyNState = Unary []

mkNState :: KenLM -> [Int] -> (NState Int, Double)
mkNState lm s
  = let n = order lm
    in  if   n < (length s)
        then ( Binary (take (n - 1) s) (last' (n - 1) s)
             , evaluateInt lm (nullContextState lm) s
             )
        else (Unary s, 0)

mergeNState
  :: KenLM
  -> (NState Int, Double)
  -> NState Int
  -> (NState Int, Double)
mergeNState lm (Unary s1, w1) (Unary s2)
  = (\ (x, w2) -> (x, w1 + w2))
  . mkNState lm
  $ (s1 ++ s2)
mergeNState lm (Unary s1, w1) (Binary s2 s3)
  = ( Binary (take ((order lm) - 1) (s1 ++ s2)) s3
    , w1 + (evaluateInt lm (nullContextState lm) (s1 ++ s2))
    )
mergeNState lm (Binary s1 s2, w1) (Unary s3)
  = ( Binary s1 (take ((order lm) - 1) (s2 ++ s3))
    , w1 + (evaluateInt lm (nullContextState lm) (s2 ++ s3))
    )
mergeNState lm (Binary s1 s2, w1) (Binary s3 s4)
  = ( Binary (take ((order lm) - 1) (s1 ++ s2))
             (last' ((order lm) - 1) (s3 ++ s4))
    , w1 + (evaluateInt lm (nullContextState lm) (s1 ++ s2))
         + (evaluateInt lm (nullContextState lm) (s3 ++ s4))
    )

mergeNStates
  :: KenLM
  -> [NState Int]
  -> (NState Int, Double)
mergeNStates lm (x:xs)
  = foldl (mergeNState lm) (x, 0) xs
mergeNStates _ []
  = (Unary [], 0)

last' :: Int -> [v] -> [v]
last' n xs = drop ((length xs) - n) xs 

-- -- | Collapses a list of 'NStateString's to a new 'NState'.
-- collapseStates
--   :: Int                  -- ^ degree of the language model
--   -> [NStateString Int]   -- ^ given 'NStateString'
--   -> NState Int           -- ^ new State
-- collapseStates n ss
--   = undefined {-let (l, ls) = break isVariable ss
--         (r, rs) = break isVariable . reverse $ ss
--         fl x (y, _) = take (n - 1) . flip (++) y
--                                    . concat
--                                    $ x
--         fr x (_, y) = reverse . take (n - 1)
--                               . reverse
--                               . (++) y
--                               . concat
--                               . reverse
--                               $ x
--                               in  (fl (ts l) . head $ (ss ls), fr (ts r) . head $ (ss rs))-}

-- | Given a 'List' of 'NStateString's, derives the intermediate weight.
-- getWeight
--   :: KenLM
--   -> [NStateString Int]
--   -> Double
-- getWeight lm l
--   = getWeight' lm [] 0 l
-- 
-- getWeight'
--   :: KenLM                -- ^ language model
--   -> [i]                  -- ^ accumulated string
--   -> Double               -- ^ accumulated weight
--   -> [NStateString Int]   -- ^ remaining input
--   -> Double               -- ^ resulting weight
-- getWeight' lm x mu []
--   = mu + evaluateInt lm (nullContextState lm) x
-- getWeight' _ _ mu ((NState _):[])
--   = mu
-- getWeight' lm [] mu ((NState (_, x2)):xs)
--   = getWeight' lm x2 mu xs
-- getWeight' lm x mu ((NState (x1, x2)):xs)
--   = getWeight' lm x2 mu' xs where
--     mu' = (evaluateInt lm (nullContextState lm) (x ++ x1)) + mu
-- getWeight' lm x mu (x1:xs)
--   = getWeight' lm (x ++ x1) mu xs
-- 
-- isVariable
--   :: NStateString i
--   -> Bool
-- isVariable NState _ = True
-- isVariable _ = False

{-- snippet initState --}
-- initState
--   :: Int                  -- ^ degree of the n-gram model
--   -> [NTT]                -- ^ input string
--   -> NState Int           -- ^ resulting 'NState'
-- initState n ntts
--   = let f (NT x) = x
--         f (T x)  = x
--         w = map f ntts
--     in  if length w < n
--         then (w,w)
--         else ( take (n - 1) w
--              , drop 
--                (length w - n + 1)
--                w
--              )
{-- /snippet initState --}
