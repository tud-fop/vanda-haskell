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
  ( collapseStates
  , initState 
  , NState
  , NString
  , NStateString
  ) where

import qualified Data.List as L
import Vanda.Grammar.NGrams.KenLM
import Data.NTT

type NState i = ([i], [i])
type NString i = [i]
data NStateString i = NString i | NState i

-- | Collapses a list of 'NStateString's to a new 'NState'.
collapseStates
  :: Int                  -- ^ degree of the language model
  -> [NStateString Int]   -- ^ given 'NStateString'
  -> NState Int           -- ^ new State
collapseStates n ss
  = undefined {-let (l, ls) = break isVariable ss
        (r, rs) = break isVariable . reverse $ ss
        fl x (y, _) = take (n - 1) . flip (++) y
                                   . concat
                                   $ x
        fr x (_, y) = reverse . take (n - 1)
                              . reverse
                              . (++) y
                              . concat
                              . reverse
                              $ x
                              in  (fl (ts l) . head $ (ss ls), fr (ts r) . head $ (ss rs))-}

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
initState
  :: Int                  -- ^ degree of the n-gram model
  -> [NTT]                -- ^ input string
  -> NState Int           -- ^ resulting 'NState'
initState n ntts
  = let f (NT x) = x
        f (T x)  = x
        w = map f ntts
    in  if length w < n
        then (w,w)
        else ( take (n - 1) w
             , drop 
               (length w - n + 1)
               w
             )
{-- /snippet initState --}
