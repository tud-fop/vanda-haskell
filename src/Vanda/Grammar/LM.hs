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
-- Class of n-gram language models.
--
-----------------------------------------------------------------------------

module Vanda.Grammar.LM where

import qualified Data.Text.Lazy as T

import Vanda.Grammar.NGrams.VandaNGrams as VN
--import Vanda.Grammar.NGrams.KenLM as KN

class LM a where
  indexOf :: a -> T.Text -> Int
  order   :: a -> Int
  score   :: a -> [Int] -> Double

instance LM (VN.NGrams T.Text) where
  indexOf = VN.indexOf
  order   = VN.order
  score   = VN.evaluateInt

--instance LM KenLM where
--  indexOf = KN.dictIndex
--  order   = KN.order
--  score l = KN.evaluateInt l (KN.nullContextState l)