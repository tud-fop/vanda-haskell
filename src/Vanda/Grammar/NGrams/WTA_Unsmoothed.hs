-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Grammar.NGrams.WTA_Unsmoothed
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
-- Queries an Automaton that represents an n-gram model. Inspired by the
-- method of incorporating an n-gram model into a translation model as
-- described in [Chiang 2007, Hierarchical Phrase-Based Translation,
-- Section 5.3].
--
-----------------------------------------------------------------------------


module Vanda.Grammar.NGrams.WTA_Unsmoothed
  ( State ()
  , delta
  , mapState
  ) where

import Vanda.Grammar.LM
import Vanda.Grammar.NGrams.WTA

delta :: LM a => a -> [State Int] -> [Int] -> [(State Int, Double)]
delta = delta' deltaW

-- | helper for transition weights (calculates intermediate
--   values using backoff and cancels them out later)
deltaW :: LM a => a -> [State Int] -> [Int] -> Double
deltaW lm [] yield
  = score lm yield
deltaW lm xs _
  = sum . map (score lm) . filter (\x -> length x >= order lm) . extractSubstrings $ xs