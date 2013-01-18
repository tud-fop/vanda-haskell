-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Grammar.NGrams.Functions
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
-----------------------------------------------------------------------------


module Vanda.Grammar.NGrams.Functions where

import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import Vanda.Grammar.NGrams
import Vanda.Grammar.NGrams.Text

loadNGrams
  :: FilePath
  -> IO (NGrams T.Text)
loadNGrams
  = fmap parseNGrams . TIO.readFile

evaluateLine
  :: NGrams T.Text
  -> Int
  -> T.Text
  -> Double
evaluateLine g i l
  = evaluate g i
  . (\ x -> (L.replicate (i - 1) . T.pack $ "<s>") L.++ x L.++ [(T.pack "</s>")])
  . T.words
  $ l
