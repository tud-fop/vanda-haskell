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
-- Top level functionality for NGrams.
--
-----------------------------------------------------------------------------


module Vanda.Grammar.NGrams.Functions where

import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import Vanda.Grammar.NGrams.VandaNGrams
import Vanda.Grammar.NGrams.Text

-- | Loads an NGram language model from a file.
loadNGrams
  :: FilePath                -- ^ file to load the model from
  -> IO (NGrams T.Text)      -- ^ NGrams model
loadNGrams
  = fmap parseNGrams
  . TIO.readFile

-- | Evaluates a sentence given a NGrams model.
evaluateLine
  :: NGrams T.Text           -- ^ NGrams model
  -> T.Text                  -- ^ sentence to score
  -> Double                  -- ^ score
evaluateLine lm l
  = evaluate lm
  . (\ x -> ( L.replicate ((order lm) - 1)
            . T.pack
            $ "<s>"
            )
       L.++ x
       L.++ [(T.pack "</s>")]
    )
  . T.words
  $ l
