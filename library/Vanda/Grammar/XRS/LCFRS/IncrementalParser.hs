{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Grammar.XRS.LCFRS.IncrementalParser
  ( parse,
    exampleGrammar
  ) where

import Vanda.Grammar.PMCFG (prettyPrintRule, exampleWPMCFG)
import Vanda.Grammar.PMCFG (WPMCFG (..), PMCFG (..), prettyPrintWPMCFG)

parse :: String
parse = "File Connected"

exampleGrammar :: String
exampleGrammar = prettyPrintWPMCFG prettyShowString prettyShowString exampleWPMCFG

-- From executable/PMCFG.hs
prettyShowString :: (Show w) => w -> String
prettyShowString s = '\"' : concatMap g (show s) ++ "\"" where
  g c    = [c]



