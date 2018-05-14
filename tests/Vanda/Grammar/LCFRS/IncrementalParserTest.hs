-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Technische Universität Dresden 2018
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Grammar.LCFRS.IncrementalParserTest
    (tests) where

import Test.HUnit
import Vanda.Grammar.XRS.LCFRS.IncrementalParser

tests :: Test
tests = TestList    [ TestCase
                        $ assertEqual "ErrorMessage" "File Connected" parse
                      , TestCase $ assertEqual "Wrong Pretty Printed Grammar" "Test" exampleGrammar ]
