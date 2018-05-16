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
import Vanda.Grammar.PMCFG 

first :: (x,y,z) -> x
first (x,_,_) = x

tests :: Test
tests = TestList    [ TestCase
                        $ assertEqual "ErrorMessage" "File Connected" testParse
--                      , TestCase $ not assertEqual "Wrong Pretty Printed Grammar" "Test" exampleGrammar 
                     ]

--                        TestCase $ assertEquals "Active Item not in Chart after ",
--                       TestCase $ assertEqual "Chart Update doesn't work" 
