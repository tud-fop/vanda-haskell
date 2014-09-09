module HUnitTests where

import Control.Monad (void)
import Test.HUnit

import qualified Vanda.CBSM.MainTests
-- import qualified Vanda.CBSM.ProductOrderTests


main :: IO ()
main = void (runTestTT tests)


tests :: Test
tests = TestList
  [ "Vanda.CBSM.Main"         ~: Vanda.CBSM.MainTests.tests
--, "Vanda.CBSM.ProductOrder" ~: Vanda.CBSM.ProductOrderTests.tests
  ]
