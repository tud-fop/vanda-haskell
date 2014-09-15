module HUnitTests where

import Control.Monad (void)
import Test.HUnit

import qualified Data.MultiMapTests
import qualified Data.RevMapTests
import qualified Vanda.CBSM.MainTests
import qualified Vanda.CBSM.ProductOrderTests


main :: IO ()
main = void (runTestTT tests)


tests :: Test
tests = TestList
  [ "Data.MultiMapTests"       ~: Data.MultiMapTests.tests
  , "Data.RevMapTests"         ~: Data.RevMapTests.tests
  , "Vanda.CBSM.Main"          ~: Vanda.CBSM.MainTests.tests
  , "Vanda.CBSM.ProductOrder"  ~: Vanda.CBSM.ProductOrderTests.tests
  ]
