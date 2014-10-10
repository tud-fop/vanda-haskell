module HUnitTests where

import Control.Monad (void)
import Test.HUnit

import qualified Control.ErrorTests
import qualified Data.MultiMapTests
import qualified Data.RevMapTests
import qualified Vanda.CBSM.CountBasedStateMergingTests
import qualified Vanda.CBSM.ProductOrderTests


main :: IO ()
main = void (runTestTT tests)


tests :: Test
tests = TestList
  [ "Control.Error"
    ~: Control.ErrorTests.tests
  , "Data.MultiMapTests"
    ~: Data.MultiMapTests.tests
  ,   "Data.RevMapTests"
    ~: Data.RevMapTests.tests
  ,   "Vanda.CBSM.CountBasedStateMerging"
    ~: Vanda.CBSM.CountBasedStateMergingTests.tests
  ,   "Vanda.CBSM.ProductOrder"
    ~: Vanda.CBSM.ProductOrderTests.tests
  ]
