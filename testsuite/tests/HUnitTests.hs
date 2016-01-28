module HUnitTests where

import Control.Monad (when)
import System.Exit (exitFailure)
import Test.HUnit

import qualified Control.ErrorTests
import qualified Data.List.ExtraTests
import qualified Data.MultiMapTests
import qualified Data.RevMapTests
import qualified Vanda.Algorithms.EarleyTest
import qualified Vanda.CBSM.CountBasedStateMergingTests
import qualified Vanda.CBSM.DovetailingTests
import qualified Vanda.CBSM.ProductOrderTests


main :: IO ()
main = do
  cnts <- runTestTT tests
  when (errors cnts /= 0 || failures cnts /= 0) exitFailure


tests :: Test
tests = TestList
  [ "Control.Error"
    ~: Control.ErrorTests.tests
  , "Data.List.Extra"
    ~: Data.List.ExtraTests.tests
  , "Data.MultiMapTests"
    ~: Data.MultiMapTests.tests
  ,   "Data.RevMapTests"
    ~: Data.RevMapTests.tests
  ,   "Vanda.Algorithms.Earley"
    ~: Vanda.Algorithms.EarleyTest.tests
  ,   "Vanda.CBSM.CountBasedStateMerging"
    ~: Vanda.CBSM.CountBasedStateMergingTests.tests
  ,   "Vanda.CBSM.Dovetailing"
    ~: Vanda.CBSM.DovetailingTests.tests
  ,   "Vanda.CBSM.ProductOrder"
    ~: Vanda.CBSM.ProductOrderTests.tests
  ]
