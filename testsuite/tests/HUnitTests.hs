module HUnitTests where

import Control.Monad (when)
import System.Exit (exitFailure)
import Test.HUnit

import qualified Control.ErrorTests
import qualified Data.List.ExtraTests
import qualified Data.List.ShuffleTests
import qualified Data.MultiMapTests
import qualified Data.RevMapTests
import qualified Vanda.Algorithms.EarleyTest
import qualified Vanda.CBSM.CountBasedStateMergingTests
import qualified Vanda.CBSM.DovetailingTests
import qualified Vanda.CBSM.ProductOrderTests
import qualified Vanda.Corpus.BinarizationTests
import qualified Vanda.Grammar.PMCFG.DeductiveSolverTest
import qualified Vanda.Grammar.PMCFG.CYKParserTest
import qualified Vanda.Grammar.PMCFG.NaiveParserTest


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
  ,   "Data.List.Shuffle"
    ~: Data.List.ShuffleTests.tests
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
  ,   "Vanda.Corpus.Binarization"
    ~: Vanda.Corpus.BinarizationTests.tests
  ,   "Vanda.Grammar.PMCFG.DeductiveSolver"
    ~: Vanda.Grammar.PMCFG.DeductiveSolverTest.tests
  ,   "Vanda.Grammar.PMCFG.CYKParser"
    ~: Vanda.Grammar.PMCFG.CYKParserTest.tests
  ,   "Vanda.Grammar.PMCFG.NaiveParser"
    ~: Vanda.Grammar.PMCFG.NaiveParserTest.tests
  ]
