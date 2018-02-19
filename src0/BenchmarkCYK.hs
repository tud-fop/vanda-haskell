-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Technische Universität Dresden 2018
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module BenchmarkCYK where

import qualified Algorithms.CYK
import qualified Algorithms.CYKExtended
import Data.WCFG

import Control.DeepSeq
import System.Environment (getArgs)


g :: WCFG Char Char Int Int
g = wcfg 'S'
  [ production 'S' [Left 'A', Left 'B'] 0 0
  , production 'A' [Left 'A', Left 'A'] 1 1
  , production 'A' [Right 'a'] 2 2
  , production 'B' [Left 'C', Left 'B'] 3 3
  , production 'B' [Right 'b'] 4 4
  , production 'C' [Right 'b'] 5 5
  ]


main :: IO ()
main = do
  alg <- fmap head $ getArgs
  let parser = case alg of
                "cyk" -> Algorithms.CYK.cyk
                "ex"  -> Algorithms.CYKExtended.cyk
                _     -> error "Unknown action."
--   putStr $ drawWCFG $ parser (replicate 40 'a' ++ replicate 40 'b') g
  rnf (parser (replicate 40 'a' ++ replicate 40 'b') g) `seq` return ()
