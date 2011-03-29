-- Copyright (c) 2011, Toni Dietze

module Main where

import qualified Algorithms.NBest as NB
import qualified Data.WTA as WTA
import qualified Data.WSA as WSA
import Data.Hypergraph
import qualified Parser.Negra as Negra
import qualified RuleExtraction as RE
import qualified StateSplit as SPG
import qualified WTABarHillelComplete as BH
import Tools.Miscellaneous (mapFst)
import Data.List (nub)

import TestData.TestWTA

import Control.DeepSeq
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Tree as T
import Text.Parsec.String (parseFromFile)
import qualified Random as R
import System(getArgs)
import System.IO.Unsafe

main = do
  args <- getArgs
  case head args of
    "tdbh" ->  tdbh (tail args)
    "tdbhStats" ->  tdbhStats (tail args)
    "readWTA" -> readWTA (tail args)


tdbh args
  = tdbhHelper args
      (\ wsa wta -> rnf (BH.intersect wsa wta) `seq` return ())


tdbhStats args
  = tdbhHelper args
      ( \ wsa wta -> do
        let wta' = BH.intersect wsa wta
        let target' = (fst $ head $ WTA.finalWeights wta')
        let wta'' = WTA.fromHypergraph target'
                  $ dropUnreachables target'
                  $ WTA.toHypergraph
                  $ wta'
        putStr "yield-length:              "
        putStrLn $ show $ length $ (read (args !! 1) :: [String])
        putStr "tdbh-trans-states-finals:  "
        printWTAStatistic wta'
        putStr "tdbh-unreachables-dropped: "
        putStrLn "-1\t-1\t-1"  -- printWTAStatistic wta''
        putStr "item-count:                "
        putStrLn "-1"
          -- $ show
          -- $ length
          -- $ BH.getIntersectItems (const False) wsa wta
        putStr "complete-Bar-Hillel-trans: "
        putStrLn $ show $ BH.intersectTransitionCount wsa wta
      )


readWTA args
  = tdbhHelper args (\ wsa wta -> rnf wta `seq` return ())


tdbhHelper args f = do
  g <-  fmap (read :: String -> Hypergraph {-(String, Int)-}Int String Double ())
    $   readFile (args !! 0)
  let yld = read (args !! 1) :: [String]
  f (WSA.fromList 1 yld) (WTA.fromHypergraph {-("ROOT", 0)-}0 g)


printWTAStatistic wta = do
  putStr   $ show $ length $ WTA.transitions  wta
  putStr "\t"
  putStr   $ show $ length $ WTA.states       wta
  putStr "\t"
  putStrLn $ show $ length $ WTA.finalWeights wta
