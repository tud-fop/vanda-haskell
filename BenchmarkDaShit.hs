-- Copyright (c) 2011, Toni Dietze!!!

module Main where

import qualified Algorithms.NBest as NB
import qualified Data.WTAid as WTA
import qualified Data.WSA as WSA
import Data.Hypergraph
import qualified Parser.Negra as Negra
import qualified WTABarHillelTopDownBin as BHB
import Tools.Miscellaneous (mapFst)
import Data.List (nub)

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
    "print" -> printFileHG (tail args)
    "printYields" -> printYields (tail args)
    "tdbh" ->  tdbh (tail args)
    "tdbhStats" ->  tdbhStats (tail args)
    "printWTA" -> printWTA (tail args)
    "readWTA" -> readWTA (tail args)
    "manySentencesZigZag" -> manySentencesZigZag (tail args)
    "evenSentencelength" -> evenSentencelength (tail args)


printFileHG [hgFile]
  = readFile hgFile
  >>= putStrLn
    . drawHypergraph
    . (read :: String -> Hypergraph {-(String, Int)-}Int String Double ())

getData
  = parseFromFile
      Negra.p_negra
      "Parser/tiger_release_aug07_notable_2000_utf-8.export"


printYields _ = do
  Right dta <- getData
  putStr $ unlines $ map (show . reverse . yield . onlyPreterminals) $ negrasToTrees dta

tdbh args
  = tdbhHelper args
      (\ wsa wta -> rnf (BHB.intersect wsa wta) `seq` return ())


tdbhStats args
  = tdbhHelper args
      ( \ wsa wta -> do
        let wta' = BHB.intersect wsa wta
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
        printWTAStatistic wta''  -- putStrLn "-1\t-1\t-1"
        putStr "item-count:                "
        putStrLn "-1"
          -- $ show
          -- $ length
          -- $ BH.getIntersectItems (const False) wsa wta
        putStr "complete-Bar-Hillel-trans: "
        putStrLn "-1"  -- $ show $ BHC.intersectTransitionCount wsa wta
      )


printWTA args
  = tdbhHelper args (const WTA.printWTA)


readWTA args
  = tdbhHelper args (\ wsa wta -> rnf wta `seq` return ())


tdbhHelper args f = do
  g <-  fmap (read :: String -> Hypergraph {-(String, Int)-}Int String Double ())
    $   readFile (args !! 0)
  let yld = read (args !! 1) :: [String]
  f (WSA.fromList 1 yld) (WTA.addId $ WTA.fromHypergraph {-("ROOT", 0)-}0 g)


manySentencesZigZag args = do
  g <-  fmap (read :: String -> Hypergraph {-(String, Int)-}Int String Double ())
    $   readFile (args !! 0)
  let ylds = read (args !! 1) :: [[String]]
  let wta = WTA.addId $ WTA.fromHypergraph {-("ROOT", 0)-}0 g
  let wsa = combineWSAs $ map (WSA.fromList 1) ylds
  putStrLn $ unlines $ map show $ WSA.transitions wsa
  putStrLn $ unlines $ map show $ WSA.initialWeights wsa
  putStrLn $ unlines $ map show $ WSA.finalWeights wsa
  rnf (BHB.intersect wsa wta) `seq` return ()
  where
    combineWSAs xs
      = WSA.create
          (concatMap WSA.transitions xs)
          (concatMap WSA.initialWeights xs)
          (concatMap WSA.finalWeights xs)


evenSentencelength args = do
  g <-  fmap (read :: String -> Hypergraph {-(String, Int)-}Int String Double ())
    $   readFile (args !! 0)
  let ylds = read (args !! 1) :: [String]
  let redundancy = read (args !! 2) :: Int
  let wta = WTA.addId $ WTA.fromHypergraph {-("ROOT", 0)-}0 g
  let wsa = WSA.create
              ( flip concatMap (nub ylds) $ \ x ->
                  flip concatMap [1 .. redundancy] $ \ n ->
                    [WSA.Transition x 0 n 1, WSA.Transition x n 0 1]
              )
              [(0 :: Int, 1)]
              [(0 :: Int, 1)]
--   let wsa = WSA.fromListCyclic 1 ylds
--   putStrLn $ unlines $ map show $ WSA.transitions wsa
--   putStrLn $ unlines $ map show $ WSA.initialWeights wsa
--   putStrLn $ unlines $ map show $ WSA.finalWeights wsa
--   WTA.printWTA $ BHB.intersect wsa wta
--   print $ length $ WTA.transitions $ BHB.intersect wsa wta
  rnf (BHB.intersect wsa wta) `seq` return ()


negrasToTrees
  = concatMap
      ( fmap Negra.negraTreeToTree
      . Negra.negraToForest
      . Negra.filterPunctuation
      . Negra.sData
      )


onlyPreterminals (T.Node x [T.Node _ []]) = T.Node x []
onlyPreterminals (T.Node x ts) = T.Node x (map onlyPreterminals ts)


yield (T.Node r []) = [r]
yield (T.Node _ ts) = concatMap yield ts


traceFile file x y
  = unsafePerformIO (writeFile file (show x) >> return y)


hgToNBestHg g
  = ( vertices g
    , \ v -> map (\ e -> (eId e, eTail e)) $ M.findWithDefault [] v eM
    , \ i _ -> M.findWithDefault 0 i iM
    )
  where
    eM = edgesM g
    iM = M.fromList $ map (\ e -> (eId e, eWeight e)) $ edges g


hPathToTree (NB.B i bs)
  = T.Node i (map hPathToTree bs)


idTreeToLabelTree g
  = fmap (\ i -> M.findWithDefault (error "unknown eId") i iM)
  where
    iM = M.fromList $ map (\ e -> (eId e, eLabel e)) $ edges g


pairToTuple (NB.P x y) = (x, y)


printWTAStatistic wta = do
  putStr   $ show $ length $ WTA.transitions  wta
  putStr "\t"
  putStr   $ show $ length $ WTA.states       wta
  putStr "\t"
  putStrLn $ show $ length $ WTA.finalWeights wta
