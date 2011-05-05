-- (c) 2011 Matthias Büchse <Matthias.Buechse@tu-dresden.de>
-- (c) 2011 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

module Main where

import qualified Data.WTAid as WTA
import qualified Data.WSA as WSA
import Data.Hypergraph
import qualified Parser.Negra as Negra
import qualified Algorithms.WTABarHillelTopDownBinarizing as BHB
import Data.List (nub)

import Control.DeepSeq
import qualified Data.Tree as T
import Text.Parsec.String (parseFromFile)
import System(getArgs)
import Text.Parsec (ParseError ())


main :: IO ()
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
    _ -> putStrLn "Unknown action."


printFileHG :: [String] -> IO ()
printFileHG args
  = readFile (args !! 0)
  >>= putStrLn
    . drawHypergraph
    . (read :: String -> Hypergraph {-(String, Int)-}Int String Double ())


getData :: IO (Either ParseError [Negra.Sentence])
getData
  = parseFromFile
      Negra.p_negra
      "Parser/tiger_release_aug07_notable_2000_utf-8.export"


printYields :: a -> IO ()
printYields _ = do
  Right dta <- getData
  putStr $ unlines $ map (show . reverse . yield . onlyPreterminals) $ negrasToTrees dta


tdbh :: [String] -> IO ()
tdbh args
  = tdbhHelper args
      (\ wsa wta -> rnf (BHB.intersect wsa wta) `seq` return ())


tdbhStats :: [String] -> IO ()
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


printWTA :: [String] -> IO ()
printWTA args
  = tdbhHelper args (const WTA.printWTA)


readWTA :: [String] -> IO ()
readWTA args
  = tdbhHelper args (\ _ wta -> rnf wta `seq` return ())


tdbhHelper
  :: (Num w)
  => [String]
  -> (WSA.WSA Int String w -> WTA.WTA Int String Double Int -> IO a)
  -> IO a
tdbhHelper args f = do
  g <-  fmap (read :: String -> Hypergraph {-(String, Int)-}Int String Double ())
    $   readFile (args !! 0)
  let yld = read (args !! 1) :: [String]
  f (WSA.fromList 1 yld) (WTA.addId $ WTA.fromHypergraph {-("ROOT", 0)-}0 g)


manySentencesZigZag :: [String] -> IO ()
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


evenSentencelength :: [String] -> IO ()
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


negrasToTrees :: [Negra.Sentence] -> [T.Tree String]
negrasToTrees
  = concatMap
      ( fmap Negra.negraTreeToTree
      . Negra.negraToForest
      . Negra.filterPunctuation
      . Negra.sData
      )


onlyPreterminals :: T.Tree a -> T.Tree a
onlyPreterminals (T.Node x [T.Node _ []]) = T.Node x []
onlyPreterminals (T.Node x ts) = T.Node x (map onlyPreterminals ts)


yield :: T.Tree t -> [t]
yield (T.Node r []) = [r]
yield (T.Node _ ts) = concatMap yield ts


printWTAStatistic :: WTA.WTA q t w i -> IO ()
printWTAStatistic wta = do
  putStr   $ show $ length $ WTA.transitions  wta
  putStr "\t"
  putStr   $ show $ length $ WTA.states       wta
  putStr "\t"
  putStrLn $ show $ length $ WTA.finalWeights wta
