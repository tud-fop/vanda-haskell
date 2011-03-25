-- Copyright (c) 2011, Toni Dietze

module Main where

import qualified Data.WTA as WTA
import qualified Data.WSA as WSA
import Data.Hypergraph
import qualified Parser.Negra as Negra
import qualified RuleExtraction as RE
import qualified StateSplit as SPG
import qualified WTABarHillelTopDown as BH
import Data.List (nub)

import TestData.TestWTA

import qualified Data.Map as M
import qualified Data.Tree as T
import Text.Parsec.String (parseFromFile)
import qualified Random as R
import System(getArgs)
import System.IO.Unsafe

main = do
  args <- getArgs
  case head args of
    "print" -> printFileHG (tail args)
    "train" -> train (tail args)
    "test" -> test (tail args)


printFileHG [hgFile]
  = readFile hgFile
  >>= putStrLn . drawHypergraph . (read :: String -> Hypergraph (String, Int) String Double [Int])

getData
  = parseFromFile
      Negra.p_negra
      "Parser/tiger_release_aug07_notable_2000_utf-8.export"


train args = do
  let its = read (args !! 0)
  let exs = read (args !! 1)
  Right dta <- getData
  let ts = take exs $ negrasToTrees dta
  let trains = map onlyPreterminals ts
  let exPretermToTerm = mapIds (const []) $ SPG.initialize $ RE.extractHypergraph $ concatMap terminalBranches ts
  let gsgens
        = take (its + 1)
        $ SPG.train'
            trains
            "ROOT"
            (RE.extractHypergraph trains :: Hypergraph String String Double ())
            (R.mkStdGen 0)
  flip mapM_ (zip [0 ..] gsgens) $ \ (n, (g, _)) -> do
    writeFile ("hg_" ++ show exs ++ "_" ++ show n ++ ".txt") (show g)
    writeFile ("hg_" ++ show exs ++ "_" ++ show n ++ "_withTerminals.txt")
      $ show
      $ hypergraph
          (  filter (null . eTail) (edges exPretermToTerm)
          ++ concatMap (extendEdge (edgesM exPretermToTerm)) (edges g)
          )
  where
    onlyPreterminals (T.Node x [T.Node _ []]) = T.Node x []
    onlyPreterminals (T.Node x ts) = T.Node x (map onlyPreterminals ts)
    terminalBranches t@(T.Node _ [T.Node _ []]) = [t]
    terminalBranches (T.Node _ ts) = concatMap terminalBranches ts
    extendEdge eM e
      = if null (eTail e)
        then
          case M.lookup (fst $ eHead e, 0) eM of
            Nothing -> [e]
            Just es -> map (eMapWeight (eWeight e *) . eMapHead (const $ eHead e)) es
        else [e]


test args = do
  let hgFile = args !! 0
  g <- fmap read $ readFile hgFile :: IO (Hypergraph (String, Int) String Double [Int])
  putStrLn $ drawHypergraph g
  Right dta <- getData
  let ts = {-filter ((< 15) . length . yield) $-} {-drop 200 $-} negrasToTrees dta
  let wta = WTA.fromHypergraph ("ROOT", 0) g
  flip mapM_ ts $ \ t -> do
    let wta'  = WTA.fromHypergraph (0, ("ROOT", 0), length $ yield t)
              $ dropUnreachables (0, ("ROOT", 0), length $ yield t)
              $ WTA.toHypergraph
              $ BH.intersect (WSA.fromList 1 $ yield t) wta
    let ts' = take 3
            $ filter ((t ==) . fst)
            $ filter ((0 /= ) . snd)
            $ map ( \ t' -> (t', WTA.weightTree wta' t'))
            $ nub
            $ filter (\ (T.Node r _) -> r == "ROOT")
            $ take 10000
            $ WTA.generate
            $ wta'
    -- putStrLn $ T.drawTree t
    print $ yield t
    -- putStrLn $ WTA.showWTA $ wta'
    flip mapM_ ts' $ \ (t', w) -> do
      putStr "Weight: "
      print $ w
      putStrLn $ T.drawTree t'
    putStrLn (replicate 80 '=')


negrasToTrees
  = concatMap
      ( fmap Negra.negraTreeToTree
      . Negra.negraToForest
      . Negra.filterPunctuation
      . Negra.sData
      )


yield (T.Node r []) = [r]
yield (T.Node _ ts) = concatMap yield ts


traceFile file x y
  = unsafePerformIO (writeFile file (show x) >> return y)
