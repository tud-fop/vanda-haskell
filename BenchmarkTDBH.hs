-- Copyright (c) 2011, Toni Dietze

module Main where

import qualified Algorithms.NBest as NB
import qualified Data.WTA as WTA
import qualified Data.WSA as WSA
import Data.Hypergraph
import qualified Parser.Negra as Negra
import qualified RuleExtraction as RE
import qualified StateSplit as SPG
import qualified WTABarHillelTopDown as BH
import qualified WTABarHillelComplete as BHC
import Tools.Miscellaneous (mapFst, mapSnd)
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
import Text.Parsec (ParseError ())


main :: IO ()
main = do
  args <- getArgs
  case head args of
    "print" -> printFileHG (tail args)
    "printYields" -> printYields (tail args)
    "train" -> train (tail args)
    "test" -> test (tail args)
    "convert" -> convert (tail args)
    "convert2" -> convert2 (tail args)
    "binarize" -> binarize (tail args)
    "tdbh" ->  tdbh (tail args)
    "tdbhStats" ->  tdbhStats (tail args)
    "printWTA" -> printWTA (tail args)
    "readWTA" -> readWTA (tail args)
    "example" -> example (tail args)
    "manySentences" -> manySentences (tail args)
    "manySentencesZigZag" -> manySentencesZigZag (tail args)
    "evenSentencelength" -> evenSentencelength (tail args)


printFileHG :: [String] -> IO ()
printFileHG [hgFile]
  = readFile hgFile
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


train :: [String] -> IO ()
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
    writeFile ("hg_" ++ show exs ++ "_" ++ show n ++ ".txt") (show $ mapIds (const ()) g)
    writeFile ("hg_" ++ show exs ++ "_" ++ show n ++ "_withTerminals.txt")
      $ show
      $ mapIds (const ())
      $ hypergraph
          (  filter (null . eTail) (edges exPretermToTerm)
          ++ concatMap (extendEdge (edgesM exPretermToTerm)) (edges g)
          )
  where
    terminalBranches t@(T.Node _ [T.Node _ []]) = [t]
    terminalBranches (T.Node _ ts) = concatMap terminalBranches ts
    extendEdge eM e
      = if null (eTail e)
        then
          case M.lookup (fst $ eHead e, 0) eM of
            Nothing -> [e]
            Just es -> map (eMapWeight (eWeight e *) . eMapHead (const $ eHead e)) es
        else [e]


test :: [String] -> IO ()
test args = do
  let hgFile = args !! 0
  let treeIndex = read $ args !! 1 :: Int
  let f = if length args >= 3 then onlyPreterminals else id
  g <-  fmap (read :: String -> Hypergraph {-(String, Int)-}Int String Double ())
    $   readFile hgFile
  -- putStrLn $ drawHypergraph g
  Right dta <- getData
  let ts = {-filter ((< 15) . length . yield) $-} drop treeIndex $ map f (negrasToTrees dta)
  let wta = WTA.fromHypergraph {-("ROOT", 0)-}0 g
  flip mapM_ ts $ \ t -> do
    let target' = (0, {-("ROOT", 0)-}0, length $ yield t)
    let (_, g') = mapAccumIds (\ i _ -> {-i `seq`-} (i + 1, i)) (0 :: Int)
                $ dropUnreachables target'
                $ WTA.toHypergraph
                $ BH.intersect (WSA.fromList 1 $ yield t) wta
    let wta' = WTA.fromHypergraph target' g'
    let ts' = take 3
            $ filter ((t ==) . fst)
            $ filter ((0 /= ) . snd)
            $ map ( \ t' -> (t', WTA.weightTree wta' t'))
            $ nub
            $ filter (\ (T.Node r _) -> r == "ROOT")
            $ take 10000
            $ WTA.generate
            $ wta'
    let nbHg = hgToNBestHg g'
    let ts''  = map (mapFst (idTreeToLabelTree g' . hPathToTree) . pairToTuple)
              $ NB.best' nbHg target' 3
    print $ yield t
    -- putStrLn $ WTA.showWTA $ wta'
    if null (vertices g')
      then putStrLn "---!!! no parse !!!---"
      else do
        putStrLn $ "correct tree:"
        putStrLn $ "weight (in input wta):      " ++ show (WTA.weightTree wta t)
        putStrLn $ "weight (in Bar-Hillel wta): " ++ show (WTA.weightTree wta' t)
        putStrLn $ T.drawTree t
        flip mapM_ ts'' $ \ (t', w) -> do
          putStrLn $ "weight (n-best):            " ++ show w
          putStrLn $ "weight (in input wta):      " ++ show (WTA.weightTree wta t')
          putStrLn $ "weight (in Bar-Hillel wta): " ++ show (WTA.weightTree wta' t')
          putStrLn $ T.drawTree t'
    putStrLn (replicate 80 '=')


convert :: [String] -> IO ()
convert args = do
  let hgFile = args !! 0
  g <-  fmap (mapIds (const ()) . (read :: String -> Hypergraph (String, Int) String Double [Int]))
    $   readFile hgFile
  let gRev = mapTails reverse g
  let hgFile' = reverse . drop 4  . reverse $ hgFile
  writeFile ("noId/" ++ hgFile) (show g)
  writeFile ("noId/" ++ hgFile' ++ "_reverse.txt") (show gRev)


convert2 :: [String] -> IO ()
convert2 args = do
  let hgFile = args !! 0
  g <-  fmap (read :: String -> Hypergraph (String, Int) String Double ())
    $   readFile hgFile
  writeFile ("IntVertices/" ++ hgFile) (show $ snd $ verticesToInt ("ROOT", 0) g)


binarize :: [String] -> IO ()
binarize args = do
  let hgFile = args !! 0
  g <-  fmap (read :: String -> Hypergraph (String, Int) String Double ())
    $   readFile hgFile
  let g'  = snd
          $ verticesToInt [("ROOT", 0)] -- mapVertices (flip (,) 0 . show)
          $ mapLabels (fromMaybe "@")
          $ WTA.toHypergraph
          $ WTA.binarize
          $ WTA.fromHypergraph ("ROOT", 0) g
  let hgFile' = reverse . drop 4  . reverse $ hgFile
  writeFile ("binarized/" ++ hgFile' ++ "_binarized.txt") (show g')
--   putStrLn $ drawHypergraph g'


tdbh :: [String] -> IO ()
tdbh args
  = tdbhHelper args
      (\ wsa wta -> rnf (BH.intersect wsa wta) `seq` return ())


tdbhStats :: [String] -> IO ()
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
        printWTAStatistic wta''  -- putStrLn "-1\t-1\t-1"
        putStr "item-count:                "
        putStrLn  -- "-1"
          $ show
          $ length
          $ BH.getIntersectItems (const False) wsa wta
        putStr "complete-Bar-Hillel-trans: "
        putStrLn $ show $ BHC.intersectTransitionCount wsa wta
      )


printWTA :: [String] -> IO ()
printWTA args
  = tdbhHelper args (const WTA.printWTA)


readWTA :: [String] -> IO ()
readWTA args
  = tdbhHelper args (\ wsa wta -> rnf wta `seq` return ())


tdbhHelper
  :: (Num w)
  => [String]
  -> (WSA.WSA Int String w -> WTA.WTA Int String Double -> IO a)
  -> IO a
tdbhHelper args f = do
  g <-  fmap (read :: String -> Hypergraph {-(String, Int)-}Int String Double ())
    $   readFile (args !! 0)
  let yld = read (args !! 1) :: [String]
  f (WSA.fromList 1 yld) (WTA.fromHypergraph {-("ROOT", 0)-}0 g)


example :: a -> IO ()
example _ = do
  let wta' = BHC.intersect wsa wta
  let ts = WTA.transitions wta'
  flip mapM_ (WTA.states wta') $ \ v ->
     putStrLn
      $   "\\node[state] (" 
      ++  stateLab v
      ++  ") {$\\mathit{"
      ++  stateLab v
      ++  "}/"
      ++  maybe "0" show (L.lookup v (WTA.finalWeights wta'))
      ++ "$};"
  flip mapM_ ts $ \ t ->
    putStrLn $ "\\node[edge] (" ++ transLab t ++ ") {};"
  flip mapM_ ts $ \ t@(WTA.Transition l hd tl w) ->
    putStrLn
      $   "\\path ("
      ++  transLab t
      ++  ") node[above=\\ab] {$"
      ++  l
      ++  "/"
      ++  show w
      ++  "$};"
  flip mapM_ ts $ \ t@(WTA.Transition l hd tl w) -> do
    putStrLn $ "\\draw[->] (" ++ transLab t ++ ") to (" ++ stateLab hd ++ ");"
    flip mapM_ tl $ \ v ->
      putStrLn $ "\\draw[->] (" ++ stateLab v ++ ") to (" ++ transLab t ++ ");"
  where
    stateLab (p, q, p') = [p, q, p']
    transLab (WTA.Transition l hd tl w)
      = stateLab hd ++ "-" ++ concat (L.intersperse "_" (map stateLab tl))
    wta = WTA.create
            [ WTA.Transition "\\sigma" 'f' "qf" 1
            , WTA.Transition "\\alpha" 'f' ""   2
            , WTA.Transition "\\alpha" 'q' ""   2
            ]
            [ ('f', 1) ]
    wsa = WSA.create
            [ WSA.Transition "\\alpha" 'p' 'r' 1
            , WSA.Transition "\\alpha" 'r' 'p' 1
            ]
            [ ('p', 1) ]
            [ ('r', 1) ]


manySentences :: [String] -> IO ()
manySentences args = do
  g <-  fmap (read :: String -> Hypergraph {-(String, Int)-}Int String Double ())
    $   readFile (args !! 0)
  let ylds = read (args !! 1) :: [[String]]
  let wta = WTA.fromHypergraph {-("ROOT", 0)-}0 g
  let ss =  [ ["KON","ART","ADJA","NN","VVFIN","KOUS","PRF","ART","NN","KOKOM","NE","APPR","NE","ADJD","VVINF","VAFIN","PPER","VAFIN","ADV","PPER","VVFIN","ART","NN","PTKVZ"]
            , ["ART","ADJA","NN","PRELS","ADV","APPR","CARD","NN","APPRART","NN","VVFIN","VVFIN","PIS","PRELS","PTKNEG","VVFIN","KOUS","NE","APPR","NN","ART","ADJA","NN","VAFIN"]
            , ["ADV","VVFIN","PPER","ART","NN","APPO","APPR","ADJA","KON","ADJA","NN","PIAT","NN","PRELS","VVFIN","NE","VAFIN","PIS","APPR","PPER","KON","PRELS","ART","NN","VVFIN"]
            , ["CARD","NN","PRELS","NE","ADV","ADV","VVFIN","NN","KON","NN","VVFIN","PPER","ADV","PTKZU","VVINF"]
            , ["PIS","PRELS","PRF","APPR","ART","NN","VVFIN","VAFIN","NE","NE","PRELS","NE","APPR","ART","NN","ART","NN","NE","CARD","NN","NN","VVFIN"]
            , ["ART","NN","PROAV","PWAT","NN","NN","NE","ART","NN","ADV","ART","NN","APPR","NE","NN","VAFIN","VVFIN","PRF","PROAV","PWAV","NE","PPOSAT","NN","VVFIN"]
            ]
  -- let ss = ["abc", "def"]
  let wsa = combineWSAs $ map (WSA.fromList 1) ylds
  putStrLn $ unlines $ map show $ WSA.transitions wsa
  putStrLn $ unlines $ map show $ WSA.initialWeights wsa
  putStrLn $ unlines $ map show $ WSA.finalWeights wsa
  rnf (BH.intersect wsa wta) `seq` return ()
  where
    combineWSAs xs = let (ts, is, fs) = go (0 :: Int) xs in WSA.create ts is fs
      where
        go _ [] = ([], [], [])
        go n (x:xs)
          = let (ts, is, fs) = go (n + 1) xs
            in
            ( (map (\ (WSA.Transition t p p' w) -> WSA.Transition t (n, p) (n, p') w) (WSA.transitions x) ++ ts)
            , (map (\ (p, w) -> ((n, p), w)) (WSA.initialWeights x) ++ is)
            , (map (\ (p, w) -> ((n, p), w)) (WSA.finalWeights x) ++ fs)
            )


manySentencesZigZag :: [String] -> IO ()
manySentencesZigZag args = do
  g <-  fmap (read :: String -> Hypergraph {-(String, Int)-}Int String Double ())
    $   readFile (args !! 0)
  let ylds = read (args !! 1) :: [[String]]
  let wta = WTA.fromHypergraph {-("ROOT", 0)-}0 g
  let wsa = combineWSAs $ map (WSA.fromList 1) ylds
  putStrLn $ unlines $ map show $ WSA.transitions wsa
  putStrLn $ unlines $ map show $ WSA.initialWeights wsa
  putStrLn $ unlines $ map show $ WSA.finalWeights wsa
  rnf (BH.intersect wsa wta) `seq` return ()
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
  let wta = WTA.fromHypergraph {-("ROOT", 0)-}0 g
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
--   WTA.printWTA $ BH.intersect wsa wta
--   print $ length $ WTA.transitions $ BH.intersect wsa wta
  rnf (BH.intersect wsa wta) `seq` return ()


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


yield :: T.Tree a -> [a]
yield (T.Node r []) = [r]
yield (T.Node _ ts) = concatMap yield ts


hgToNBestHg
  :: (Ord v, Num w, Ord i)
  => Hypergraph v l w i
  -> ([v], v -> [(i, [v])], i -> [w] -> w)
hgToNBestHg g
  = ( vertices g
    , \ v -> map (\ e -> (eId e, eTail e)) $ M.findWithDefault [] v eM
    , \ i ws -> negate $ product (map negate ws) * M.findWithDefault 0 i iM
    )
  where
    eM = edgesM g
    iM = M.fromList $ map (\ e -> (eId e, eWeight e)) $ edges g


hPathToTree :: NB.HPath a -> T.Tree a
hPathToTree (NB.B i bs)
  = T.Node i (map hPathToTree bs)


idTreeToLabelTree :: (Ord i, Functor f) => Hypergraph v l w i -> f i -> f l
idTreeToLabelTree g
  = fmap (\ i -> M.findWithDefault (error "unknown eId") i iM)
  where
    iM = M.fromList $ map (\ e -> (eId e, eLabel e)) $ edges g


pairToTuple :: NB.Pair a b -> (a, b)
pairToTuple (NB.P x y) = (x, y)


printWTAStatistic :: WTA.WTA q t w -> IO ()
printWTAStatistic wta = do
  putStr   $ show $ length $ WTA.transitions  wta
  putStr "\t"
  putStr   $ show $ length $ WTA.states       wta
  putStr "\t"
  putStrLn $ show $ length $ WTA.finalWeights wta
