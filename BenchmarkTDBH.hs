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

import qualified Data.WTA as WTA
import qualified Data.WSA as WSA
import Data.Hypergraph
import qualified Parser.NegraLazy as N
import qualified Algorithms.RuleExtraction as RE
import qualified Algorithms.StateSplit as SPG
import qualified Algorithms.WTABarHillelTopDown as BH
import qualified Algorithms.WTABarHillelComplete as BHC
import Tools.Miscellaneous (mapFst)
import TestData.TestHypergraph

import Control.DeepSeq
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Tree as T
import qualified Random as R
import System(getArgs)


main :: IO ()
main = do
  args <- getArgs
  case head args of
    "print" -> printFileHG (tail args)
    "printYields" -> printYields (tail args)
    "train" -> train (tail args)
    "test" -> test (tail args)
    "test2" -> test2 (tail args)
    "convert" -> convert (tail args)
    "convert2" -> convert2 (tail args)
    "binarize" -> binarizeHypergraph (tail args)
    "tdbh" ->  tdbh (tail args)
    "tdbhStats" ->  tdbhStats (tail args)
    "printWTA" -> printWTA (tail args)
    "readWTA" -> readWTA (tail args)
    -- "example" -> example (tail args)
    "manySentences" -> manySentences (tail args)
    "manySentencesZigZag" -> manySentencesZigZag (tail args)
    "evenSentencelength" -> evenSentencelength (tail args)
    _ -> putStrLn "Unknown action."


printFileHG :: [String] -> IO ()
printFileHG args
  = readFile (args !! 0)
  >>= putStrLn
    . drawHypergraph
    . (read :: String -> Hypergraph {-(String, Int)-}Int String Double ())


getData :: IO [T.Tree String]
getData
  = fmap (convertNegra . N.parseNegra)
  $ N.readFileLatin1
      "/var/local/share/gdp/nlp/resources/tigercorpus2.1/corpus/tiger_release_aug07.export"


printYields :: a -> IO ()
printYields _
  =   getData
  >>= putStr
    . unlines
    . map (show . yield . defoliate)


train :: [String] -> IO ()
train args = do
  let its = read (args !! 0)
  let exs = read (args !! 1)
  ts <- fmap (take exs) getData
  let trains = map defoliate ts
  let exPretermToTerm = mapIds (const []) $ SPG.initialize $ RE.extractHypergraph $ concatMap terminalBranches ts
  let gsgens
        = take (its + 1)
        $ SPG.train'
            trains
            "ROOT"
            (RE.extractHypergraph trains :: Hypergraph String String Double ())
            (R.mkStdGen 0)
        :: [(Hypergraph (String, Int) String Double [Int], R.StdGen)]
  flip mapM_ (zip [(0 :: Int) ..] gsgens) $ \ (n, (g, _)) -> do
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
  let f = if length args >= 3 then map defoliate else id
  g <-  fmap (read :: String -> Hypergraph {-(String, Int)-}Int String Double ())
    $   readFile hgFile
  -- putStrLn $ drawHypergraph g
  ts <- fmap ({-filter ((< 15) . length . yield) $-} drop treeIndex . f) getData
  let wta = WTA.WTA (M.singleton {-("ROOT", 0)-}0 1) g
  flip mapM_ ts $ \ t -> do
    let target' = (0, {-("ROOT", 0)-}0, length $ yield t)
    let g' = dropUnreachables target'
           $ WTA.toHypergraph
           $ BH.intersect (WSA.fromList 1 $ yield t) wta
    let wta' = WTA.WTA (M.singleton target' 1) g'
    let ts'  = map (mapFst (fmap eLabel))
              $ nBest' 3 target' g'
    print $ yield t
    -- putStrLn $ WTA.showWTA $ wta'
    if null (vertices g')
      then putStrLn "---!!! no parse !!!---"
      else do
        putStrLn $ "correct tree:"
        putStrLn $ "weight (in input wta):      " ++ show (WTA.weightTree wta t)
        putStrLn $ "weight (in Bar-Hillel wta): " ++ show (WTA.weightTree wta' t)
        putStrLn $ T.drawTree t
        flip mapM_ ts' $ \ (t', w) -> do
          putStrLn $ "weight (n-best):            " ++ show w
          putStrLn $ "weight (in input wta):      " ++ show (WTA.weightTree wta t')
          putStrLn $ "weight (in Bar-Hillel wta): " ++ show (WTA.weightTree wta' t')
          putStrLn $ T.drawTree t'
    putStrLn (replicate 80 '=')


test2 :: [String] -> IO ()
test2 _ = do
  flip mapM_ (tail testHypergraphs :: [Hypergraph Char Char Double ()]) $ \ g -> do
    flip mapM_ (vertices g) $ \ target -> do
      let ts  = map (mapFst (fmap eLabel))
              $ nBest' 5 target g
      putStrLn $ drawHypergraph g
      putStrLn $ "target: " ++ show target
      putStrLn ""
      flip mapM_ ts $ \ (t, w) -> do
        putStrLn $ "weight (n-best):            " ++ show w
        putStrLn $ "weight (in input wta):      " ++ show (WTA.weightTree (WTA.WTA (M.singleton target 1) g) t)
        putStrLn $ T.drawTree $ fmap show t
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


binarizeHypergraph :: [String] -> IO ()
binarizeHypergraph args = do
  let hgFile = args !! 0
  g <-  fmap (read :: String -> Hypergraph (String, Int) String Double ())
    $   readFile hgFile
  let g'  = snd
          $ verticesToInt [("ROOT", 0)] -- mapVertices (flip (,) 0 . show)
          $ mapLabels (fromMaybe "@")
          $ binarize g
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
        let target' = fst $ head $ M.toList $ WTA.finalWeights wta'
        let wta'' = WTA.WTA (M.singleton target' 1)
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
  = tdbhHelper args $ const $ putStr . WTA.drawWTA


readWTA :: [String] -> IO ()
readWTA args
  = tdbhHelper args (\ _ wta -> rnf wta `seq` return ())


tdbhHelper
  :: (Num w)
  => [String]
  -> (WSA.WSA Int String w -> WTA.WTA Int String Double () -> IO a)
  -> IO a
tdbhHelper args f = do
  g <-  fmap (read :: String -> Hypergraph {-(String, Int)-}Int String Double ())
    $   readFile (args !! 0)
  let yld = read (args !! 1) :: [String]
  f (WSA.fromList 1 yld) (WTA.WTA (M.singleton {-("ROOT", 0)-}0 1) g)

{-
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
  flip mapM_ ts $ \ t@(WTA.Transition l _ _ w) ->
    putStrLn
      $   "\\path ("
      ++  transLab t
      ++  ") node[above=\\ab] {$"
      ++  l
      ++  "/"
      ++  show w
      ++  "$};"
  flip mapM_ ts $ \ t@(WTA.Transition _ hd tl _) -> do
    putStrLn $ "\\draw[->] (" ++ transLab t ++ ") to (" ++ stateLab hd ++ ");"
    flip mapM_ tl $ \ v ->
      putStrLn $ "\\draw[->] (" ++ stateLab v ++ ") to (" ++ transLab t ++ ");"
  where
    stateLab (p, q, p') = [p, q, p']
    transLab (WTA.Transition _ hd tl _)
      = stateLab hd ++ "-" ++ concat (L.intersperse "_" (map stateLab tl))
    wta = WTA.create
            [ WTA.Transition "\\sigma" 'f' "qf" (1 :: Int)
            , WTA.Transition "\\alpha" 'f' ""   2
            , WTA.Transition "\\alpha" 'q' ""   2
            ]
            [ ('f', 1) ]
    wsa = WSA.create
            [ WSA.Transition "\\alpha" 'p' 'r' (1 :: Int)
            , WSA.Transition "\\alpha" 'r' 'p' 1
            ]
            [ ('p', 1) ]
            [ ('r', 1) ]
-}

manySentences :: [String] -> IO ()
manySentences args = do
  g <-  fmap (read :: String -> Hypergraph {-(String, Int)-}Int String Double ())
    $   readFile (args !! 0)
  let ylds = read (args !! 1) :: [[String]]
  let wta = WTA.WTA (M.singleton {-("ROOT", 0)-}0 1) g
  let wsa = combineWSAs $ map (WSA.fromList 1) ylds
  putStrLn $ unlines $ map show $ WSA.transitions wsa
  putStrLn $ unlines $ map show $ WSA.initialWeights wsa
  putStrLn $ unlines $ map show $ WSA.finalWeights wsa
  rnf (BH.intersect wsa wta) `seq` return ()
  where
    combineWSAs xs = let (ts, is, fs) = go (0 :: Int) xs in WSA.create ts is fs
      where
        go _ [] = ([], [], [])
        go n (x:xs')
          = let (ts, is, fs) = go (n + 1) xs'
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
  let wta = WTA.WTA (M.singleton {-("ROOT", 0)-}0 1) g
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
  let wta = WTA.WTA (M.singleton {-("ROOT", 0)-}0 1) g
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


-- | Convert a corpus in NEGRA format to a list of 'Data.Tree's with pos tags
-- as leaves.
convertNegra :: N.Negra -> [T.Tree String]
convertNegra n
  = map N.negraTreeToTree
  . concatMap N.negraToForest
  . filter (not . null)
  . map (filter fltr)
  . map N.sData
  $ N.sentences n
  where
    -- unbound = ["UNKNOWN","--","$,","$.","$("]
    unbound = map N.wtTag $ filter (not . N.wtBound) $ N.wordtags n
    fltr N.SentenceWord{N.sdPostag = tag} = notElem tag unbound
    fltr _ = True


-- | Remove the leaves from a 'T.Tree'.
defoliate :: T.Tree t -> T.Tree t
defoliate (T.Node _ []) = error "Cannot defoliate a leaf-only tree."
defoliate (T.Node x xs)
  = T.Node x $ map defoliate $ filter (not . null . T.subForest) xs


yield :: T.Tree a -> [a]
yield (T.Node r []) = [r]
yield (T.Node _ ts) = concatMap yield ts


printWTAStatistic :: (Ord q) => WTA.WTA q t w i -> IO ()
printWTAStatistic wta = do
  putStr   $ show $ length $ edges $ WTA.toHypergraph  wta
  putStr "\t"
  putStr   $ show $ length $ WTA.states       wta
  putStr "\t"
  putStrLn $ show $ M.size $ WTA.finalWeights wta
