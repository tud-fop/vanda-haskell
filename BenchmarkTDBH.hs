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
import qualified Algorithms.WTABarHillelTopDownBinarizing as BHB
import qualified Algorithms.WTABarHillelComplete as BHC
import Tools.Miscellaneous (mapFst, mapSnd)

import Control.DeepSeq
import qualified Data.IntMap as IM
import Data.List (nub)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Tree as T
import Data.Traversable (Traversable, mapAccumL)
import qualified Random as R
import System(getArgs)
import System.IO (hSetBuffering, stdout, BufferMode(..))


main :: IO ()
main = do
  args <- getArgs
  case args of
    ("print" : xs) -> printFileHG xs
    ["printYields"] -> printYields
    ["printRandomYields", drp, n] -> printRandomYields (read drp) (read n)
    ["calcIntDict"] -> calcIntDict
    ("train" : xs) -> train xs
    ["test", hgFile, treeIndex, defol] ->
      test hgFile (read treeIndex) (read defol)
    ("manySentences" : xs) -> manySentences xs
    ("manySentencesZigZag" : xs) -> manySentencesZigZag xs
    ("evenSentencelength" : xs) -> evenSentencelength xs
    ["memTestString"] -> memTestString
    ["memTestInt"] -> memTestInt
    ["preprocess", hgFile] -> preprocess hgFile
    ["synthBench", m, n, bin, action, algo] ->
      synthBench (read m) (read n) (read bin) action algo
    ["synthBench2", n, bin, action, algo] ->
      synthBench2 (read n) (read bin) action algo
    ["benchmark", hgFile, yieldFile, yieldNr, action, algo] ->
      benchmark hgFile yieldFile (read yieldNr) action algo
    _ -> putStrLn "Unknown action or wrong number of arguments."


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


memTestString :: IO ()
memTestString = do
  dta <- {-fmap (take 20000)-} getData
  print $ rnf dta
  print $ L.foldl' (+) 0 [1 :: Int .. 10000000]
  print $ rnf dta


memTestInt :: IO ()
memTestInt = do
  dta <- fmap (corpusToInt {-. take 20000-}) getData
  print $ rnf (snd dta)
  print $ rnf dta
--   putStr $ unlines $ map (T.drawTree . fmap show) $ snd dta
  print $ L.foldl' (+) 0 [1 :: Int .. 10000000]
  print $ rnf dta


corpusToInt
  :: (Ord k, Traversable a, Traversable b)
  => a (b k) -> (M.Map k Int, a (b Int))
corpusToInt
  = mapAccumL
      ( mapAccumL
        (\ m k -> let s = M.size m
                  in maybe (M.insert k s m, s) ((,) m) (M.lookup k m)
        )
      )
      M.empty


printYields :: IO ()
printYields
  =   getData
  >>= putStr
    . unlines
    . map (show . yield . defoliate)
    . snd
    . corpusToInt


printRandomYields :: Int -> Int -> IO ()
printRandomYields drp n = do
  ys <- fmap (map yield . map defoliate . snd . corpusToInt) getData
  rnf ys `seq` return ()
  putStr
    $ unlines
    $ map show
    $ concatMap snd
    $ snd
    $ mapAccumL
        (\ g (l, xs) -> let (g', rs) = rands (0, length xs - 1) g n
            in (g', (l, map (xs !!) rs)))
        (R.mkStdGen 0)
    $ map (mapSnd nub)
    $ M.toList
    $ M.fromListWith (++) [(length y, [y]) | y <- drop drp ys]
  where
    rands (lo, hi) g0 m = if hi - lo < m then (g0, [lo .. hi]) else go m g0 []
      where
        go 0 g xs = (g, xs)
        go i g xs = let (x, g') = R.randomR (lo, hi) g
                    in if elem x xs then go i g' xs else go (i - 1) g' (x : xs)


calcIntDict :: IO ()
calcIntDict = do
  (m, ts) <- fmap corpusToInt getData
  rnf ts `seq`
    writeFile "IntDict.txt" (show $ map (\ (x, y) -> (y, x)) $ M.toList m)


train :: [String] -> IO ()
train args = do
  hSetBuffering stdout NoBuffering
  let its = read (args !! 0)
  let exs = read (args !! 1)
  ts <- fmap (take exs . snd . corpusToInt) getData
  let trains = map defoliate ts
  let exPretermToTerm = mapIds (const []) $ SPG.initialize $ RE.extractHypergraph $ concatMap terminalBranches ts
  let gsgens
        = take (its + 1)
        $ SPG.train'
            trains
            0 {-"ROOT"-}
            (RE.extractHypergraph trains :: Hypergraph Int Int Double ())
            (R.mkStdGen 0)
        :: [(Hypergraph (Int, Int) Int Double [Int], R.StdGen)]
  flip mapM_ (zip [(0 :: Int) ..] gsgens) $ \ (n, (g, _)) -> do
    writeFile ("hg_" ++ show exs ++ "_" ++ show n ++ ".txt") (show $ mapIds (const ()) g)
    writeFile ("hg_" ++ show exs ++ "_" ++ show n ++ "_withTerminals.txt")
      $ show
      $ mapIds (const ())
      $ hypergraph
          (  filter (null . eTail) (edges exPretermToTerm)
          ++ concatMap (extendEdge (edgesM exPretermToTerm)) (edges g)
          )
    putStrLn $ "running state-split iteration " ++ show (n + 1) ++ " ..."
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


test :: String -> Int -> Bool -> IO ()
test hgFile treeIndex defol = do
  let target = 0
  let f = if defol then map defoliate else id
  g <-  fmap (read :: String -> Hypergraph Int Int Double Int)
    $   readFile hgFile
  ts <- fmap (f . snd . corpusToInt) getData
  intDict <- fmap (IM.fromList . read :: String -> IM.IntMap String) $ readFile "IntDict.txt"
  let intDictLookup v = IM.findWithDefault (show v) v intDict
  let wta = WTA.WTA (M.singleton target 1) g
  print $ rnf $ take treeIndex ts
  flip mapM_ (drop treeIndex ts) $ \ t -> do
    let target' = (0, target, length $ yield t)
    let g' = dropUnreachables target'
           $ WTA.toHypergraph
           $ BH.intersect (WSA.fromList 1 $ yield t) wta
    let wta' = WTA.WTA (M.singleton target' 1) g'
    let ts'  = map (mapFst (fmap eLabel))
              $ nBest' 3 target' g'
    print $ fmap intDictLookup $ yield t
    -- putStrLn $ WTA.showWTA $ wta'
    putStrLn $ rnf g' `seq` "Intersection calculated."
    if null (vertices g')
      then putStrLn "---!!! no parse !!!---"
      else do
        putStrLn $ "correct tree:"
        putStrLn $ "weight (in input wta):      " ++ show (WTA.weightTree wta t)
        putStrLn $ "weight (in Bar-Hillel wta): " ++ show (WTA.weightTree wta' t)
        putStrLn $ T.drawTree $ fmap intDictLookup t
        flip mapM_ ts' $ \ (t', w) -> do
          putStrLn $ "weight (n-best):            " ++ show w
          putStrLn $ "weight (in input wta):      " ++ show (WTA.weightTree wta t')
          putStrLn $ "weight (in Bar-Hillel wta): " ++ show (WTA.weightTree wta' t')
          putStrLn $ T.drawTree $ fmap intDictLookup t'
    putStrLn (replicate 80 '=')


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


preprocess :: FilePath -> IO ()
preprocess hgFile = do
  g <-  fmap (read :: String -> Hypergraph (Int, Int) Int Double ())
    $   readFile hgFile
  let gInt = snd $ mapAccumIds (\ (i : is) _ -> (is, i)) [0 :: Int ..]
           $ snd $ verticesToInt (0, 0) g
  let gBin = snd $ mapAccumIds (\ (i : is) _ -> (is, i)) [0 :: Int ..]
           $ snd $ verticesToInt [0]
           $ mapLabels (fromMaybe (-1))
           $ binarize gInt
  let file = reverse . tail . dropWhile ('.' /=) . reverse $ hgFile
  writeFile (file ++ "_IntVertices.txt") $ show gInt
  writeFile (file ++ "_IntVertices_binarized.txt") $ show gBin


benchmark :: FilePath -> FilePath -> Int -> [Char] -> [Char] -> IO ()
benchmark hgFile yieldFile yieldNr action algo = do
  g <-  fmap (read :: String -> Hypergraph Int Int Double Int)
    $   readFile hgFile
  y <-  fmap ((read :: String -> [Int]) . (!! yieldNr) . lines)
    $   readFile yieldFile
  benchmark'1 algo action (WSA.fromList 1 y) (WTA.WTA (M.singleton 0 1) g)


synthBench :: Int -> Int -> Bool -> String -> String -> IO ()
synthBench m n bin action algo
  = if bin
    then benchmark'1 algo action wsaB wtaB
    else benchmark'1 algo action wsa  wta
  where
    w   = 1 / fromIntegral n :: Double
    wta = WTA.wtaCreate
            [((), 1)]
            [hyperedge () (replicate i ()) i w i | i <- 0 : [2 .. n]]
    wsa = WSA.fromList 1 (replicate m 0)
    wtaB = WTA.WTA (M.singleton [()] 1)
         $ snd
         $ mapAccumIds (\ (i : is) _ -> (is, i)) [0 :: Int ..]
         $ binarize' (WTA.toHypergraph wta)
    wsaB = WSA.justTerminals wsa


synthBench2 :: Int -> Bool -> String -> String -> IO ()
synthBench2 n bin action algo
  = if bin
    then benchmark'1 algo action wsaB wtaB
    else benchmark'1 algo action wsa  wta
  where
    wta = WTA.wtaCreate
            [(0, 1)]
            [ hyperedge 0 [] 0 0.5 0 :: Hyperedge Int Int Double Int
            , hyperedge 0 [0, 1] 1 0.25 1
            , hyperedge 0 [1, 0] 2 0.25 2
            , hyperedge 1 [] 0 1 3
            ]
    wsa = WSA.fromList 1 (replicate n 0)
    wtaB = WTA.WTA (M.singleton [0] 1)
         $ snd
         $ mapAccumIds (\ (i : is) _ -> (is, i)) [0 :: Int ..]
         $ binarize' (WTA.toHypergraph wta)
    wsaB = WSA.justTerminals wsa


benchmark'1
  :: ( Ord p, Ord q, Ord t, Ord i, Ord w
     , Num w
     , Show p, Show q, Show t, Show i
     , NFData p, NFData t, NFData w, NFData q, NFData i
     )
  => String
  -> String
  -> WSA.WSA p t w
  -> WTA.WTA q t w i
  -> IO ()
benchmark'1 algo action wsa wta
  = case algo of
      "bha" ->
        benchmark'2 action BH.intersect BH.intersectionItemCount wsa wta
      "bhb" ->
        benchmark'2 action BHB.intersect BHB.intersectionItemCount wsa wta
      "bhc" ->
        benchmark'2 action BHC.intersect (const . const (0 :: Int)) wsa wta
      _ ->
        putStrLn $ "Unknown intersection algorithm: " ++ algo


benchmark'2
  ::  ( Ord q'
      , Show q, Show q', Show t, Show t', Show w, Show i
      , NFData p, NFData q, NFData q', NFData t, NFData t', NFData w, NFData i
      )
  => String
  -> (WSA.WSA p t w -> WTA.WTA q t w i -> WTA.WTA q' t' w i)
  -> (WSA.WSA p t w -> WTA.WTA q t w i -> Int)
  -> WSA.WSA p t w
  -> WTA.WTA q t w i
  -> IO ()
benchmark'2 action inter itemCounter wsa wta
  = case action of
      "print" -> do
        putStr $ WTA.drawWTA wta
        putStr $ WTA.drawWTA $ inter wsa wta
      "pretend" ->
        rnf wsa `seq` rnf wta `seq` return ()
      "bench" ->
        rnf wsa `seq` rnf wta `seq` rnf (inter wsa wta) `seq` return ()
      "stats" ->
        let wta' = inter wsa wta
        in print $
            [ ("wsaStates", show $ length $ WSA.states wsa)
            , ("wsaEdges" , show $ length $ WSA.transitions wsa)
            , ("states", show $ length $ WTA.states wta')
            , ("edges" , show $ length $ edges $ WTA.toHypergraph wta')
            , ("items" , show $ itemCounter wsa wta)
            ]
      _ -> putStrLn $ "Unknown synthBench action: " ++ action


-- ---------------------------------------------------------------------------

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
