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

-- | Train a dictionary with a bilingual corpus using an instance of the
-- expectation maximization algorithm. The algorithm is taken from Figure 2 of
-- the following paper where step 3b is replaced as suggested on the following
-- page.
--
-- * Kevin Knight.
--   /Squibs and discussions - Decoding complexity in word-replacement translation models./
--   Computational Linguistics, 25(4), 1999.
--   <http://ldc.upenn.edu/acl/J/J99/J99-4005.pdf>
module Algorithms.EMDictionaryIntMap
( train
, train'
, train''
, main
, corpusToInts
) where

import Tools.PrettyPrint (putStrColumns)

import Control.Exception (bracket)
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM
import System.Environment (getArgs)
import System.IO

-- import Debug.Trace

-- ---------------------------------------------------------------------------

data Array2 a = Array2 a (IM.IntMap (IM.IntMap a))


(!) :: Array2 a -> (IM.Key, IM.Key) -> a
(Array2 def m) ! (i, j)
  = IM.findWithDefault def j
  $ IM.findWithDefault IM.empty i m


(//) :: Array2 t -> [((Int, IM.Key), t)] -> Array2 t
a // [] = a
(Array2 def m) // (((i, j), v) : xs)
  = Array2 def
      ( IM.alter
          (Just . maybe (IM.singleton j v) (IM.insert j v))
          i
          m
      )
  // xs

-- ---------------------------------------------------------------------------

train :: (Ord e, Ord f) => Double -> [([e], [f])] -> [((e, f), Double)]
train delta = last . train' delta


train' :: (Ord e, Ord f) => Double -> [([e], [f])] -> [[((e, f), Double)]]
train' delta corpus
  = let (corpus', (eA, fA)) = corpusToInts corpus
        (fLower, fUpper) = A.bounds fA
        p = 1 / fromIntegral (fUpper - fLower + 1)
        s = Array2 p IM.empty
    in
    map
      ( \ (Array2 _ m) ->
        concatMap
          (\ (e, m') ->
            map (\ (f, w) -> ((eA A.! e, fA A.! f), w))
          $ IM.assocs m'
          )
      $ IM.assocs m
      )
  $ iter delta corpus' s


train'' :: Double -> [([Int], [Int])] -> [IM.IntMap (IM.IntMap Double)]
train'' delta corpus
  = map (\ (Array2 _ m) -> m) $ iter delta corpus $ Array2 1 IM.empty


iter
  :: Double
  -> [([Int], [Int])]
  -> Array2 Double
  -> [Array2 Double]
iter delta corpus s
  = let s' = step corpus s
        (Array2 _ sMM) = s
        (Array2 _ sMM') = s'
        d = maximum
          $ IM.elems
          $ IM.map (maximum . IM.elems)
          $ IM.unionWith
              (\ sM sM' -> IM.unionWith (\ x y -> abs (x - y)) sM sM')
              sMM
              sMM'
    in {-traceShow d $-}
    if d < delta
    then [s']
    else s' : iter delta corpus s'


step :: [([Int], [Int])] -> Array2 Double -> Array2 Double
step corpus s
  = normalize
  $ for (Array2 0 IM.empty) corpus $ \ c'' (es, fs) ->
      for c'' fs $ \ c' f ->
        let norm = (/) 1 $ for 0 es $ \ summ e -> summ + s ! (e, f) in
        for c' es $ \ c e ->
          let i = (e, f)
              w = c ! i + s ! i * norm
          in w `seq` c // [(i, w)]
  where
    for :: a -> [b] -> (a -> b -> a) -> a
    for i xs f = L.foldl' f i xs


normalize :: Array2 Double -> Array2 Double
normalize (Array2 def cM)
  = Array2 def
  $ flip IM.map cM $ \ cM' ->
      let norm = (/) 1 $ L.foldl' (+) 0 $ IM.elems cM'
      in  IM.map ((*) norm) cM'


corpusToInts
  :: (Ord e, Ord f)
  => [([e], [f])]
  -> ([([Int], [Int])], (A.Array Int e, A.Array Int f))
corpusToInts corpus
  = let (eI, eM) = getMap fst
        (fI, fM) = getMap snd
    in  ( map (\ (es, fs) -> (map ((M.!) eM) es, map ((M.!) fM) fs)) corpus
        , (mapToArray eI eM, mapToArray fI fM)
        )
  where
    getMap f
      = M.mapAccum (\ i _ -> let i' = i + 1 in (i', i')) (-1)
      $ M.fromList (map (\ x -> (x, ())) . concatMap f $ corpus)
    mapToArray maxI m
      = A.array (0, maxI)
      $ map (\ (x, i) -> (i, x))
      $ M.toList m

-- ---------------------------------------------------------------------------

parseCorpus :: String -> IO [([String], [String])]
parseCorpus file
  =   readFile file
  >>= return
    . pair
    . filter (not . null)
    . fmap words
    . lines
  where
    pair [] = []
    pair (e : f : xs) = (e, f) : pair xs
    pair _ = error $ file ++ ": Odd number of sentences."


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["best", delta, corpus]
      -> mainTrain False (read delta) corpus
    ["bestSwap", delta, corpus]
      -> mainTrain True (read delta) corpus
    ["csv", delta, corpus]
      -> mainSteps (read delta) corpus
    ["unzipCorpus", corpus]
      -> mainUnzipCorpus corpus
    ["generate-test-corpus", wordCnt, wordCntPerSentence]
      -> mainGenerateTestCorpus (read wordCnt) (read wordCntPerSentence)
    (_:_)
      -> error "Unknown action or wrong number of arguments."
    _
      -> do
          putStrLn "Expecting an action with its arguments:"
          putStrLn "  best <maximal delta> <corpus file>"
          putStrLn "  csv <maximal delta> <corpus file>"


mainTrain :: Bool -> Double -> String -> IO ()
mainTrain swap delta corpus
  = parseCorpus corpus
  >>= putStrColumns [" | "]
    . (\ (x, y, z) -> ["e" : x, "f" : y, "p(f|e)" : z])
    . unzip3
    . fmap (\ ((e, f), w) -> (e, f, show w))
    . filter ((<) 0.1 . snd)
    . train delta
    . (if swap then map (\ (a, b) -> (b, a)) else id)


mainSteps :: Double -> String -> IO ()
mainSteps delta corpus = do
  xs <- fmap (train' delta) $ parseCorpus corpus
  putStr
    . unlines
    . map (L.intercalate "\t")
    . (\ (x, y, z) -> [x, y, z])
    . unzip3
    . fmap (\ ((e, f), w) -> (e, f, {-replaceComma $-} show w))
    $ head xs
  putStr
    . unlines
    . fmap
      ( L.intercalate "\t"
      . fmap ({-replaceComma .-} show . snd)
      )
    $ tail xs
--   where
--     replaceComma "" = ""
--     replaceComma ('.' : cs) = ',' : cs
--     replaceComma (c   : cs) = c   : replaceComma cs


mainUnzipCorpus :: FilePath -> IO ()
mainUnzipCorpus file
  = bracket (openFile (file ++ ".e.txt") WriteMode) hClose $ \ hE ->
    bracket (openFile (file ++ ".f.txt") WriteMode) hClose $ \ hF ->
      parseCorpus file >>= mapM_ (\ (es, fs) -> do
                                   hPutStrLn hE $ unwords es
                                   hPutStrLn hF $ unwords fs
                                 )


mainGenerateTestCorpus :: Int -> Int -> IO ()
mainGenerateTestCorpus n m
  = putStr
  . L.intercalate "\n"
  $ map
      (\ i ->
        unlines
          [ L.intercalate " " $ map show [i + 1 .. i + m]
          , L.intercalate " " $ map show [i + m, i + m - 1 .. i + 1]
          ]
      )
      [1 .. n - m]
