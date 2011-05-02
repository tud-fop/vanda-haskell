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
module Algorithms.EMDictionaryArray
( train
, train'
) where


import Tools.PrettyPrint (putStrColumns)

import Data.Array{-.Diff-}
import qualified Data.List as L
import qualified Data.Map as M
import System (getArgs)

-- import Debug.Trace


train :: (Ord e, Ord f) => Double -> [([e], [f])] -> [((e, f), Double)]
train delta = last . train' delta


train' :: (Ord e, Ord f) => Double -> [([e], [f])] -> [[((e, f), Double)]]
train' delta corpus
  = let (corpus', (eA, fA)) = corpusToInts corpus
        (eLower, eUpper) = bounds eA
        (fLower, fUpper) = bounds fA
        p = 1 / fromIntegral (fUpper - fLower + 1)
        s = fmap (const p) $ array ((eLower, fLower), (eUpper, fUpper)) []
    in
    map
      ( map (\ ((e, f), w) -> ((eA ! e, fA ! f), w))
      . assocs
      )
  $ iter delta corpus' s


iter
  :: Double
  -> [([Int], [Int])]
  -> Array (Int, Int) Double
  -> [Array (Int, Int) Double]
iter delta corpus s
  = s
  : let s' = step corpus s
        ((eLower, fLower), (eUpper, fUpper)) = bounds s'
        d = for 0 [eLower .. eUpper] $ \ d'' e ->
              for d'' [fLower .. fUpper] $ \ d' f ->
                let i = (e, f) in max d' $ abs $ s ! i - s' ! i
    in {-traceShow d $-}
    if d < delta
    then [s']
    else iter delta corpus s'


step :: [([Int], [Int])] -> Array (Int, Int) Double -> Array (Int, Int) Double
step corpus s
  = normalize
  $ for (fmap (const 0) s) corpus $ \ c'' (es, fs) ->
      for c'' fs $ \ c' f ->
        let norm = (/) 1 $ for 0 es $ \ summ e -> summ + s ! (e, f) in
        for c' es $ \ c e ->
          let i = (e, f)
              w = c ! i + s ! i * norm
          in w `seq` c // [(i, w)]


normalize :: Array (Int, Int) Double -> Array (Int, Int) Double
normalize c
  = let ((eLower, fLower), (eUpper, fUpper)) = bounds c
    in for c [eLower .. eUpper] $ \ s' e ->
        let norm = (/) 1
                 $ for 0 [fLower .. fUpper] $ \ summ' f ->
                     summ' + c ! (e, f) in
        for s' [fLower .. fUpper] $ \ s f ->
          let i = (e, f)
              w = s ! i * norm
          in w `seq` s // [(i, w)]


for :: a -> [b] -> (a -> b -> a) -> a
for i xs f = L.foldl' f i xs


corpusToInts
  :: (Ord e, Ord f)
  => [([e], [f])]
  -> ([([Int], [Int])], (Array Int e, Array Int f))
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
      = array (0, maxI)
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
      -> mainTrain (read delta) corpus
    ["csv", delta, corpus]
      -> mainSteps (read delta) corpus
    ["generate-test-corpus", wordCnt, wordCntPerSentence]
      -> mainGenerateTestCorpus (read wordCnt) (read wordCntPerSentence)
    (_:_)
      -> error "Unknown action or wrong number of arguments."
    _
      -> do
          putStrLn "Expecting an action with its arguments:"
          putStrLn "  best <maximal delta> <corpus file>"
          putStrLn "  csv <maximal delta> <corpus file>"


mainTrain :: Double -> String -> IO ()
mainTrain delta corpus
  = parseCorpus corpus
  >>= putStrColumns [" | "]
    . (\ (x, y, z) -> ["e" : x, "f" : y, "p(f|e)" : z])
    . unzip3
    . fmap (\ ((e, f), w) -> (e, f, show w))
    . filter ((<) 0.1 . snd)
    . train delta


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
