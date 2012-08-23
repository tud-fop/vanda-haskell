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
, trainInt
, trainIntAll
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


train :: (Ord e, Ord f) => Double -> [([e], [f])] -> [[((e, f), Double)]]
train delta corpus
  = let (corpus', (eA, fA)) = corpusToInts corpus
  in map ( map (\ ((e, f), w) -> ((eA A.! e, fA A.! f), w))
         . assocs
         ) $ trainInt delta corpus'


assocs :: IM.IntMap (IM.IntMap Double) -> [((IM.Key, IM.Key), Double)]
assocs
  = concatMap (uncurry $ \ e -> map (\ (f, w) -> ((e, f), w)) . IM.assocs)
  . IM.assocs


trainInt
  :: Double
  -> [([Int], [Int])]
  -> [IM.IntMap (IM.IntMap Double)]
trainInt delta
  = takeWhile' (\ m1 m2 -> d m1 m2 >= delta) . trainIntAll
  where
    takeWhile' f (x0 : xs@(x1 : _))
      | f x0 x1 = x0 : takeWhile' f xs
      | otherwise = x0 : x1 : []
    takeWhile' _ xs = xs
    d mm1 mm2
      = maximum
      $ IM.elems
      $ IM.map (maximum . IM.elems)
      $ IM.unionWith
          (\ m1 m2 -> IM.unionWith (\ x y -> abs (x - y)) m1 m2)
          mm1
          mm2


trainIntAll :: [([Int], [Int])] -> [IM.IntMap (IM.IntMap Double)]
trainIntAll c = let m0 = step c 1 IM.empty
                 in m0 : iterate (step c 0) m0


step
  :: [([Int], [Int])]
  -> Double
  -> IM.IntMap (IM.IntMap Double)
  -> IM.IntMap (IM.IntMap Double)
step corpus def s
  = normalize
  $ for IM.empty corpus $ \ c'' (es, fs) ->
      for c'' fs $ \ c' f ->
        let ps = map (fnd f) es
            norm = 1 / L.foldl' (+) 0 ps  -- = 1 / sum ps
        in for c' (zip es ps) $ \ c (e, p) ->
          let w = p * norm
          in if w == 0
          then c
          else (\ m -> m IM.! e `seq` m)  -- delete this for strict containers
             $ IM.alter
                 (Just . maybe (IM.singleton f w) (IM.insertWith' (+) f w))
                 e c
  where
    for :: a -> [b] -> (a -> b -> a) -> a
    for i xs f = L.foldl' f i xs
    fnd :: IM.Key -> IM.Key -> Double
    fnd j i = IM.findWithDefault def j (IM.findWithDefault IM.empty i s)


normalize :: IM.IntMap (IM.IntMap Double) -> IM.IntMap (IM.IntMap Double)
normalize
  = IM.mapMaybe $ \ m -> nothingWhen IM.null
                       $ let norm = 1 / IM.foldl' (+) 0 m
                         in IM.mapMaybe (nothingWhen (0 ==) . (norm *)) m


nothingWhen :: (a -> Bool) -> a -> Maybe a
nothingWhen f x | f x       = Nothing
                | otherwise = Just x


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
    . last
    . train delta
    . (if swap then map (\ (a, b) -> (b, a)) else id)


mainSteps :: Double -> String -> IO ()
mainSteps delta corpus = do
  xs <- fmap (train delta) $ parseCorpus corpus
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
