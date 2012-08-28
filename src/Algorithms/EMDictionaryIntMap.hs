{-# LANGUAGE Rank2Types #-}

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

import Control.Arrow
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.ST (ST)
import qualified Control.Monad.Trans.State.Lazy as StL
import qualified Data.Array.IArray as A
import qualified Data.Array.ST.Safe as A
import qualified Data.Array.Unboxed as A
import qualified Data.Ix as Ix
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Tuple (swap)
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
trainIntAll c
  = map toIntMap
  $ iterate (step corpusA mapE)
  $ A.accumArray undefined 1 (A.bounds mapE) []
  where
    (corpusA, (_, (mapE, mapF))) = corpusToArrays c
    toIntMap
      = IM.map (IM.fromListWith (error "toIntMap: double entry"))
      . IM.fromListWith (++)
      . map (\ (i, p) -> (mapE A.! i, [(mapF A.! i, p)]))
      . A.assocs


step
  :: A.UArray Int Int
  -> A.UArray Int Int
  -> A.UArray Int Double
  -> A.UArray Int Double
step corpusA partitionsA probA = A.runSTUArray $ do
  let countB = A.bounds probA
  countA <- A.newArray countB 0
  forCorpusSegments corpusA 0 $ \ i ->
    let norm = 1 / L.foldl' (\ s j -> s + probA A.! j)
                            0
                            (map (corpusA A.!) [i + 1 .. i + corpusA A.! i])
    in forM_ (map (corpusA A.!) [i + 1 .. i + corpusA A.! i]) $ \ j ->
         adjustArray countA j (norm * probA A.! j +)
  let normB = (minimum $ A.elems partitionsA, maximum $ A.elems partitionsA)
  normA <- A.newArray normB 0 :: forall s. ST s (A.STUArray s Int Double)
  forM_ (Ix.range countB) $ \ i ->  -- sum partitions
    A.readArray countA i >>= adjustArray normA (partitionsA A.! i) . (+)
  forM_ (Ix.range normB) $ \ i ->  -- invert
    adjustArray normA i (1 /)
  forM_ (Ix.range countB) $ \ i ->  -- normalize counts
    A.readArray normA (partitionsA A.! i) >>= adjustArray countA i . (*)
  return countA
  where
    adjustArray a i f = A.readArray a i >>= A.writeArray a i . f
    forCorpusSegments
      :: (Monad m, Num i, Ix.Ix i, A.IArray a i)
      => a i i -> i -> (i -> m b) -> m ()
    forCorpusSegments a i f
      | Ix.inRange (A.bounds a) i
      = f i >> forCorpusSegments a (i + 1 + a A.! i) f
      | otherwise
      = return ()


corpusToArrays
  :: [([Int], [Int])]
  -> ( A.UArray Int Int
     , ( IM.IntMap (IM.IntMap Int)
       , ( A.UArray Int Int
         , A.UArray Int Int
     ) ) )
corpusToArrays corpus = (\ res@(cA, (m, _)) -> cA `seq` m `seq` res) $
  let size = sum
           $ map (\ (es, fs) -> let lf = length fs in lf + lf * length es)
                 corpus
  in second (\ x -> (snd x, inv x))
  $ first (A.listArray (0, size - 1))
  $ flip StL.runState (0, IM.empty)
  $ fmap concat $ forM corpus $ \ (es, fs) ->
      let le = length es in
      fmap concat $ forM fs $ \ f ->
        fmap (le :) $ forM es $ \ e ->
          lookupInsert e f
  where
    inv (cnt, m) = let bnds = (0, cnt - 1)
     in ( A.array bnds
        $ concatMap (uncurry $ (. IM.elems) . flip zip . repeat)
        $ IM.assocs m
        , A.array bnds $ concatMap (map swap . IM.assocs) $ IM.elems m
        )


lookupInsert :: Int -> Int -> StL.State (Int, IM.IntMap (IM.IntMap Int)) Int
lookupInsert k1 k2 = do
  (cnt, m) <- StL.get
  let m' = IM.findWithDefault IM.empty k1 m
  case IM.lookup k2 m' of
    Nothing -> let cnt' = cnt + 1
            in cnt' `seq` StL.put (cnt', IM.insert k1 (IM.insert k2 cnt m') m)
            >> return cnt
    Just i  -> return i


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
mainTrain swapLangs delta corpus
  = parseCorpus corpus
  >>= putStrColumns [" | "]
    . (\ (x, y, z) -> ["e" : x, "f" : y, "p(f|e)" : z])
    . unzip3
    . fmap (\ ((e, f), w) -> (e, f, show w))
    . filter ((<) 0.1 . snd)
    . last
    . train delta
    . (if swapLangs then map (\ (a, b) -> (b, a)) else id)


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
