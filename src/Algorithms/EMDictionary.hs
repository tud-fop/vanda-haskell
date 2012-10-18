{-# LANGUAGE FlexibleContexts, Rank2Types #-}

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
module Algorithms.EMDictionary
( train
, trainInt
, trainIntAll
, Id(..)
, main
, corpusToInts
) where

import Tools.PrettyPrint (putStrColumns)

import Control.Arrow
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.ST (ST)
import qualified Control.Monad.Trans.State.Strict as StS
import qualified Data.Array.Base as AB
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


train
  :: (Ord e, Ord f) => Double -> [([e], [f])] -> [M.Map e (M.Map f Double)]
train delta corpus
  = map (unIntMap eA . IM.map (unIntMap fA)) $ trainInt delta corpus'
  where
    (corpus', (eA, fA)) = corpusToInts corpus
    unIntMap a = M.fromListWith err . map (first (a A.!)) . IM.toList
    err _ = error
           "Algorithms.EMDictionary.train.unIntMap: key mapping not injective"


assocs :: M.Map k1 (M.Map k2 a) -> [((k1, k2), a)]
assocs
  = concatMap (uncurry $ \ e -> map (\ (f, w) -> ((e, f), w)) . M.assocs)
  . M.assocs


trainInt
  :: Double
  -> [([Int], [Int])]
  -> [IM.IntMap (IM.IntMap Double)]
trainInt delta corpus
  = map (toIntMap mapE mapF) $ takeWhile' (\ a1 a2 -> d a1 a2 >= delta) arrays
  where
    (arrays, (mapE, mapF)) = trainIntArrays corpus
    d a1 a2 = maximum $ zipWith ((abs .) . (-)) (A.elems a1) (A.elems a2)
    takeWhile' f (x0 : xs@(x1 : _))
      | f x0 x1 = x0 : takeWhile' f xs
      | otherwise = x0 : x1 : []
    takeWhile' _ xs = xs


data Id a = Id { unId :: a } deriving (Bounded, Eq, Ord, Read, Show)


trainIntAll :: [([Int], [Int])] -> [Id (IM.IntMap (IM.IntMap Double))]
trainIntAll c
  = map (\ a -> a `seq` Id (toIntMap mapE mapF a)) arrays
  where
    (arrays, (mapE, mapF)) = trainIntArrays c


toIntMap
  :: A.UArray Int Int              -- ^ index → e
  -> A.UArray Int Int              -- ^ index → f
  -> A.UArray Int Double           -- ^ index → probability
  -> IM.IntMap (IM.IntMap Double)  -- ^ e → f → probability
toIntMap mapE mapF
  = IM.map (IM.fromListWith (error "toIntMap: double entry"))
  . IM.fromListWith (++)
  . map (\ (i, p) -> (mapE A.! i, [(mapF A.! i, p)]))
  . filter ((0 /=) . snd)
  . A.assocs


trainIntArrays
  :: [([Int], [Int])]  -- ^ corpus
  -> ([A.UArray Int Double], (A.UArray Int Int, A.UArray Int Int))
                       -- ^ (index → probability, (index → e, index → f))
trainIntArrays c
  = ( iterate (step corpusA mapE)
    $ A.accumArray undefined 1 (A.bounds mapE) []
    , (mapE, mapF)
    )
  where
    (corpusA, (mapE, mapF)) = corpusToArrays c


step
  :: A.UArray Int Int
  -> A.UArray Int Int
  -> A.UArray Int Double
  -> A.UArray Int Double
step corpusA partitionsA probA = A.runSTUArray $ do
  let countAMaxAt = AB.numElements probA - 1
  countA <- A.newArray (0, countAMaxAt) 0
  forCorpusSegments corpusA 0 $ \ i ->
    let norm = 1 / L.foldl' (\ s j -> s + probA ! (corpusA ! j))
                            0
                            [i + 1 .. i + corpusA ! i]
    in forM_ [i + 1 .. i + corpusA ! i] $ \ j ->
         let k = corpusA ! j
         in adjustArray countA k (norm * probA ! k +)
  let normAMaxAt = maximum $ A.elems partitionsA
  normA <- A.newArray (0, normAMaxAt) 0
        :: forall s. ST s (A.STUArray s Int Double)
  forM_ [0 .. countAMaxAt] $ \ i ->  -- sum partitions
    AB.unsafeRead countA i >>= adjustArray normA (partitionsA ! i) . (+)
  forM_ [0 .. normAMaxAt ] $ \ i ->  -- invert
    adjustArray normA i (1 /)
  forM_ [0 .. countAMaxAt] $ \ i ->  -- normalize counts
    AB.unsafeRead normA (partitionsA ! i) >>= adjustArray countA i . (*)
  return countA
  where
    (!) :: (A.IArray a e, Ix.Ix i) => a i e -> Int -> e
    (!) = AB.unsafeAt
    adjustArray a i f = AB.unsafeRead a i >>= AB.unsafeWrite a i . f
    forCorpusSegments
      :: (A.IArray a Int, Monad m)
      => a Int Int -> Int -> (Int -> m b) -> m ()
    forCorpusSegments a i f
      | i < AB.numElements a
      = f i >> forCorpusSegments a (i + 1 + a ! i) f
      | otherwise
      = return ()


corpusToArrays
  :: [([Int], [Int])]  -- ^ corpus
  -> (A.UArray Int Int, (A.UArray Int Int, A.UArray Int Int))
                       -- ^ (Int → (count or index), (index → e, index → f))
corpusToArrays corpus = (\ res@(cA, (m, _)) -> cA `seq` m `seq` res) $
  let size = sum
           $ map (\ (es, fs) -> let lf = length fs in lf + lf * length es)
                 corpus
  in second inv
  $ first (A.listArray (0, size - 1))
  $ flip StS.runState (0, IM.empty)
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


lookupInsert :: Int -> Int -> StS.State (Int, IM.IntMap (IM.IntMap Int)) Int
lookupInsert k1 k2 = do
  (cnt, m) <- StS.get
  let m' = IM.findWithDefault IM.empty k1 m
  case IM.lookup k2 m' of
    Nothing -> let cnt' = cnt + 1
            in cnt' `seq` StS.put (cnt', IM.insert k1 (IM.insert k2 cnt m') m)
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
    . assocs
    . last
    . train delta
    . (if swapLangs then map (\ (a, b) -> (b, a)) else id)


mainSteps :: Double -> String -> IO ()
mainSteps delta corpus = do
  xs <- fmap (train delta) $ parseCorpus corpus
  let fullMap = M.map (M.map (const 0)) $ head xs
  putStr
    . unlines
    . map (L.intercalate "\t")
    . (\ (x, y, z) -> [x, y, z])
    . unzip3
    . fmap (\ ((e, f), w) -> (e, f, {-replaceComma $-} show w))
    . assocs
    $ head xs
  putStr
    . unlines
    . fmap
      ( L.intercalate "\t"
      . fmap ({-replaceComma .-} show . snd)
      . assocs
      . M.unionWith (M.unionWith (+)) fullMap
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
