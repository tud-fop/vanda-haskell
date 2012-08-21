{-# LANGUAGE Rank2Types #-}

module Algorithms.DecoderIBM1 where


import Control.Applicative
import Control.Arrow
import Control.Exception (bracket)
import Control.Monad
import qualified Control.Monad.Trans.State.Lazy as StL
import Data.Array
import Data.Char (isSpace)
import Data.Function (on)
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO
import Text.Printf (printf)

import qualified Algorithms.EMDictionaryIntMap as Dict
import Tools.Timestamps


data Hypothesis = Hypothesis
  { transLen :: Int
  , transRev :: [Int]
  , nGramW :: Double
  , transWs :: [Double]
  }


data Model = Model
  { pLen   :: Int -> Int -> Double
  , pTrans :: Int -> Int -> Double
  , pWordE :: Int -> Double
  , pWordF :: Int -> Double
  , pNGram :: [Int] -> Double
  , countE :: Int
  , countF :: Int
  }


pWordFArray :: Model -> [Int] -> IM.IntMap Double
pWordFArray m fs
  = IM.fromList
    [ ( f
      , sum [ pWordE m e * pTrans m e f
            | e <- [0 .. countE m - 1]
            ]
      )
    | f <- fs
    ]


expand :: Model -> Int -> [Int] -> Hypothesis -> Int -> (Hypothesis, Double)
expand m fLen fs (Hypothesis eLen es w ws) e
  = ( Hypothesis eLen' es' w' ws'
    , pLen m eLen' fLen + sum (map log ws') + w'
    )
  where
    eLen' = eLen + 1
    es'   = e : es
    w'    = w * pNGram m undefined
    ws'   = zipWith (\ f w'' -> w'' * pTrans m e f) fs ws


-- ---------------------------------------------------------------------------

data Datafiles = Datafiles
  { dfBasename :: String
  , dfLangIdE  :: String
  , dfLangIdF  :: String
  }


dfDictionary, dfLengthmodel,
  dfBasenameE, dfCorpusE, dfCorpusIntE, dfLexiconE, dfNGramsE, dfUnigramsE,
  dfBasenameF, dfCorpusF, dfCorpusIntF, dfLexiconF, dfNGramsF
  :: Datafiles -> String
dfBasenameE   df = dfBasename  df ++ "." ++ dfLangIdE df
dfBasenameF   df = dfBasename  df ++ "." ++ dfLangIdF df
dfCorpusE     df = dfBasenameE df ++ ".txt"
dfCorpusF     df = dfBasenameF df ++ ".txt"
dfCorpusIntE  df = dfBasenameE df ++ ".int.txt"
dfCorpusIntF  df = dfBasenameF df ++ ".int.txt"
dfLexiconE    df = dfBasenameE df ++ ".lexicon.txt"
dfLexiconF    df = dfBasenameF df ++ ".lexicon.txt"
dfNGramsE     df = dfBasenameE df ++ ".ngrams.txt"
dfNGramsF     df = dfBasenameF df ++ ".ngrams.txt"
dfUnigramsE   df = dfBasenameE df ++ ".unigrams.txt"
dfDictionary  df = dfBasename  df ++ ".dictionary.txt"
dfLengthmodel df = dfBasename  df ++ ".lengthmodel.txt"


main :: IO ()
main = do
  args <- getArgs
  case args of
    basename : idE : idF : args' -> let df = Datafiles basename idE idF in
      case args' of
        ["intify"] -> mainIntify df
        ["unintify"] -> mainUnintify df
        ["trainDictionary" ] -> mainTrainDictionary  df
        ["trainLengthmodel"] -> mainTrainLengthmodel df
        ["trainNGrams"     ] -> mainTrainNGrams      df
        ["trainUnigrams"   ] -> mainTrainUnigrams    df
        _ -> error "Sorry, I do not understand."
    _ -> error "Sorry, I do not understand."


mainIntify :: Datafiles -> IO ()
mainIntify df = do
  (pairs, (lexiconE, lexiconF))
    <- readParallelCorpusIntifying (dfCorpusE df) (dfCorpusF df)
  bracket (openFile (dfCorpusIntE df) WriteMode) hClose $ \ hE ->
    bracket (openFile (dfCorpusIntF df) WriteMode) hClose $ \ hF ->
      forM_ pairs $ \ (es, fs) -> do
        hPutStrLn hE $ unwords $ map show es
        hPutStrLn hF $ unwords $ map show fs
  writeLexicon (dfLexiconE df) (invert lexiconE)
  writeLexicon (dfLexiconF df) (invert lexiconF)


mainUnintify :: Datafiles -> IO ()
mainUnintify df = do
  lexiconE <- readLexicon (dfLexiconE df)
  lexiconF <- readLexicon (dfLexiconF df)
  whenM (doesFileExist (dfUnigramsE df))
    $ printTimestamp >> putStrLn ("Converting " ++ (dfUnigramsE df) ++ " ...")
    >>  readMap (read :: String -> Int) (read :: String -> Int)
                (dfUnigramsE df)
    >>= writeMap id (dfUnigramsE df ++ ".noint.txt")
      . M.mapKeys (lexiconE ?)
  whenM (doesFileExist (dfNGramsE df))
    $ printTimestamp >> putStrLn ("Converting " ++ (dfNGramsE df) ++ " ...")
    >>  readNestedMaps (read :: String -> [Int]) (read :: String -> Int)
                       (read :: String -> Int) (dfNGramsE df)
    >>= writeNestedMaps unwords id (dfNGramsE df ++ ".noint.txt")
      . M.mapKeys (map (lexiconE ?))
      . M.map (M.mapKeys (lexiconE ?))
  whenM (doesFileExist (dfDictionary df))
    $ printTimestamp >> putStrLn ("Converting " ++ (dfDictionary df) ++ " ...")
    >>  readNestedMapsList (read :: String -> Int) (read :: String -> Int)
                           (read :: String -> Double) (dfDictionary df)
    >>= writeNestedMapsList id id (dfDictionary df ++ ".noint.txt")
      . map (first (lexiconE ?))
      . map (second $ map $ first (lexiconF ?))
  printTimestamp >> putStrLn "... done"
  where
    whenM m1 m2 = m1 >>= flip when m2
    a ? i | i == -1              = "<BOUNDARY>"
          | inRange (bounds a) i = a ! i
          | otherwise            = "<UNKNOWN>"


mainTrainDictionary :: Datafiles -> IO ()
mainTrainDictionary df = do
  pairs <- readParallelCorpus read (dfCorpusIntE df) (dfCorpusIntF df)
  forM_ (zip [1 :: Int ..] $ Dict.train'' 0 pairs) $ \ (i, dict) -> do
    let file = dfDictionary df ++ "." ++ printf "%04d" i
    printTimestamp
    putStrLn $ "Writing " ++ file ++ " ..."
    hFlush stdout
    writeNestedMaps show show file $ fmap intMapToMap $ intMapToMap dict
    printTimestamp
    putStrLn "... done."


mainTrainNGrams :: Datafiles -> IO ()
mainTrainNGrams df
  = readCorpus (read :: String -> Int) (dfCorpusIntE df)
  >>= writeNestedMaps show show (dfNGramsE df)
    . trainNGrams 2 (-1)


trainNGrams :: Ord a => Int -> a -> [[a]] -> M.Map [a] (M.Map a Int)
trainNGrams n boundary
  = conditionalCount
  . concatMap
      ( map (\ xs -> (init xs, last xs))
      . windowSplit n
      . (replicate (n - 1) boundary ++)
      . (++ [boundary])
      )
  where
    windowSplit :: Int -> [a] -> [[a]]
    windowSplit m xs
      | length xs < m = []
      | otherwise     = take m xs : windowSplit m (tail xs)


mainTrainUnigrams :: Datafiles -> IO ()
mainTrainUnigrams df
  = readCorpus (read :: String -> Int) (dfCorpusIntE df)
  >>= writeMap show (dfUnigramsE df)
    . L.foldl' (\ m k -> M.insertWith' (+) k (1 :: Int) m) M.empty
    . concat


mainTrainLengthmodel :: Datafiles -> IO ()
mainTrainLengthmodel df
  = readParallelCorpus (read :: String -> Int)
      (dfCorpusIntE df) (dfCorpusIntF df)
  >>= writeNestedMaps show show (dfLengthmodel df)
    . conditionalCount
    . map (\ (es, fs) -> (length es, length fs))


-- | For a return value @r@, the value @(r 'M.!' c) 'M.!' e@ is the count of
-- the pair @(c, e)@ in the input.
conditionalCount :: (Ord c, Ord e) => [(c, e)] -> M.Map c (M.Map e Int)
conditionalCount
  = keypairmap2NestedMaps
  . L.foldl' (\ m k -> M.insertWith' (+) k 1 m) M.empty


-- | Let @xs@ be a list of pairs and
-- @r = 'postprocessConditionalCounts' ('conditionalCount' xs)@. The value
-- @('fst' ('fst' r 'M.!' c)) 'M.!' e@ is the count of the pair @(c, e)@ in
-- @xs@, @'snd' ('fst' r 'M.!' c)@ is the count of pairs with @c@ as their
-- first component, and @'snd' r@ is the number of distinct second components
-- of all pairs in @xs@.
postprocessConditionalCounts
  :: (Ord c, Ord e)
  => M.Map c (M.Map e Int) -> (M.Map c (M.Map e Int, Int), Int)
postprocessConditionalCounts
  = (\ m -> (m, S.size $ M.foldr' (S.union . M.keysSet . fst) S.empty m))
  . M.map (\ m -> (m, sum $ M.elems m))


keypairmap2NestedMaps
  :: (Eq k1, Eq k2) => M.Map (k1, k2) v -> M.Map k1 (M.Map k2 v)
keypairmap2NestedMaps
  = M.fromAscList
  . map (\ xs ->
          ( fst $ fst $ head xs
          , M.fromAscList $ map (\ ((_, k), v) -> (k, v)) xs
        ) )
  . L.groupBy ((==) `on` fst . fst)
  . M.toAscList


mapToIntMap :: M.Map IM.Key a -> IM.IntMap a
mapToIntMap = IM.fromAscList . M.toAscList


intMapToMap :: IM.IntMap a -> M.Map IM.Key a
intMapToMap = M.fromAscList . IM.toAscList


readCorpus :: (String -> a) -> FilePath -> IO [[a]]
readCorpus f corpus = map (map f . words) <$> lines <$> readFile corpus


readParallelCorpus :: (String -> a) -> FilePath -> FilePath -> IO [([a], [a])]
readParallelCorpus f corpusE corpusF = do
  es <- readCorpus f corpusE
  fs <- readCorpus f corpusF
  return $ zip es fs


readCorpusIntifying :: FilePath -> IO ([[Int]], M.Map String Int)
readCorpusIntifying corpus = intify (mapM . mapM) <$> readCorpus id corpus


readParallelCorpusIntifying
  :: FilePath
  -> FilePath
  -> IO ([([Int], [Int])], (M.Map String Int, M.Map String Int))
readParallelCorpusIntifying corpusE corpusF = do
  (es, dictE) <- readCorpusIntifying corpusE
  (fs, dictF) <- readCorpusIntifying corpusF
  return (zip es fs, (dictE, dictF))


readLexicon :: FilePath -> IO (Array Int String)
readLexicon file = do
  (bnds : ls) <- lines <$> readFile file
  return
    $ array (read bnds)
    $ map (first read . splitAtSpaces) ls


writeLexicon :: FilePath -> Array Int String -> IO ()
writeLexicon file lexiconA
  = writeFile file
  $ unlines
  $ show (bounds lexiconA)
  : map (\ (i, cs) -> show i ++ "\t" ++ cs) (assocs lexiconA)


readMap
  :: (Eq k, Read a)
  => (String -> k) -> (String -> a) -> FilePath -> IO (M.Map k a)
readMap readKey readValue file
  = M.fromAscList
  . map (first readKey . second readValue . splitAtSpaces)
  . lines
  <$> readFile file


writeMap :: Show a => (k -> String) -> FilePath -> M.Map k a -> IO ()
writeMap showKey file
  = writeFile file
  . unlines
  . map (\ (k, v) -> showKey k ++ "\t" ++ show v)
  . M.toAscList


readNestedMaps
  :: (Eq k1, Eq k2, Read a)
  => (String -> k1)
  -> (String -> k2)
  -> (String -> a)
  -> FilePath
  -> IO (M.Map k1 (M.Map k2 a))
readNestedMaps readKey1 readKey2 readValue file
  = M.fromAscList
  . map (second M.fromAscList)
  <$> readNestedMapsList readKey1 readKey2 readValue file


readNestedMapsList
  :: (Eq k1, Eq k2, Read a)
  => (String -> k1)
  -> (String -> k2)
  -> (String -> a)
  -> FilePath
  -> IO [(k1, [(k2, a)])]
readNestedMapsList readKey1 readKey2 readValue file
  = map (\ (cs : css) ->
          ( readKey1 cs
          , map ( first readKey2
                . second readValue
                . splitAtSpaces
                . dropWhile isSpace
                ) css
        ) )
  . tail
  . myGroup (isSpace . head)
  . lines
  <$> readFile file
  where
    myGroup f (x : xs) | f x       =      (x : ys) : yss
                       | otherwise = [] : (x : ys) : yss
      where (ys : yss) = myGroup f xs
    myGroup _ _ = [[]]


writeNestedMaps
  :: Show a
  => (k1 -> String)
  -> (k2 -> String)
  -> FilePath
  -> M.Map k1 (M.Map k2 a)
  -> IO ()
writeNestedMaps showKey1 showKey2 file
  = writeNestedMapsList showKey1 showKey2 file
  . map (second M.toAscList)
  . M.toAscList


writeNestedMapsList
  :: Show a
  => (k1 -> String)
  -> (k2 -> String)
  -> FilePath
  -> [(k1, [(k2, a)])]
  -> IO ()
writeNestedMapsList showKey1 showKey2 file xs
  = bracket (openFile file WriteMode) hClose $ \ h ->
      forM_ xs $ \ (k1, ys) -> do
        hPutStrLn h $ showKey1 k1
        forM_ ys $ \ (k2, v) ->
          hPutStrLn h $ "\t" ++ showKey2 k2 ++ "\t" ++ show v


firstM  :: Monad m => (b -> m c) -> (b, d) -> m (c, d)
secondM :: Monad m => (b -> m c) -> (d, b) -> m (d, c)
firstM  f (x, y) = (flip (,) y) `liftM` f x
secondM f (x, y) = (     (,) x) `liftM` f y


invert :: M.Map k Int -> Array Int k
invert m = array (0, M.size m - 1) [(v, k) | (k, v) <- M.assocs m]


intify
  :: Ord k
  => (forall m. Monad m => (k -> m Int) -> a -> m b)
  -> a
  -> (b, M.Map k Int)
intify myMapM = flip StL.runState M.empty . myMapM lookupInsert


lookupInsert :: (Ord k) => k -> StL.State (M.Map k Int) Int
lookupInsert k = do
  m <- StL.get
  case M.lookup k m of
    Nothing -> let n = M.size m in StL.put (M.insert k n m) >> return n
    Just i  -> return i


splitAtSpaces :: String -> (String, String)
splitAtSpaces = second (dropWhile isSpace) . break isSpace
