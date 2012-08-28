{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}

module Algorithms.DecoderIBM1 where


import Prelude hiding (catch)

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Exception (bracket, catch, SomeException)
import Control.Monad
import Control.Concurrent
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
import System.Posix.Signals

import qualified Algorithms.EMDictionaryIntMap as Dict
import Tools.Timestamps


data Hypothesis = Hypothesis
  { hLen        :: Int
  , hTrans      :: [Int]
  , hPNGrams    :: Double
  , hP          :: Double
  , hCandidates :: IM.IntMap Int
  } deriving Show


data Model = Model
  { mMTrans :: M.Map Int (M.Map Int Double)
  , mPTrans :: Int -> Int -> Double
  , mPLen   :: Int -> Int -> Double
  , mPNGram :: [Int] -> Int -> Double
  , mPWordE :: Int -> Double
  , mPAlign :: Int -> Int -> Int -> Int -> Double
--   , pWordF :: Int -> Double
  , mSizeE :: Int
--   , countF :: Int
  }


pWordFArray :: Model -> [Int] -> IM.IntMap Double
pWordFArray m fs
  = IM.fromList
    [ ( f
      , sum [ mPWordE m e * mPTrans m e f
            | e <- [0 .. mSizeE m - 1]
            ]
      )
    | f <- fs
    ]


expandH
  :: Model
  -> (Int -> Double)
  -> Int
  -> [Int]
  -> Hypothesis
  -> Int
  -> Hypothesis
expandH m pWordF fLen fs h e
  = updateHP m pWordF fLen fs
  $ Hypothesis
      (hLen h + 1)
      (hTrans h ++ [e])
      (hPNGrams h * mPNGram m [last ((-1) : hTrans h)] e)
      undefined
      (IM.update updt e $ hCandidates h)
  where
    updt i | i <= 1    = Nothing
           | otherwise = Just (i - 1)


updateHP
  :: Model -> (Int -> Double) -> Int -> [Int] -> Hypothesis -> Hypothesis
updateHP m pWordF fLen fs h
  = h { hP
      = hPNGrams h * sum
        [ mPLen m fLen l * product
            [ sum [ mPAlign m l fLen i j * mPTrans m e f
                  | (e, i) <- zip (hTrans h) [1 .. hLen h]
                  ]
            + sum [ mPAlign m l fLen i j * pWordF f
                  | i <- [hLen h + 1 .. l]
                  ]
            | (f, j) <- zip fs [1 .. fLen]
            ]
        | l <- [hLen h .. 2 * fLen]
        ]
      }


pAddOneSmoothedNestedMaps
  :: (Ord k1, Ord k2, Show k1)
  => M.Map k1 (M.Map k2 Int) -> Double -> k1 -> k2 -> Double
pAddOneSmoothedNestedMaps m0
  = \ add e f ->  -- lambda to allow memoization of where clause definitions
    case M.lookup e m1 of
      Just (m2, cntSum)
        -> maybe add ((add +) . fromIntegral) (M.lookup f m2)
         / fromIntegral (cntSum + cntE)
      Nothing
        -> error $ "Unknown condition: " ++ show e
  where
    (m1, cntE) = postprocessConditionalCounts m0


-- TODO: Check for underflow
decode m fs = (heaps)
  where
    h0 = updateHP m pWordF fLen fs
       $ Hypothesis 0 [] 1 undefined
       $ IM.fromListWith (+) $ concatMap (map (\ x -> (x, 1)) . candidateE) fs
    fLen = length fs
    pWordF = (pWordFArray m fs IM.!)
    mTransSwapped = swapNestedMaps $ mMTrans m
    candidateE f = map fst
                 $ take 20
                 $ L.sortBy (compare `on` FlipOrd . snd)
                 $ M.assocs
                 $ M.findWithDefault M.empty f mTransSwapped
    for :: a -> [b] -> (a -> b -> a) -> a
    for z xs f = L.foldl' f z xs
    heaps = take (2 * fLen) $ iterate step $ phSingleton 20 (hP h0) h0
    step ph0 = for (phEmpty 20) (phElems ph0) $ \ ph1 h ->
                 for ph1 (IM.keys $ hCandidates h) $ \ ph2 e ->
                   let h' = expandH m pWordF fLen fs h e
                   in phInsert (hP h') h' ph2


newtype FlipOrd a = FlipOrd { unflipOrd :: a } deriving (Eq)

instance (Ord a) => Ord (FlipOrd a) where
  compare (FlipOrd x) (FlipOrd y) = compare y x

-- ---------------------------------------------------------------------------

data PruningHeap k a = PruningHeap Int (M.Map k [a]) deriving Show


phEmpty :: Int -> PruningHeap k a
phEmpty n = PruningHeap n M.empty


phSingleton :: Int -> k -> a -> PruningHeap k a
phSingleton n k x = PruningHeap n (M.singleton k [x])


phInsert :: Ord k => k -> a -> PruningHeap k a -> PruningHeap k a
phInsert k x (PruningHeap n m)
  = let m' = M.insertWith (++) k [x] m
  in PruningHeap n $ if M.size m' > n then M.deleteMin m' else m'


phElems :: PruningHeap k a -> [a]
phElems (PruningHeap _ m) = concat $ M.elems m

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
        ["decode"          ] -> mainDecode df
        ["intify"          ] -> mainIntify df
        ["unintify"        ] -> mainUnintify df
        ["trainDictionary" ] -> mainTrainDictionary  df
        ["trainLengthmodel"] -> mainTrainLengthmodel df
        ["trainNGrams"     ] -> mainTrainNGrams      df
        ["trainUnigrams"   ] -> mainTrainUnigrams    df
        _ -> error "Sorry, I do not understand."
    _ -> error "Sorry, I do not understand."


mainDecode :: Datafiles -> IO ()
mainDecode df = do
  lexiconE     <- load (dfLexiconE df) readLexicon
  lexiconFInv  <- invertA2M <$> load (dfLexiconF df) readLexicon
  dictionary   <- load (dfDictionary df)
                $ readNestedMaps (read :: String ->  Int    )
                                 (read :: String ->  Int    )
                                 (read :: String ->  Double )
  lengthmodel  <- load (dfLengthmodel df)
                $ readNestedMaps (read :: String ->  Int    )
                                 (read :: String ->  Int    )
                                 (read :: String ->  Int    )
  nGrammodel   <- load (dfNGramsE df)
                $ readNestedMaps (read :: String -> [Int   ])
                                 (read :: String ->  Int    )
                                 (read :: String ->  Int    )
  unigrammodel <- load (dfUnigramsE df)
                $ readMap        (read :: String ->  Int    )
                                 (read :: String ->  Int    )
  let cntE = sum $ M.elems unigrammodel
      model = Model
                dictionary
                (\ e f -> M.findWithDefault 0 f
                        $ M.findWithDefault M.empty e dictionary)
                (pAddOneSmoothedNestedMaps lengthmodel 1)
                (pAddOneSmoothedNestedMaps nGrammodel 1)
                ((/ fromIntegral cntE) . fromIntegral . (unigrammodel M.!))
                (\ l m _{-i-} _{-j-} -> 1 / fromIntegral (l ^ m))
                (M.size unigrammodel)
      unlexi cs = maybe (error $ "Unknown word: " ++ cs)
                        id (M.lookup cs lexiconFInv)
  sentences <- lines <$> getContents
  forM_ sentences $ \ sentence -> do
--     putStrLn $ unwords $ map (lexiconE !)
--       $ decode model sentence
--     print $ pWordFArray model sentence
    putStrLn sentence
    putStrLn ""
    forM_ (decode model $ map unlexi $ words sentence) $ \ ph -> do
      forM_ (phElems ph) $ \ h -> do
        putStr $ show $ hP h
        putStr $ ": "
        putStrLn $ unwords $ map (lexiconE !) (hTrans h)
      putStrLn ""
  where
    load file m = do printTimestamp
                     putStrLn ("Loading " ++ file ++ " ...")
                     m file >>= (\ x -> x `deepseq` return x)


mainIntify :: Datafiles -> IO ()
mainIntify df = do
  (pairs, (lexiconE, lexiconF))
    <- readParallelCorpusIntifying (dfCorpusE df) (dfCorpusF df)
  bracket (openFile (dfCorpusIntE df) WriteMode) hClose $ \ hE ->
    bracket (openFile (dfCorpusIntF df) WriteMode) hClose $ \ hF ->
      forM_ pairs $ \ (es, fs) -> do
        hPutStrLn hE $ unwords $ map show es
        hPutStrLn hF $ unwords $ map show fs
  writeLexicon (dfLexiconE df) (invertM2A lexiconE)
  writeLexicon (dfLexiconF df) (invertM2A lexiconF)


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
      . map (second $ L.sortBy (compare `on` (FlipOrd . snd)))
      . map (second $ map $ first (lexiconF ?))
  printTimestamp >> putStrLn "... done"
  where
    whenM m1 m2 = m1 >>= flip when m2
    a ? i | i == -1              = "<BOUNDARY>"
          | inRange (bounds a) i = a ! i
          | otherwise            = "<UNKNOWN>"


mainTrainDictionary :: Datafiles -> IO ()
mainTrainDictionary df = do
  putStrLnTimestamped $
    "Dictionary training started. On SIGUSR1 the result of the most recently \
    \completed training step is written to " ++ dfDictionary df ++ " without \
    \interupting the training, and on SIGINT or any exception the result of \
    \the last successful training step is written to this file before the \
    \program terminates."
  varDict <- newEmptySampleVar
  hndlr <- mutexize (handler varDict)
  _ <- installHandler sigUSR1 (Catch hndlr) Nothing
  catch (worker varDict) $ \ (e :: SomeException) ->
    printTimestamp >> putStr "Worker: " >> print e >> hndlr
  where
    worker :: SampleVar (IM.IntMap (IM.IntMap Double)) -> IO ()
    worker varDict = do
      pairs <- readParallelCorpus read (dfCorpusIntE df) (dfCorpusIntF df)
      pairs `deepseq` putStrLnTimestamped $ "Loaded " ++ show (length pairs)
        ++ " sentence pairs containing "
        ++ show (length $ concatMap (uncurry (++)) pairs)
        ++ " words. The training algorithm has to consider "
        ++ show (sum $ map (\ (xs, ys) -> length xs * length ys) pairs)
        ++ " word pairs."
      forM_ (zip [1 :: Int ..] $ Dict.trainIntAll pairs) $ \ (i, dict) -> do
        dict `seq` writeSampleVar varDict dict
        printTimestamp
        putStrLn $ "Completed dictionary training step " ++ show i ++ "."
    handler :: SampleVar (IM.IntMap (IM.IntMap Double)) -> IO ()
    handler varDict = do
      isEmpty <- isEmptySampleVar varDict
      if isEmpty
      then putStrLnTimestamped "There is no completed training step."
      else do
        dict <- readSampleVar varDict
        putStrLnTimestamped $ "Writing " ++ (dfDictionary df) ++ " ..."
        writeNestedMaps show show (dfDictionary df)
          $ fmap intMapToMap
          $ intMapToMap dict
        putStrLnTimestamped "... done."
    mutexize :: IO a -> IO (IO a)
    mutexize m = do
      mutex <- newMVar ()
      return $ do
        takeMVar mutex
        ret <- m
        putMVar mutex ()
        return ret


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


invertM2A :: M.Map k Int -> Array Int k
invertM2A m = array (0, M.size m - 1) [(v, k) | (k, v) <- M.assocs m]


invertA2M :: Ord k => Array Int k -> M.Map k Int
invertA2M a = M.fromList [(v, k) | (k, v) <- assocs a]


swapNestedMaps
  :: (Ord k1, Ord k2) => M.Map k1 (M.Map k2 a) -> M.Map k2 (M.Map k1 a)
swapNestedMaps
  = flip M.foldlWithKey' M.empty $ \ m k1 ->
      flip M.foldrWithKey' m $ \ k2 x ->
        M.alter (Just . maybe (M.singleton k1 x) (M.insert k1 x)) k2

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
