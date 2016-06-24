{-# LANGUAGE BangPatterns, RecordWildCards #-}
-- for convenient cmdargs definitions:
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CBSM.Main
-- Copyright   :  (c) Technische Universität Dresden 2014
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.CBSM.Main
( main
, mainArgs
, cmdArgs
, Args()
) where


import           Data.List.Extra (groupWithRanges, isSingleton, toRanges)
import           System.Console.CmdArgs.Explicit.Misc
import           Vanda.Algorithms.EarleyMonadic
import qualified Vanda.Algorithms.Earley.WSA as WSA
import           Vanda.CBSM.CountBasedStateMerging
import           Vanda.Corpus.Binarization.CmdArgs
import           Vanda.Corpus.Penn.Filter
import           Vanda.Corpus.Penn.Text (treeToPenn)
import           Vanda.Corpus.SExpression as SExp
import qualified Vanda.Features as F
import qualified Vanda.Hypergraph as H
import           Vanda.Util.IO
import           Vanda.Util.Timestamps
import           Vanda.Util.Tree as T

import           Control.Arrow (second)
import           Control.Monad
import qualified Data.Binary as B
import           Data.List (intercalate, nub)
import           Data.Map ((!))
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Ord
import qualified Data.Set as S
import           Data.Tree
import qualified Data.Vector as V
import           Numeric.Log (Log(..))
import           System.Console.CmdArgs.Explicit
import           System.CPUTime
import           System.Directory ( createDirectoryIfMissing
                                  , doesDirectoryExist
                                  , getDirectoryContents )
import           System.Exit (exitFailure)
import           System.FilePath ((</>), (<.>))
import           System.IO ( Handle
                           , IOMode(..)
                           , hFlush
                           , hPutStr
                           , hPutStrLn
                           , stdout
                           , withFile )
import           System.Posix.Files (fileExist)


data Args
  = Help String
  | PrintCorpora
    { flagAsForests :: Bool
    , flagPennFilter :: Bool
    , flagBinarization :: FlagBinarization
    , flagDefoliate :: Bool
    , flagFilterByLeafs :: FilePath
    , flagOutputFormat :: FlagOutputFormat
    , argCorpora :: [FilePath]
    }
  | CBSM
    { flagAsForests :: Bool
    , flagBinarization :: FlagBinarization
    , flagDefoliate :: Bool
    , flagPennFilter :: Bool
    , flagFilterByLeafs :: FilePath
    , flagRestrictMerge :: [FlagRestrictMerge]
    , flagBeamWidth :: Int
    , flagNormalize :: Bool
    , flagIterations :: Int
    , flagDir :: FilePath
    , argCorpora :: [FilePath]
    }
  | CBSMContinue
    { flagRestrictMerge :: [FlagRestrictMerge]
    , flagBeamWidth :: Int
    , flagNormalize :: Bool
    , flagIterations :: Int
    , flagDir :: FilePath
    }
  | ShowInfo
    { flagIntToTreeMap :: FilePath
    , argInfo :: FilePath
    }
  | Parse
    { flagUnknownWords :: FlagUnknownWords
    , flagUnknownWordOutput :: FlagUnknownWordOutput
    , flagOutputFormat :: FlagOutputFormat
    , flagMessageNoParse :: String
    , argGrammar :: FilePath
    , argCount :: Int
    }
  | Bests
    { flagOutputFormat :: FlagOutputFormat
    , argGrammar :: FilePath
    , argCount :: Int
    }
  deriving Show


data FlagOutputFormat = FOFPretty | FOFPenn | FOFYield deriving (Eq, Show)

data FlagRestrictMerge
  = FRMLeafs | FRMTerminals | FRMUnary
  deriving (Eq, Show)

data FlagUnknownWords = FUWStrict | FUWArbitrary deriving (Eq, Show)

data FlagUnknownWordOutput
       = FUWOOriginal | FUWOReplacement | FUWOBoth deriving (Eq, Show)


cmdArgs :: Mode Args
cmdArgs
  = modes "cbsm" (Help $ defaultHelp cmdArgs) "Count-Based State Merging"
  [ (modeEmpty $ PrintCorpora False False FBNone False "" FOFPretty [])
    { modeNames = ["print-corpora"]
    , modeHelp =
        "Print trees from TREEBANKs. Can be used to check for parsing \
        \errors. Every TREEBANK can be a file or a directory. Directories \
        \are traversed recursively. If no TREEBANK is given, the trees are \
        \read from standard input. \
        \The filters (if used) apply in the order penn-filter, defoliate, \
        \and filter-by-leafs."
    , modeArgs = ([], Just flagArgCorpora)
    , modeGroupFlags = toGroup
        [ flagNoneAsForests
        , flagNonePennFilter
        , flagReqBinarization (\ b x -> x{flagBinarization = b})
        , flagNoneDefoliate
        , flagReqFilterByLeafs
        , flagOutputFormat
        ]
    }
  , ( modeEmpty
        $ CBSM False FBNone False False "" [] 1000 False (pred maxBound) "" [])
    { modeNames = ["cbsm"]
    , modeHelp = "Read-off a grammar from TREEBANKs and generalize it. See \
        \print-corpora for further information about the TREEBANK arguments."
    , modeArgs = ([], Just flagArgCorpora)
    , modeGroupFlags = toGroup
        [ flagNoneAsForests
        , flagReqBinarization (\ b x -> x{flagBinarization = b})
        , flagNoneDefoliate
        , flagNonePennFilter
        , flagReqFilterByLeafs
        , flagReqRestrictMerge
        , flagReqBeamWidth
        , flagNoneNormalize
        , flagReqIterations
        , flagReqDir
        ]
    }
  , (modeEmpty $ CBSMContinue [] 1000 False (pred maxBound) "")
    { modeNames = ["cbsm-continue"]
    , modeHelp = "Continue cbsm training with a grammar."
    , modeGroupFlags = toGroup
        [ flagReqRestrictMerge
        , flagReqBeamWidth
        , flagNoneNormalize
        , flagReqIterations
        , flagReqDir
        ]
    }
  , (modeEmpty $ ShowInfo "" "")
    { modeNames = ["show-info"]
    , modeHelp = "Show collected meta information and visualize the done \
                 \merges, if available."
    , modeArgs =
        ( [ flagArgMergeTreeMap{argRequire = True}
          ]
        , Nothing
        )
    , modeGroupFlags = toGroup
        [ flagReqIntToTreeMap
        ]
    }
  , (modeEmpty $ Parse FUWStrict FUWOOriginal FOFPretty "" "" 1)
    { modeNames = ["parse"]
    , modeHelp = "Parse newline-separated sentences from standard input."
    , modeArgs =
        ( [ flagArgGrammar{argRequire = True}
          , flagArgCount
          ]
        , Nothing
        )
    , modeGroupFlags = toGroup
        [ flagReqUnknownWords
        , flagReqUnknownWordOutput
        , flagOutputFormat
        , flagReqMessageNoParse
        ]
    }
  , (modeEmpty $ Bests FOFPretty "" 1)
    { modeNames = ["bests"]
    , modeHelp = "View best trees of a grammar."
    , modeArgs =
        ( [ flagArgGrammar{argRequire = True}
          , flagArgCount
          ]
        , Nothing
        )
    , modeGroupFlags = toGroup
        [ flagOutputFormat
        ]
    }
  ]
  where
    flagNoneAsForests
      = flagNone ["as-forests"] (\ x -> x{flagAsForests = True})
          "the TREEBANKs contain forests instead of trees"
    flagNoneDefoliate
      = flagNone ["defoliate"] (\ x -> x{flagDefoliate = True})
          "remove leaves from trees in TREEBANKs"
    flagNoneNormalize
      = flagNone ["normalize"] (\ x -> x{flagNormalize = True})
          "normalize likelihood deltas by number of merged states"
    flagNonePennFilter
      = flagNone ["penn-filter"] (\ x -> x{flagPennFilter = True})
          "remove predicate argument structure annotations from TREEBANKs"
    flagReqFilterByLeafs
      = flagReq ["filter-by-leafs"] (\ a x -> Right x{flagFilterByLeafs = a})
          "FILE"
          "only use trees whose leafs occur in FILE"
    flagOutputFormat
      = flagReq [flag] update "FORMAT" ("one of " ++ optsStr)
      where
        flag = "output-format"
        err  = flag ++ " expects one of " ++ optsStr
        optsStr = intercalate ", " (map fst opts)
        opts = [("pretty", FOFPretty), ("penn", FOFPenn), ("yield", FOFYield)]
        update y x = maybe (Left err) (\ z -> Right x{flagOutputFormat = z})
                   $ lookup y opts
    flagReqRestrictMerge
      = flagReq [flag] update "RESTRICTION"
      $ "one of " ++ optsStr ++ ". The RESTRICTION leafs means that states \
        \producing leafs are not merged with states producing inner nodes. \
        \The restriction terminals means that only states producing the same \
        \terminal may be merged. The restriction unary prohibits merges of \
        \unary nodes with nodes of any other arity; this can be useful in \
        \connection with some binarizations. If this flag is used more than \
        \once, all named restrictions are applied simultaneously."
      where
        flag = "restrict-merge"
        err  = flag ++ " expects one of " ++ optsStr
        optsStr = intercalate ", " (map fst opts)
        opts = [ ("leafs"    , FRMLeafs    )
               , ("terminals", FRMTerminals)
               , ("unary"    , FRMUnary    ) ]
        update y x = maybe (Left err)
                       (\ z -> Right x{flagRestrictMerge
                                                   = z : flagRestrictMerge x})
                   $ lookup y opts
    flagReqUnknownWords
      = flagReq [flag] update "MODE"
      $ "one of " ++ optsStr ++ ". The MODE strict accepts only known \
        \words for parsing. The MODE arbitrary accepts any known word \
        \as replacment for an unknown word."
      where
        flag = "unknown-words"
        err  = flag ++ " expects one of " ++ optsStr
        optsStr = intercalate ", " (map fst opts)
        opts = [("strict", FUWStrict), ("arbitrary", FUWArbitrary)]
        update y x = maybe (Left err) (\ z -> Right x{flagUnknownWords = z})
                   $ lookup y opts
    flagReqUnknownWordOutput
      = flagReq [flag] update "MODE"
      $ "one of " ++ optsStr
      where
        flag = "unknown-word-output"
        err  = flag ++ " expects one of " ++ optsStr
        optsStr = intercalate ", " (map fst opts)
        opts = [ ("original", FUWOOriginal)
               , ("replacement", FUWOReplacement)
               , ("both", FUWOBoth) ]
        update y x = maybe (Left err)
                           (\ z -> Right x{flagUnknownWordOutput = z})
                   $ lookup y opts
    flagReqBeamWidth
      = flagReq ["beam-width"]
                (readUpdate $ \ a x -> x{flagBeamWidth = a})
                "BEAMWIDTH"
                "Larger values refine the search for the best merge \
                \candidate"
    flagReqIterations
      = flagReq ["iterations"]
                (readUpdate $ \ a x -> x{flagIterations = a})
                "ITERATIONS"
                "limit number of iterations"
    flagReqMessageNoParse
      = flagReq ["message-no-parse"]
          (\ a x -> Right x{flagMessageNoParse = a})
          "MSG"
          "print MSG if there is no parse"
    flagReqDir
      = flagReq ["dir"] (\ a x -> Right x{flagDir = a}) "DIRECTORY"
          "write output files to DIRECTORY instead of current"
    flagReqIntToTreeMap
      = flagReq ["int2tree"] (\ a x -> Right x{flagIntToTreeMap = a}) "FILE"
          "resolve Int to trees from the original corpus"
    flagArgCorpora
      = flagArg (\ a x -> Right x{argCorpora = argCorpora x ++ [a]}) "TREEBANK"
    flagArgMergeTreeMap
      = flagArg (\ a x -> Right x{argInfo = a}) "INFO-FILE"
    flagArgGrammar
      = flagArg (\ a x -> Right x{argGrammar = a}) "GRAMMAR-FILE"
    flagArgCount
      = flagArg (readUpdate $ \ a x -> x{argCount = a}) "COUNT"


fileNameGrammar          :: Int -> FilePath
fileNameIntToTreeMap     ::        FilePath
fileNameInfo             :: Int -> FilePath
fileNameLastIteration    ::        FilePath
fileNameStatistics       ::        FilePath
fileNameEvaluations      ::        FilePath
fileNameEquivBeamIndizes ::        FilePath
fileNameGrammar        i = "grammar-"              ++ show0 9 i <.> "bin"
fileNameIntToTreeMap     = "int2tree"                           <.> "bin"
fileNameInfo           i = "info-"                 ++ show0 9 i <.> "bin"
fileNameLastIteration    = "last-iteration"                     <.> "txt"
fileNameStatistics       = "statistics"                         <.> "csv"
fileNameEvaluations      = "statistics-evaluations"             <.> "csv"
fileNameEquivBeamIndizes = "statistics-equivalent-beam-indizes" <.> "csv"


show0 :: Show a => Int -> a -> String
show0 l i = replicate (l - length cs) '0' ++ cs
  where cs = show i


type BinaryCRTG = CRTG Int String
type BinaryIntToTreeMap = M.Map Int (Tree String)
type BinaryInfo = Info Int


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()

mainArgs (Help cs) = putStr cs

mainArgs PrintCorpora{..}
    = putStr
    . unlines
    . zipWith ( drawTreeFormatted flagBinarization flagOutputFormat
              . show
              ) [1 :: Int ..]
    . map (flaggedBinarization flagBinarization)
  =<< (if null flagFilterByLeafs then return
                                 else filterByLeafs flagFilterByLeafs)
  =<< readCorpora flagAsForests flagDefoliate flagPennFilter argCorpora

mainArgs CBSM{..} = do
  exist <- fileExist (flagDir </> fileNameIntToTreeMap)
  when exist $ do
    putStrLn $ "File exists: " ++ (flagDir </> fileNameIntToTreeMap)
    putStrLn   "Probably you have run cbsm in this directory before."
    putStrLn   "Did you mean cbsm-continue?"
    exitFailure
  createDirectoryIfMissing True flagDir
  (g, tM) <- fmap ( forestToGrammar
                  . map (flaggedBinarization flagBinarization) )
           $ (if null flagFilterByLeafs then return
                                        else filterByLeafs flagFilterByLeafs)
         =<< readCorpora flagAsForests flagDefoliate flagPennFilter argCorpora
  B.encodeFile (flagDir </> fileNameIntToTreeMap) (tM :: BinaryIntToTreeMap)
  withFile (flagDir </> fileNameStatistics) AppendMode $ \ hStat ->
   withFile (flagDir </> fileNameEvaluations) AppendMode $ \ hEvals ->
   withFile (flagDir </> fileNameEquivBeamIndizes) AppendMode $ \ hBeam -> do
    hPutStrLn hStat
      "CPU time,iteration,rules,states,initial states,merge pairs,beam width,\
      \beam index,candidate index,rule merges,state merges,\
      \initial-state merges,log₂ likelihood delta,likelihood delta,\
      \log₂ evaluation of merge,evaluation of merge"
    hPutStrLn hEvals "iteration,beam index low,beam index high,\
      \log₂ evaluation of merge,evaluation of merge"
    hPutStrLn hBeam "iteration,beam index low,beam index high"
    safeSaveLastGrammar flagDir hStat hEvals hBeam
      $ take (succ flagIterations)
      $ cbsm
          (mergeGroups flagRestrictMerge tM)
          (if flagNormalize then normalizeLklhdByMrgdStates else flip const)
          flagBeamWidth
          (g, initialInfo (cntState g))

mainArgs CBSMContinue{..} = do
  it   <- read <$> readFile (flagDir </> fileNameLastIteration) :: IO Int
  g    <- B.decodeFile (flagDir </> fileNameGrammar it) :: IO BinaryCRTG
  info <- B.decodeFile (flagDir </> fileNameInfo    it) :: IO BinaryInfo
  groups <- mergeGroups flagRestrictMerge
    <$> (B.decodeFile (flagDir </> fileNameIntToTreeMap)
           :: IO BinaryIntToTreeMap)
  withFile (flagDir </> fileNameStatistics) AppendMode $ \ hStat ->
   withFile (flagDir </> fileNameEvaluations) AppendMode $ \ hEvals ->
   withFile (flagDir </> fileNameEquivBeamIndizes) AppendMode $ \ hBeam ->
    safeSaveLastGrammar flagDir hStat hEvals hBeam
      $ take (succ flagIterations)
      $ cbsm
          groups
          (if flagNormalize then normalizeLklhdByMrgdStates else flip const)
          flagBeamWidth
          (g, info)

mainArgs ShowInfo{..} = do
  info <- B.decodeFile argInfo :: IO BinaryInfo
  putStr "iteration           : " >> print (infoIteration       info)
  putStr "merge pairs         : " >> print (infoMergePairs      info)
  putStr "beam width          : " >> print (infoBeamWidth       info)
  putStr "beam index          : " >> print (infoBeamIndex       info)
  putStr "candidate index     : " >> print (infoCandidateIndex  info)
  putStr "rule merges         : " >> print (infoMergedRules     info)
  putStr "state merges        : " >> print (infoMergedStates    info)
  putStr "initial-state merges: " >> print (infoMergedInitials  info)
  putStrLn $ let l = infoLikelihoodDelta info
      in "likelihood delta    : 2^" ++ show (ln l / log 2) ++ " = " ++ show l
  putStrLn $ let l = infoEvaluation info
      in "evaluation of merge : 2^" ++ show (ln l / log 2) ++ " = " ++ show l
  putStrLn ""
  putStrLn ""
  putStrLn "merge history:"
  putStrLn ""
  m <- if null flagIntToTreeMap
       then return
          $ M.map (fmap $ \ x -> Node (show x) [])
          $ infoMergeTreeMap info
       else do tM <- B.decodeFile flagIntToTreeMap :: IO BinaryIntToTreeMap
               return
                $ M.map (fmap (tM !))
                $ infoMergeTreeMap info
  let mergeTree2Tree (State t c ) = Node (colorTTY [96] ("count: " ++ show c))
                                         [mapLeafs (colorTTY [93]) t]
      mergeTree2Tree (Merge i ms) = Node (colorTTY [7, 96] $ show i)
                                  $ map mergeTree2Tree ms
  forM_ (M.toAscList m) $ \ (i, t) -> do
    putStrLn $ show i ++ ":"
    putStrLn $ drawTree' (drawstyleCompact2 1 "") $ mergeTree2Tree t

mainArgs Parse{..} = do
  (hg, inis) <- toHypergraph <$> (B.decodeFile argGrammar :: IO BinaryCRTG)
  let comp e | a == 0    = [Right (H.label e)]
              | otherwise = map Left [0 .. a - 1]
        where a = H.arity e
  let feature = F.Feature (\ _ (i, _) xs -> i * product xs) V.singleton
  sents <- map words . lines <$> getContents
  forM_ sents $ \ sent -> do
    let wsa = createWSA flagUnknownWords hg sent
    let (hg', _) = earley' (asBackwardStar hg) comp wsa (M.keys inis)
    let inis' = M.mapKeys (\ k -> (0, k, length sent)) inis
    printWeightedTrees flagOutputFormat flagMessageNoParse
      $ take argCount
      $ map (second $ unknownWordOutput flagUnknownWordOutput sent
                    . fmap H.label)
      $ bestsIni hg' feature (V.singleton 1) inis'
    hFlush stdout

mainArgs Bests{..} = do
  (hg, inis) <- toHypergraph <$> (B.decodeFile argGrammar :: IO BinaryCRTG)
  let feature = F.Feature (\ _ i xs -> i * product xs) V.singleton
  printWeightedTrees flagOutputFormat "language empty"
    $ take argCount
      $ map (second $ fmap H.label)
    $ bestsIni (asBackwardStar hg) feature (V.singleton 1) inis


readCorpora :: Bool -> Bool -> Bool -> [FilePath] -> IO (Forest String)
readCorpora asForests doDefoliate doPennFilter corpora
    = (if doDefoliate  then map T.defoliate         else id)
    . (if doPennFilter then mapMaybe stripAll       else id)
    . (if asForests    then concatMap SExp.toForest else map SExp.toTree)
  <$> if null corpora
        then SExp.parse SExp.pSExpressions "stdin"
          <$> getContents
        else concat
          <$> (   SExp.parseFromFiles SExp.pSExpressions
              =<< getContentsRecursive corpora
              )


filterByLeafs :: FilePath -> [Tree String] -> IO [Tree String]
filterByLeafs file ts = do
  wordS <- S.fromList . words <$> readFile file
  return $ filter (all (`S.member` wordS) . yield) ts


data RestrictMergeFeature a
  = RMFLeaf Bool
  | RMFTerminal a
  | RMFUnary Bool
  deriving (Eq, Ord)


rmFeature :: Ord a => FlagRestrictMerge -> Tree a -> RestrictMergeFeature a
rmFeature FRMLeafs     = RMFLeaf . null . subForest
rmFeature FRMTerminals = RMFTerminal . rootLabel
rmFeature FRMUnary     = RMFUnary . isSingleton . subForest


mergeGroups
  :: (Ord v, Ord a) => [FlagRestrictMerge] -> M.Map v (Tree a) -> [S.Set v]
mergeGroups flags
  = map S.fromList
  . M.elems
  . M.fromListWith (++)
  . map (\ (v, t) -> (map ($ t) features, [v]))
  . M.toList
  where
    features = map rmFeature (nub flags)


createWSA
  :: (H.Hypergraph h, Ord v, Ord l, Num w)
  => FlagUnknownWords -> h v l i -> [l] -> WSA.WSA Int l w
createWSA flag hg xs
  = case flag of
      FUWStrict    -> id
      FUWArbitrary -> replaceUnknownsInWSA
  $ WSA.fromList 1 xs
  where
    knownWords
      = S.fromList $ map H.label $ filter ((0 ==) . H.arity) $ H.edges hg
    replaceUnknownsInWSA x
      = x{WSA.transitions = concatMap replaceUnknowns $ WSA.transitions x}
    replaceUnknowns t
      = if S.member (WSA.transTerminal t) knownWords
        then [t]
        else map (\ w -> t{WSA.transTerminal = w}) $ S.toList knownWords


unknownWordOutput
  :: FlagUnknownWordOutput -> [String] -> Tree String -> Tree String
unknownWordOutput FUWOOriginal s t
  = zipLeafsWith const s t
unknownWordOutput FUWOReplacement _ t = t
unknownWordOutput FUWOBoth s t
  = zipLeafsWith (\ x y -> x ++ "/" ++ y) s t


printWeightedTrees
  :: Show a
  => FlagOutputFormat -> String -> [(a, Tree String)] -> IO ()
printWeightedTrees _ msg [] = putStrLn msg
printWeightedTrees fmt _ xs =
  forM_ (zip [1 :: Int ..] xs) $ \ (i, (w, t)) ->
    putStrLn
      $ drawTreeFormatted FBNone fmt (show i ++ ": " ++ show w) t


drawTreeFormatted
  :: FlagBinarization
  -> FlagOutputFormat
  -> String
  -> Tree String
  -> String
drawTreeFormatted b FOFPretty cs t = cs ++ newline
                                  ++ unwords (binarizedYield b t) ++ newline
                                  ++ drawTreeColored b t
drawTreeFormatted _ FOFPenn   _  t = treeToPenn id t
drawTreeFormatted b FOFYield  _  t = unwords (binarizedYield b t)


drawTreeColored :: FlagBinarization -> Tree String -> String
drawTreeColored b
  = drawTree' (drawstyleCompact2 0 "")
  . mapWithSubtrees (\ x ts -> if binarizedLeafCheck b ts
                               then colorTTY [93] x
                               else x
                    )


binarizedYield :: FlagBinarization -> Tree a -> [a]
binarizedYield b = filterTree (const $ binarizedLeafCheck b)


binarizedLeafCheck :: FlagBinarization -> Forest a -> Bool
binarizedLeafCheck b = \ ts -> case b of
  FBNone -> null ts
  FBFcns -> case ts of { [Node _ [], Node _ []] -> True; _ -> False }
  FBLeftbranching  ->  case ts of { [Node _ []] -> True; _ -> False }


colorTTY :: [Int] -> String -> String
colorTTY cols str
  = "\ESC[" ++ intercalate ";" (map show cols) ++ "m" ++ str ++ "\ESC[m"


newline :: String
newline = unlines [""]


safeSaveLastGrammar
  :: FilePath
  -> Handle
  -> Handle
  -> Handle
  -> [(BinaryCRTG, Info Int)]
  -> IO ()
safeSaveLastGrammar dir hStat hEvals hBeam xs
  = handleInterrupt worker handler
  where
    worker :: ((BinaryCRTG, BinaryInfo) -> IO ()) -> IO ()
    worker update
      = forM_ xs $ \ x@(!g, Info{..}) -> do
          update x
          cpuTime <- getCPUTime
          let rules         = M.size $ cntRule  g
              states        = M.size $ cntState g
              initialStates = M.size $ cntInit  g
              showIfValid n = if n < 0 then "NaN" else show n
          hPutStrLn hStat $ intercalate ","
            [ showFixedComma 12 cpuTime  -- pico = 10^-12
            , show infoIteration
            , show rules
            , show states
            , show initialStates
            , showIfValid infoMergePairs
            , showIfValid infoBeamWidth
            , showIfValid infoBeamIndex
            , showIfValid infoCandidateIndex
            , showIfValid infoMergedRules
            , showIfValid infoMergedStates
            , showIfValid infoMergedInitials
            , show (ln infoLikelihoodDelta / log 2)
            , show infoLikelihoodDelta
            , show (ln infoEvaluation / log 2)
            , show infoEvaluation
            ]
          hPutStr hEvals
            $ unlines
            $ map (\ (lo, hi, e) -> show infoIteration ++ ","
                                 ++ show (succ lo) ++ ","
                                 ++ show (succ hi) ++ ","
                                 ++ show (ln (head e) / log 2) ++ ","
                                 ++ show (head e) )
            $ groupWithRanges infoEvaluations
          hPutStr hBeam
            $ unlines
            $ map (\ (lo, hi) -> show infoIteration ++ ","
                              ++ show lo ++ ","
                              ++ show hi)
            $ toRanges infoEquivalentBeamIndizes
          hFlush hStat
          hFlush hEvals
          hFlush hBeam
          putStrLnTimestamped
            $ "Iteration " ++ show infoIteration ++ ": "
              ++ show rules         ++ " rules, "
              ++ show states        ++ " states, "
              ++ show initialStates ++ " initial states."
          hFlush stdout

    handler :: (BinaryCRTG, BinaryInfo) -> IO ()
    handler (g, info) = do
      let i = infoIteration info
      putStrLnTimestamped $ "Writing result of iteration " ++ show i ++ " ..."
      hFlush stdout
      B.encodeFile (dir </> fileNameGrammar i) (g    :: BinaryCRTG)
      B.encodeFile (dir </> fileNameInfo    i) (info :: BinaryInfo)
      writeFile (dir </> fileNameLastIteration) (show i)
      putStrLnTimestamped
        $ "... done writing result of iteration " ++ show i ++ "."
      hFlush stdout


showFixedComma :: (Show a, Integral a) => Int -> a -> String
showFixedComma o = go
  where
    d = 10^o
    go x | r == 0    = show l
         | otherwise = show l ++ "." ++ replicate (o - length r') '0' ++ r'
      where l  = x `div` d
            r  = x - l * d
            r' = show r


bestsIni
  :: (H.Hypergraph h, Ord v, Eq l)
  => h v l i
  -> F.Feature l i x
  -> V.Vector Double
  -> M.Map v Double
  -> [(Double, H.Derivation v l i)]
bestsIni hg feat wV inis
  = mergesBy (comparing (Down . fst))
  $ M.elems
  $ M.intersectionWith (\ w' -> map (\ (F.Candidate w d _) -> (w' * w, d))) inis
  $ H.bests hg feat wV
  where

    -- | Merge sorted lists to a single sorted list.
    mergesBy :: (a -> a -> Ordering) -> [[a]] -> [a]
    mergesBy cmp = foldl (mergeBy cmp) []


    -- | Merge two sorted lists to a single sorted list.
    mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
    mergeBy cmp xs@(x:xs') ys@(y:ys')
      = case x `cmp` y of
          GT ->  y : mergeBy cmp xs  ys'
          _  ->  x : mergeBy cmp xs' ys
    mergeBy _ [] ys = ys
    mergeBy _ xs [] = xs


getContentsRecursive :: [FilePath] -> IO [FilePath]
getContentsRecursive paths
  = fmap concat
  $ forM paths $ \ path ->
      ifM (doesDirectoryExist path)
        ( getDirectoryContents path
          >>= getContentsRecursive
            . map (path </>)
            . filter (`notElem` [".", ".."])  -- TODO: this seems like a hack
        )
        (return [path])


ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM predicateM thn els = do
  b <- predicateM
  if b then thn else els
