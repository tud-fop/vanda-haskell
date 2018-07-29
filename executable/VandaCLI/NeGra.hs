{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

{-|
Module:      VandaCLI.NeGra
Description: membership in /Dyck languages/ and /congruence multiple Dyck languages/
Copyright:   to be discussed
License:     BSD-style
Maintainer:  Felix.Voelker@tu-dresden.de
Stability:   unknown

TODO Documentaion
-}
module VandaCLI.NeGra
( main
, mainArgs
, cmdArgs
, Args()
) where


import           Control.Monad
import           Data.List
import qualified Data.Text.Lazy.IO                    as T
import           Data.Tree
import           System.Console.CmdArgs.Explicit
import           System.Console.CmdArgs.Explicit.Misc
import qualified Vanda.Corpus.Negra                   as N
import           Vanda.Corpus.Negra.Text              as NT
import           VandaCLI.Corpus.Negra.Intervals
import qualified VandaCLI.Corpus.Negra.Util           as NU


data Args
  = Help String
  | Filter
    { byLength               :: Intervals
    , byGapDegree            :: Intervals
    , bySentenceNumber       :: Intervals
    , byHeight               :: Intervals
    , byAllowedWords         :: FilePath
    , byDisallowedWords      :: FilePath
    , byAllowedPosTags       :: FilePath
    , byDisallowedPosTags    :: FilePath
    , byAllowedInnerNodes    :: FilePath
    , byDisallowedInnerNodes :: FilePath
    }
  | Transform
    { deleteSubtreeWithWords :: FilePath
    , replaceWordsByPosTag   :: Bool
    , renumberSentences      :: Int
    }
  | Statistics
    { statIntervals :: Intervals
    , statLength    :: Bool
    , statGapDeg    :: Bool
    , statHeight    :: Bool
    }
  | Query QueryArgs
    {-{ exVoc :: Bool
    , exPos :: Bool
    , exNod :: Bool
    }-}
    deriving Show

data QueryArgs
  = HelpQ String
  | ExVoc
  | ExPos
  | ExNod
  deriving Show

queryMode :: Mode QueryArgs
queryMode
  = modes "query" (HelpQ $ defaultHelp queryMode) "extract data from the corpus"
  [ (modeEmpty ExVoc)
     { modeNames = ["extract-vocabulary"]
     , modeHelp = "outputs a newline separated sorted list of words"
     , modeGroupFlags = toGroup []
     }
    , (modeEmpty ExPos)
     { modeNames = ["extract-pos-tags"]
    , modeHelp = "outputs a newline separated sorted list of POS-tags"
    , modeGroupFlags = toGroup []
    }
    ,(modeEmpty ExNod)
    { modeNames = ["extract-inner-nodes"]
    , modeHelp = "outputs a newline separated sorted list of inner node labels"
    , modeGroupFlags = toGroup []
    }
  ]

cmdArgs :: Mode Args
cmdArgs
  = modes "negra" (Help $ defaultHelp cmdArgs) "tools for the NeGra export format"
  [ (modeEmpty $ Filter "" "" "" "" "/dev/null" "/dev/null" "/dev/null" "/dev/null" "/dev/null" "/dev/null")
    { modeNames = ["filter"]
    , modeHelp = "filters a corpus according to specified predicates"
    -- , modeArgs = Nothing
    , modeGroupFlags = toGroup [ flagArgByLength
                               , flagArgByGapDegree
                               , flagArgBySentenceNumber
                               , flagArgByHeight                               , flagArgByAllowedWords
                               , flagArgByDisallowedWords
                               , flagArgByAllowedPosTags
                               , flagArgByDisallowedPosTags
                               , flagArgByAllowedInnerNodes
                               , flagArgByDisallowedInnerNodes
                               ]
                               }
  , ( modeEmpty $ Transform "/dev/null" False 0)
    { modeNames = ["transform"]
    , modeHelp = "transforms a corpus according to the specified rules"
    , modeGroupFlags = toGroup [ flagArgDelSubTreeWWords
                               , flagArgRepWordsByPosTag
                               , flagArgReNumSentence
                               ]
                               }
  , remap2 Query (\ (Query x) -> x) queryMode
  , ( modeEmpty $ Statistics "" False False False)
    { modeHelp = "outputs corpus statistics"
    , modeNames = ["statistics"]
    , modeArgs = ([ flagArgStatIntervals{argRequire = False}], Nothing)
    , modeGroupFlags = toGroup [ flagArgStatLength
                               , flagArgStatGapDeg
                               , flagArgStatHeight
                               ]
                               }
                               ]
  where
    flagArgByLength
      = flagReq ["l", "by-length"]
                (\ a x -> Right x{byLength = a})
                "INTERVALS" ""
    flagArgByGapDegree
      = flagReq ["g", "by-gap-degree"]
                (\ a x -> Right x{byGapDegree = a})
                "INTERVALS" ""
    flagArgBySentenceNumber
      = flagReq ["n", "by-sentence-number"]
                (\ a x -> Right x{bySentenceNumber = a})
                "INTERVALS" ""
    flagArgByHeight
      = flagReq ["h", "by-height"]
                (\ a x -> Right x{byHeight = a})
                "INTERVALS" ""
    flagArgByAllowedWords
      = flagReq ["w", "by-allowed-words"]
                (\ a x -> Right x{byAllowedWords = a})
                "FILE" ""
    flagArgByDisallowedWords
      = flagReq ["by-disallowed-words"]
                (\ a x -> Right x{byDisallowedWords = a})
                "FILE" ""
    flagArgByAllowedPosTags
      = flagReq ["p", "by-allowed-pos-tags"]
                (\ a x -> Right x{byAllowedPosTags = a})
                "FILE" ""
    flagArgByDisallowedPosTags
      = flagReq ["by-disallowed-pos-tags"]
                (\ a x -> Right x{byDisallowedPosTags = a})
                "FILE"""
    flagArgByAllowedInnerNodes
      = flagReq ["i", "by-allowed-inner-nodes"]
                (\ a x -> Right x{byAllowedInnerNodes = a})
                "FILE" ""
    flagArgByDisallowedInnerNodes
      = flagReq ["by-disallowed-inner-nodes"]
                (\ a x -> Right x{byDisallowedInnerNodes = a})
                "FILE" ""
    flagArgDelSubTreeWWords
      = flagReq ["d", "delete-subtree-with-words"]
                (\ a x -> Right x{deleteSubtreeWithWords = a})
                "FILE" ""
    flagArgRepWordsByPosTag
      = flagBool ["r", "replace-words-by-pos-tag"]
                 (\ b x -> x{replaceWordsByPosTag = b})
                 ""
    flagArgReNumSentence
      = flagReq ["n", "renumber-sentences"]
                (\ a x -> Right x{renumberSentences = read a})
                "STARTINDEX" ""
    flagArgStatLength
      = flagBool ["l", "length"]
                 (\ b x -> x{statLength = b})
                 ""
    flagArgStatGapDeg
      = flagBool ["g", "gap-degree"]
                 (\ b x -> x{statGapDeg = b})
                 ""
    flagArgStatHeight
      = flagBool ["h", "height"]
                 (\ b x -> x{statHeight = b})
                 ""
    flagArgStatIntervals
      = flagArg (\ a x -> Right x{statIntervals = a})
                "[INTERVALS]"


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs

mainArgs :: Args -> IO ()
mainArgs (Help cs) = putStr cs
mainArgs (Filter length_interval gap_degree_inteval sen_numebTestr_interval height_inteval alw_ws_file dis_ws_file alw_pos_file dis_pos_file alw_inn_file dis_inn_file)
  = do
    expContent <- T.getContents
    let negra = NT.parseNegra expContent in
      NU.putNegra $ filterNegra negra (Filter length_interval gap_degree_inteval sen_numebTestr_interval height_inteval alw_ws_file dis_ws_file alw_pos_file dis_pos_file alw_inn_file dis_inn_file)

mainArgs (Transform _ isReplacWsbyPosTags startindex)
  = do
    expContent <- T.getContents
    let negra = NT.parseNegra expContent in
      NU.putNegra $ if isReplacWsbyPosTags
        then shiftIndex startindex (replaceWdByPOS negra)
        else shiftIndex startindex negra
    where
      shiftIndex :: Int -> N.Negra -> N.Negra
      shiftIndex n (N.Negra wt st) = N.Negra wt (map (shiftId n) st)

      shiftId :: Int -> N.Sentence -> N.Sentence
      shiftId n (N.Sentence sId ed date orig com sdata) = N.Sentence (sId + n) ed date orig com sdata

      replaceWdByPOS :: N.Negra -> N.Negra
      replaceWdByPOS (N.Negra wt st) =  N.Negra wt (map repWdPos st)

mainArgs (Statistics _ lenght gap_deg height)
  = do
    expContent <- T.getContents
    let negra = NT.parseNegra expContent in
      do
      when lenght (printLengthStats negra)
      when gap_deg (printGapStats negra)
      when height (printHeightStats negra)

mainArgs (Query queryArg)
  = queryArgs queryArg

queryArgs :: QueryArgs -> IO ()
queryArgs (HelpQ cs) = putStr cs
queryArgs x = queryIt $ case x of ExVoc -> getWords
                                  ExPos -> getPos
                                  ExNod -> getNodes
  where
    queryIt :: (N.Sentence -> [String]) -> IO()
    queryIt f = do
      expContent <- T.getContents
      let negra = NT.parseNegra expContent in
        mapM_ putStrLn (sort $ nub (concatMap f (N.sentences negra)))



repWdPos :: N.Sentence -> N.Sentence
repWdPos (N.Sentence id ed date orig com sdata) = N.Sentence id ed date orig com (map wdToPOS sdata)

wdToPOS :: N.SentenceData -> N.SentenceData
wdToPOS (N.SentenceNode nd pos mt ed sed cm) = N.SentenceNode nd pos mt ed sed cm
wdToPOS (N.SentenceWord wd pos mt ed sed cm) = N.SentenceWord pos pos mt ed sed cm



getSentenceData :: N.Negra -> [[N.SentenceData]]
getSentenceData n = map N.sData (N.sentences n)

printLengthStats :: N.Negra -> IO()
printLengthStats n
  = let histo = map (\l@(x:xs) -> (x,length l)) . group . sort $ map lengthNegraSentence (getSentenceData n) in
      do
        putStrLn "Statistic by length:"
        putStrLn "Mean:"
        putStrLn (show (getMean histo))
        putStrLn "Avg.:"
        putStrLn (show (getAvg histo))
        putStrLn "Hist.:"
        putStrLn (show histo)
        putStrLn ""


printGapStats :: N.Negra -> IO()
printGapStats n
  = let histo = map (\l@(x:xs) -> (x,length l)) . group . sort $ map gapDegree (N.sentences n) in do
      putStrLn "Statistic by gap degree:"
      putStrLn "Mean:"
      putStrLn (show (getMean histo))
      putStrLn "Avg.:"
      putStrLn (show (getAvg histo))
      putStrLn "Hist.:"
      putStrLn (show histo)
      putStrLn ""


printHeightStats :: N.Negra -> IO()
printHeightStats n
  = let histo = map (\l@(x:xs) -> (x,length l)) . group . sort $ map heightNegraSentence (N.sentences n) in do
      putStrLn "Statistic by height:"
      putStrLn "Mean:"
      putStrLn (show (getMean histo))
      putStrLn "Avg.:"
      putStrLn (show (getAvg histo))
      putStrLn "Hist.:"
      putStrLn (show histo)
      putStrLn ""



getAvg :: [(Int,Int)] -> Float
getAvg x = fromIntegral (sum (map (\y -> fst y * snd y) x)) / fromIntegral (sum (map snd x))

getMean :: [(Int,Int)] -> Int
getMean x = concatMap (\(y,z) -> replicate z y) x !! (sum (map snd x) `div` 2)


filterNegra :: N.Negra -> Args -> N.Negra
filterNegra (N.Negra x y) f = N.Negra x (filterSentences y f)

filterSentences :: [N.Sentence] -> Args -> [N.Sentence]
filterSentences [] _ = []
filterSentences (x:xs) f@(Filter
  length_interval
  gap_degree_inteval
  sen_numebTestr_interval
  height_inteval
  alw_ws_file
  dis_ws_file
  alw_pos_file
  dis_pos_file
  alw_inn_file
  dis_inn_file)
    = if isInIntervals (lengthNegraSentence (N.sData x)) length_interval
        && isInIntervals (gapDegree x) gap_degree_inteval
        && isInIntervals (N.sId x) sen_numebTestr_interval
        && isInIntervals (heightNegraSentence x) height_inteval
        -- && hasWds alw_ws_file x
      then x : filterSentences xs f
      else filterSentences xs f
filterSentences x _ = x

onlyWords :: [N.SentenceData] -> [N.SentenceData]
onlyWords x = [y | y@N.SentenceWord{} <- x]

getWords :: N.Sentence -> [String]
getWords x = map N.sdWord (onlyWords (N.sData x))

getPos :: N.Sentence -> [String]
getPos x = map N.sdPostag (N.sData x)

onlyNodes :: [N.SentenceData] -> [N.SentenceData]
onlyNodes x = [y | y@N.SentenceNode{} <- x]

getNodes :: N.Sentence -> [String]
getNodes x = map (show . N.sdNum) (onlyNodes (N.sData x))



{- hasWds :: FilePath -> N.Sentence -> Bool
hasWds "/dev/null" _ = True
hasWds f x = do
  alw_wd <- readFile f
  all (`elem` (words alw_wd) getWords x -}



lengthNegraSentence :: [N.SentenceData] -> Int
lengthNegraSentence []                    = 0
lengthNegraSentence (N.SentenceWord{}:xs) = 1 + lengthNegraSentence xs
lengthNegraSentence (N.SentenceNode{}:xs) = lengthNegraSentence xs

heightOfTree :: Tree a -> Int
heightOfTree (Node _ []) = 1
heightOfTree (Node _ x)  = 1 + maximum (map heightOfTree x)

heightNegraSentence :: N.Sentence -> Int
heightNegraSentence x = heightOfTree $ N.negraToCrossedTree (N.sData x)

gapDegree :: N.Sentence -> Int
gapDegree x = gapDegTree $ N.negraToCrossedTree (N.sData x)

gapDegTree :: Tree (Maybe N.SentenceData, [N.Span]) -> Int
gapDegTree x = maximum (map gapDegTree (subForest x) ++ [length (snd (rootLabel x)) - 1])
