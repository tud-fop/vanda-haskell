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
    , renumberSentences      :: Integer
    }
  | Statistics
    { statIntervals :: Intervals
    , statLength    :: Bool
    , statGapDeg    :: Bool
    , statHeight    :: Bool
    }
    deriving Show

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
                               , flagArgByHeight
                               , flagArgByAllowedWords
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
                (\ a x -> Right x{renumberSentences = toInteger $ read a})
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


mainArgs (Transform delSubTWs_file isReplacWsbyPosTags startindex)
  = do putStrLn "We're transforming"


mainArgs (Statistics interv lenght gap_deg height)
  = do putStrLn "Some Statistics"


filterNegra :: N.Negra -> Args -> N.Negra
filterNegra (N.Negra x y) f = N.Negra x (filterSentences y f)

filterSentences :: [N.Sentence] -> Args -> [N.Sentence]
filterSentences [] _ = []
filterSentences (x:xs) (Filter
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
        -- && True
        && isInIntervals (N.sId x) sen_numebTestr_interval
        && isInIntervals (heightNegraSentence x) height_inteval
        -- && True
      then x : filterSentences xs (Filter
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
      else filterSentences xs (Filter
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
filterSentences x _ = x

lengthNegraSentence :: [N.SentenceData] -> Int
lengthNegraSentence []                    = 0
lengthNegraSentence (N.SentenceWord{}:xs) = 1 + lengthNegraSentence xs
lengthNegraSentence (N.SentenceNode{}:xs) = lengthNegraSentence xs

heightOfTree :: Tree a -> Int
heightOfTree (Node _ []) = 1
heightOfTree (Node _ x)  = 1 + maximum (map heightOfTree x)

heightNegraSentence :: N.Sentence -> Int
heightNegraSentence x = heightOfTree $ N.negraToCrossedTree (N.sData x)

