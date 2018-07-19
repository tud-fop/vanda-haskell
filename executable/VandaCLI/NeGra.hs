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
import           System.Console.CmdArgs.Explicit
import           System.Console.CmdArgs.Explicit.Misc
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
  -- [ (modeEmpty $ Filter (Intervals "0-") (Intervals "0-") (Intervals "0-") (Intervals "0-") "/dev/null" "/dev/null" "/dev/null" "/dev/null" "/dev/null" "/dev/null")
  [ (modeEmpty $ Filter (Intervals "0-") (Intervals "0-") (Intervals "0-") (Intervals "0-") "/dev/null" "/dev/null" "/dev/null" "/dev/null" "/dev/null" "/dev/null")
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
  , ( modeEmpty $ Statistics (Intervals "0-") False False False)
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
                (\ a x -> Right x{byLength = Intervals a})
                "INTERVALS" ""
    flagArgByGapDegree
      = flagReq ["g", "by-gap-degree"]
                (\ a x -> Right x{byGapDegree = Intervals a})
                "INTERVALS" ""
    flagArgBySentenceNumber
      = flagReq ["n", "by-sentence-number"]
                (\ a x -> Right x{bySentenceNumber = Intervals a})
                "INTERVALS" ""
    flagArgByHeight
      = flagReq ["h", "by-height"]
                (\ a x -> Right x{byHeight = Intervals a})
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
      = flagArg (\ a x -> Right x{statIntervals = Intervals a})
                "[INTERVALS]"


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs

mainArgs :: Args -> IO ()
mainArgs (Help cs) = putStr cs
mainArgs (Filter length_interval gap_degree_inteval sen_numebTestr_interval height_inteval alw_ws_file dis_ws_file alw_pos_file dis_pos_file alw_inn_file dis_inn_file)
  = do
    putStrLn "We're filtering"
    expContent <- T.getContents
    NU.putNegra $ NT.parseNegra expContent
mainArgs (Transform delSubTWs_file isReplacWsbyPosTags startindex)
  = do putStrLn "We're transforming"
mainArgs (Statistics interv lenght gap_deg height)
  = do putStrLn "Some Statistics"


-- data Intervals = Intervals String deriving (Eq, Show)
