{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

{-|
Module:      VandaCLI.NeGra
Description: membership in /Dyck languages/ and /congruence multiple Dyck languages/
Copyright:   to be discussed
License:     BSD-style
Maintainer:  Felix.Völker@tu-dresden.de
Stability:   unknown

TODO Documentaion
-}
module VandaCLI.NeGra
( main
, mainArgs
, cmdArgs
, Args()
) where


import           System.Console.CmdArgs.Explicit
import           System.Console.CmdArgs.Explicit.Misc



data Args
  = Help String
  | Filter
    { by_length                 :: Intervals
    , by_gap_degree             :: Intervals
    , by_sentence_number        :: Intervals
    , by_height                 :: Intervals
    , by_allowed_words          :: FilePath
    , by_disallowed_words       :: FilePath
    , by_allowed_pos_tags       :: FilePath
    , by_disallowed_pos_tags    :: FilePath
    , by_allowed_inner_nodes    :: FilePath
    , by_disallowed_inner_nodes :: FilePath
    }
  | Transform
    { delete_subtree_with_words :: FilePath
    , replace_words_by_pos_tag  :: Bool
    , renumber_sentences        :: Integer
    }
    deriving Show

cmdArgs :: Mode Args
cmdArgs
  = modes "negra" (Help $ defaultHelp cmdArgs) "tools for the NeGra export format"
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
  , (modeEmpty $ Transform "/dev/null" False 0)
    { modeNames = ["transform"]
    , modeHelp = "transforms a corpus according to the specified rules"
    , modeGroupFlags = toGroup [ flagArgDelSubTreeWWords
                               , flagArgRepWordsByPosTag
                               , flagArgReNumSentence
                               ]
                               }
                               ]
  where
    flagArgByLength
      = flagReq ["l", "by-length"]
                (\ a x -> Right x{by_length = Intervals a})
                "INTERVALS" ""
    flagArgByGapDegree
      = flagReq ["g", "by-gap-degree"]
                (\ a x -> Right x{by_gap_degree = Intervals a})
                "INTERVALS" ""
    flagArgBySentenceNumber
      = flagReq ["n", "by-sentence-number"]
                (\ a x -> Right x{by_sentence_number = Intervals a})
                "INTERVALS" ""
    flagArgByHeight
      = flagReq ["h", "by-height"]
                (\ a x -> Right x{by_height = Intervals a})
                "INTERVALS" ""
    flagArgByAllowedWords
      = flagReq ["w", "by-allowed-words"]
                (\ a x -> Right x{by_allowed_words = a})
                "FILE" ""
    flagArgByDisallowedWords
      = flagReq ["by-disallowed-words"]
                (\ a x -> Right x{by_disallowed_words = a})
                "FILE" ""
    flagArgByAllowedPosTags
      = flagReq ["p", "by-allowed-pos-tags"]
                (\ a x -> Right x{by_allowed_pos_tags = a})
                "FILE" ""
    flagArgByDisallowedPosTags
      = flagReq ["by-disallowed-pos-tags"]
                (\ a x -> Right x{by_disallowed_pos_tags = a})
                "FILE"""
    flagArgByAllowedInnerNodes
      = flagReq ["i", "by-allowed-inner-nodes"]
                (\ a x -> Right x{by_allowed_inner_nodes = a})
                "FILE" ""
    flagArgByDisallowedInnerNodes
      = flagReq ["by-disallowed-inner-nodes"]
                (\ a x -> Right x{by_disallowed_inner_nodes = a})
                "FILE" ""
    flagArgDelSubTreeWWords
      = flagReq ["d", "delete-subtree-with-words"]
                (\ a x -> Right x{delete_subtree_with_words = a})
                "FILE" ""
    flagArgRepWordsByPosTag
      = flagBool ["r", "replace-words-by-pos-tag"]
                (\ b x -> x{replace_words_by_pos_tag = b})
                ""
    flagArgReNumSentence
      = flagReq ["n", "renumber-sentences"]
                (\ a x -> Right x{renumber_sentences = toInteger $ read a})
                "STARTINDEX" ""

main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs

mainArgs :: Args -> IO ()
mainArgs (Help cs) = putStr cs
mainArgs (Filter length_interval gap_degree_inteval sen_numebr_interval height_inteval alw_ws_file dis_ws_file alw_pos_file dis_pos_file alw_inn_file dis_inn_file)
  = do putStrLn "We're filtering"
mainArgs (Transform delSubTWs_file isReplacWsbyPosTags startindex)
  = do putStrLn "We're transforming"

data Intervals = Intervals String deriving (Eq, Show)
