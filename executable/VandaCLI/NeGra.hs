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


import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Explicit.Misc

data Args
  = Help String
  | Filter
    { by_length :: Intervals
    , by_gap_degree :: Intervals
    , by_sentence_number :: Intervals
    , by_height :: Intervals
    , by_allowed_words :: FilePath
    , by_disallowed_words :: FilePath
    , by_allowed_pos_tags :: FilePath
    , by_disallowed_pos_tags :: FilePath
    , by_allowed_inner_nodes :: FilePath
    , by_disallowed_inner_nodes :: FilePath
    }
  deriving Show

cmdArgs :: Mode Args
cmdArgs
  = modes "brackets" (Help $ defaultHelp cmdArgs) "tools for the NeGra export format"
  [ (modeEmpty $ Interval undefined)
    { modeNames = ["filter"]
    , modeHelp = "filters a corpus according to specified predicates"
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
  ]
  where
    flagArgByLength
      = flagReq ["l", "by-length"]
                (\ a x -> Right x{by_length = a})
                "INTERVALS"
    flagArgByGapDegree
      = flagReq ["g", "by-gap-degree"]
                (\ a x -> Right x{by_gap_degree = a})
                "INTERVALS"
    flagArgBySentenceNumber
      = flagReq ["n", "by-sentence-number"]
                (\ a x -> Right x{by_sentence_number = a})
                "INTERVALS"
    flagArgByHeight
      = flagReq ["h", "by-height"]
                (\ a x -> Right x{by_height = a})
                "INTERVALS"
    flagArgByAllowedWords
      = flagReq ["w", "by-allowed-words"]
                (\ a x -> Right x{by_allowed_words = a})
                "FILE"
    flagArgByDisallowedWords
      = flagReq ["by-disallowed-words"]
                (\ a x -> Right x{by_disallowed_words = a})
                "FILE"
    flagArgByAllowedPosTags
      = flagReq ["p", "by-allowed-pos-tags"]
                (\ a x -> Right x{by_allowed_pos_tags = a})
                "FILE"
    flagArgByDisallowedPosTags
      = flagReq ["by-disallowed-pos-tags"]
                (\ a x -> Right x{by_disallowed_pos_tags = a})
                "FILE"
    flagArgByAllowedInnerNodes
      = flagReq ["i", "by-allowed-inner-nodes"]
                (\ a x -> Right x{by_allowed_inner_nodes = a})
                "FILE"
    flagArgByDisallowedInnerNodes
      = flagReq ["by-disallowed-inner-nodes"]
                (\ a x -> Right x{by_disallowed_inner_nodes = a})
                "FILE"

main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs

mainArgs :: Args -> IO ()
mainArgs (Help cs) = putStr cs
mainArgs (Filter)
  = do putStrLn "We're filtering"

data Intervals = Intervals Integer Integer
