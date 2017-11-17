-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CBSM.CmdArgs
-- Copyright   :  (c) Technische Universität Dresden 2014–2017
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

-- for convenient cmdargs definitions:
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

module Vanda.CBSM.CmdArgs where


import           Data.List (intercalate)
import           System.Console.CmdArgs.Explicit

import           System.Console.CmdArgs.Explicit.Misc
import           System.FilePath ((<.>))
import           Vanda.Corpus.Binarization.CmdArgs
import           Vanda.Corpus.SExpression.CmdArgs
                   ( CmdArgsCorpora
                   , defaultCmdArgsCorpora
                   , flagArgCorpora
                   , modeFlagsCorpora
                   )


fileNameGrammar          :: Int -> FilePath
fileNameIntToTreeMap     ::        FilePath
fileNameInfo             :: Int -> FilePath
fileNameLastIteration    ::        FilePath
fileNameOptions          ::        FilePath
fileNameStatistics       ::        FilePath
fileNameEvaluations      ::        FilePath
fileNameEquivBeamIndizes ::        FilePath
fileNameLogBeamVerbose   ::        FilePath
fileNameCommandlineInfo  = "commandline"                        <.> "txt"
fileNameGrammar        i = "grammar-"              ++ show0 9 i <.> "bin"
fileNameIntToTreeMap     = "int2tree"                           <.> "bin"
fileNameInfo           i = "info-"                 ++ show0 9 i <.> "bin"
fileNameLastIteration    = "last-iteration"                     <.> "txt"
fileNameOptions          = "options"                            <.> "bin"
fileNameStatistics       = "statistics"                         <.> "csv"
fileNameEvaluations      = "statistics-evaluations"             <.> "csv"
fileNameEquivBeamIndizes = "statistics-equivalent-beam-indizes" <.> "csv"
fileNameLogBeamVerbose   = "statistics-beam-verbose"            <.> "csv"


show0 :: Show a => Int -> a -> String
show0 l i = replicate (l - length cs) '0' ++ cs
  where cs = show i


data Args
  = Help String
  | PrintCorpora
    { flagOutputFormat :: FlagOutputFormat
    , flagsCorpora     :: CmdArgsCorpora
    }
  | CBSM
    { flagRestrictMerge :: [FlagRestrictMerge]
    , flagHeuristic :: FlagHeuristic
    , flagBeamWidth :: Int
    , flagDynamicBeamWidth :: Bool
    , flagShuffle :: FlagShuffle
    , flagSeed :: Int
    , flagNormalize :: Bool
    , flagIterations :: Int
    , flagDir :: FilePath
    , flagLogBeamVerbose :: Bool
    , flagVerboseInfo :: Bool
    , flagSaveCounter :: Maybe Int
    , flagSaveTimer :: Maybe Int
    , flagsCorpora :: CmdArgsCorpora
    }
  | CBSMContinue
    { flagBeamWidth :: Int
    , flagIterations :: Int
    , flagDir :: FilePath
    }
  | ShowGrammar
    { argGrammar :: FilePath
    }
  | ShowInfo
    { flagIntToTreeMap :: FilePath
    , argInfo :: FilePath
    }
  | GrammarToDOT
    { argGrammar :: FilePath
    }
  | Parse
    { flagUnknownWords :: FlagUnknownWords
    , flagUnknownWordOutput :: FlagUnknownWordOutput
    , flagBinarization :: FlagBinarization
    , flagUnbinarize :: Bool
    , flagOutputFormat :: FlagOutputFormat
    , flagMessageNoParse :: String
    , argGrammar :: FilePath
    , argCount :: Int
    }
  | Bests
    { flagBinarization :: FlagBinarization
    , flagUnbinarize :: Bool
    , flagOutputFormat :: FlagOutputFormat
    , argGrammar :: FilePath
    , argCount :: Int
    }
  | RenderBeam
    { argRenderBeamInput :: FilePath
    , argRenderBeamOutput :: FilePath
    , flagRunLengthEncoding :: Bool
    , argColumn :: Int
    , flagSortFormatString :: String
    , flagColormapMin :: Double
    , flagColormapMax :: Double
    , flagChunkSize :: Int
    , flagChunkCruncher :: FlagChunkCruncher
    }
  | RenderBeamInfo
    { argRenderBeamInput :: FilePath
    , argRenderableCats :: String
    , flagSortFormatString :: String
    , argInfo :: FilePath
    , argIntToTreeMap :: FilePath
    , flagChunkSize :: Int
    , flagChunkCruncher :: FlagChunkCruncher
    , argRenderBeamOutput :: FilePath
    }
  | RecognizeTrees
    { flagPrintRecognizable :: Bool
    , flagsCorpora          :: CmdArgsCorpora
    , argGrammar            :: FilePath
    }
  deriving (Read, Show)

data FlagChunkCruncher
  = FCCMinimum | FCCMaximum | FCCMinority | FCCMajority | FCCMedian
  deriving (Eq, Read, Show)

data FlagHeuristic
  = FHCountSum | FHPartialLikelihoodDelta deriving (Eq, Read, Show)

data FlagShuffle = FSNone | FSStates | FSMerges deriving (Eq, Read, Show)

data FlagOutputFormat
  = FOFBranches | FOFPenn | FOFPretty | FOFYield deriving (Eq, Read, Show)

data FlagRestrictMerge
  = FRMBinLeaf | FRMBinMeta | FRMLeafs | FRMTerminals | FRMUnary
  deriving (Eq, Read, Show)

data FlagUnknownWords = FUWStrict | FUWArbitrary deriving (Eq, Read, Show)

data FlagUnknownWordOutput
       = FUWOOriginal | FUWOReplacement | FUWOBoth deriving (Eq, Read, Show)


cmdArgs :: Mode Args
cmdArgs
  = ( modes "cbsm" (Help $ defaultHelp cmdArgs) "Count-Based State Merging"
  [ ( modeEmpty PrintCorpora
        { flagOutputFormat = FOFPretty
        , flagsCorpora     = defaultCmdArgsCorpora
        } )
    { modeNames = ["print-corpora"]
    , modeHelp =
        "Print trees from TREEBANKs. Can be used to check for parsing \
        \errors. Every TREEBANK can be a file or a directory. Directories \
        \are traversed recursively. If no TREEBANK is given, the trees are \
        \read from standard input. \
        \The filters (if used) apply in the order penn-filter, defoliate, \
        \filter-by-leafs and finally filter-by-length."
    , modeArgs = ([], Just (flagArgCorpora liftCmdArgsCorpora))
    , modeGroupFlags = toGroup
      $ [ flagReqOutputFormat
        ] ++ modeFlagsCorpora liftCmdArgsCorpora
    }
  , ( modeEmpty CBSM
        { flagRestrictMerge    = []
        , flagHeuristic        = FHCountSum
        , flagBeamWidth        = 1000
        , flagDynamicBeamWidth = False
        , flagShuffle          = FSNone
        , flagSeed             = 0
        , flagNormalize        = False
        , flagIterations       = pred maxBound
        , flagDir              = ""
        , flagLogBeamVerbose   = False
        , flagVerboseInfo      = False
        , flagSaveCounter      = Nothing
        , flagSaveTimer        = Nothing
        , flagsCorpora         = defaultCmdArgsCorpora
        })
    { modeNames = ["cbsm"]
    , modeHelp = "Read-off a grammar from TREEBANKs and generalize it. See \
        \print-corpora for further information about the TREEBANK arguments."
    , modeArgs = ([], Just (flagArgCorpora liftCmdArgsCorpora))
    , modeGroupFlags = toGroup
      $ [ flagReqRestrictMerge
        , flagReqHeuristic
        , flagReqBeamWidth
        , flagNoneDynamicBeamWidth
        , flagReqShuffle
        , flagReqSeed
        , flagNoneNormalize
        , flagReqIterations
        , flagReqDir
        , flagNoneLogBeamVerbose
        , flagNoneVerboseInfo
        , flagReqSaveCounter
        , flagReqSaveTimer
        ] ++ modeFlagsCorpora liftCmdArgsCorpora
    }
  , (modeEmpty $ CBSMContinue 1000 (pred maxBound) "")
    { modeNames = ["cbsm-continue"]
    , modeHelp = "Continue cbsm training with a grammar."
    , modeGroupFlags = toGroup
        [ flagReqBeamWidth
        , flagReqIterations
        , flagReqDir
        ]
    }
  , (modeEmpty $ ShowGrammar "")
    { modeNames = ["show-grammar"]
    , modeHelp = "Pretty print a trained grammar."
    , modeArgs =
        ( [ flagArgGrammar{argRequire = True}
          ]
        , Nothing
        )
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
  , (modeEmpty $ GrammarToDOT "")
    { modeNames = ["grammar-to-dot"]
    , modeHelp = "Write a grammar as a hypergraph in DOT format to stdout."
    , modeHelpSuffix =
        [ "Recommended usage:"
        , "  ... grammar-to-dot GRAMMAR-FILE | dot -Tpdf > PDF-FILE"
        ]
    , modeArgs =
        ( [ flagArgGrammar{argRequire = True}
          ]
        , Nothing
        )
    }
  , (modeEmpty $ Parse FUWStrict FUWOOriginal FBNone False FOFPretty "" "" 1)
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
        , flagReqBinarization (\ b x -> x{flagBinarization = b})
        , flagNoneUnbinarize
        , flagReqOutputFormat
        , flagReqMessageNoParse
        ]
    }
  , (modeEmpty $ Bests FBNone False FOFPretty "" 1)
    { modeNames = ["bests"]
    , modeHelp = "View best trees of a grammar."
    , modeArgs =
        ( [ flagArgGrammar{argRequire = True}
          , flagArgCount
          ]
        , Nothing
        )
    , modeGroupFlags = toGroup
        [ flagReqBinarization (\ b x -> x{flagBinarization = b})
        , flagNoneUnbinarize
        , flagReqOutputFormat
        ]
    }
  , (modeEmpty $ RenderBeam "" "" False (-1) "" 0 1 1 FCCMedian)
    { modeNames = ["render-beam"]
    , modeHelp = "Render " ++ fileNameEvaluations ++ " into a png image."
    , modeArgs =
        ( [ flagArgRenderBeamInput{argRequire = True}
          , flagArgColumn{argRequire = True}
          , flagArgRenderBeamOutput{argRequire = True}
          ]
        , Nothing
        )
    , modeGroupFlags = toGroup
        [ flagNoneRunLengthEncoding
        , flagReqSortFormatString
        , flagReqColormapMin
        , flagReqColormapMax
        , flagReqChunkSize
        , flagReqChunkCruncher
        ]
    }
  , (modeEmpty $ RenderBeamInfo "" "" "" "" "" 1 FCCMedian "")
    { modeNames = ["render-beam-info"]
    , modeHelp = "Render " ++ fileNameLogBeamVerbose ++ " into a png image."
    , modeArgs =
        ( [ flagArgRenderBeamInput{argRequire = True}
          , flagArgRenderableCats{argRequire = True}
          , flagArgMergeTreeMap{argRequire = True}
          , flagArgIntToTreeMap{argRequire = True}
          , flagArgRenderBeamOutput{argRequire = True}
          ]
        , Nothing
        )
    , modeGroupFlags = toGroup
        [ flagReqSortFormatString
        , flagReqChunkSize
        , flagReqChunkCruncher
        ]
    }
  , (modeEmpty RecognizeTrees
       { flagPrintRecognizable = False
       , flagsCorpora          = defaultCmdArgsCorpora
       , argGrammar            = ""
       })
    { modeNames = ["recognize-trees"]
    , modeHelp = "Recognize trees using some grammar. Output: NUMBER-OF-STATES \
                 \LIKELIHOOD NONZERO-TREE-COUNT GEOM-MEAN-LIKELIHOOD (all \
                 \likelihoods log_2)"
    , modeArgs =
        ( [flagArgGrammar{argRequire = True}]
        , Just (flagArgCorpora liftCmdArgsCorpora)
        )
    , modeGroupFlags = toGroup
      $ [ flagNonePrintRecognizable
        ] ++ modeFlagsCorpora liftCmdArgsCorpora
    }
  ] )
  { modeHelpSuffix =
      [ "This is an implementation of the approach presented in \
        \“Count-based State Merging for Probabilistic Regular Tree Grammars” \
        \by Toni Dietze and Mark-Jan Nederhof."
      , "Download the paper at:"
      , "  https://aclweb.org/anthology/sigfsm.html#2015_0"
      , "  http://aclanthology.info/papers/\
        \count-based-state-merging-for-probabilistic-regular-tree-grammars"
      ]
  }
  where
    liftCmdArgsCorpora f
      = \ x -> x{flagsCorpora = f (flagsCorpora x)}
    flagNoneDynamicBeamWidth
      = flagNone ["dynamic-beam-width"] (\ x -> x{flagDynamicBeamWidth = True})
          "The actual beam width is at least as defined by --beam-width, but \
          \if this flag is enabled, then the actual beam width is extended \
          \to capture all candidates that have a heuristic value that is as \
          \good as for candidates within --beam-width"
    flagNoneNormalize
      = flagNone ["normalize"] (\ x -> x{flagNormalize = True})
          "normalize log likelihood deltas by number of merged states"
    flagNoneUnbinarize
      = flagNone ["unbinarize"] (\ x -> x{flagUnbinarize = True})
          "Undo the binarization before the output. Might fail if a tree is \
          \no result of a binarization."
    flagNoneLogBeamVerbose
      = flagNone ["log-beam-verbose"] (\ x -> x{flagLogBeamVerbose = True})
          (  "Write all information about the search beam to "
          ++ fileNameLogBeamVerbose   ++ "." )
    flagNoneVerboseInfo
      = flagNone ["verbose-info"] (\ x -> x{flagVerboseInfo = True})
          "Write the full beams to the info files. Otherwise only the chosen \
          \candidates is saved."
    flagReqOutputFormat
      = flagReq [flag] update "FORMAT" ("one of " ++ optsStr ++ ". Default: pretty.")
      where
        flag = "output-format"
        err  = flag ++ " expects one of " ++ optsStr
        optsStr = intercalate ", " (map fst opts)
        opts = [ ("branches", FOFBranches)
               , ("penn"    , FOFPenn    )
               , ("pretty"  , FOFPretty  )
               , ("yield"   , FOFYield   )
               ]
        update y x = maybe (Left err) (\ z -> Right x{flagOutputFormat = z})
                   $ lookup y opts
    flagReqRestrictMerge
      = flagReq [flag] update "OPT"
      $ unlines
          [ "one of " ++ optsStr ++ "."
          , "If this flag is used more than once, all named OPTs are applied simultaneously."
          , "binleaf: states producing nodes that were leafs before binarization are not merged with other states."
          , "binmeta: states producing nodes that were introduced by binarization are not merged with other states."
          , "leafs: states producing leafs are not merged with other states."
          , "terminals: only states producing the same terminal may be merged."
          , "unary: states producing unary nodes are not merged with other states."
          ]
      where
        flag = "restrict-merge"
        err  = flag ++ " expects one of " ++ optsStr
        optsStr = intercalate ", " (map fst opts)
        opts = [ ("binleaf"  , FRMBinLeaf  )
               , ("binmeta"  , FRMBinMeta  )
               , ("leafs"    , FRMLeafs    )
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
        \as replacment for an unknown word. Default: strict."
      where
        flag = "unknown-words"
        err  = flag ++ " expects one of " ++ optsStr
        optsStr = intercalate ", " (map fst opts)
        opts = [("strict", FUWStrict), ("arbitrary", FUWArbitrary)]
        update y x = maybe (Left err) (\ z -> Right x{flagUnknownWords = z})
                   $ lookup y opts
    flagReqUnknownWordOutput
      = flagReq [flag] update "MODE"
      $ "one of " ++ optsStr ++  ". Default: original."
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
    flagReqShuffle
      = flagReq [flag] update "MODE"
      $ unlines
          [ "one of " ++ optsStr ++ "."
          , "This flag allows to shuffle the order of merge candidates on \
            \the search beam."
          , "none: disable shuffling."
          , "states: shuffle states with the same count."
          , "merges: shuffle merges with the same heuristic value."
          , "Shuffling merges subsumes shuffling of states."
          ]
      where
        flag = "shuffle"
        err  = flag ++ " expects one of " ++ optsStr
        optsStr = intercalate ", " (map fst opts)
        opts = [ ("none"  , FSNone  )
               , ("states", FSStates)
               , ("merges", FSMerges) ]
        update y x = maybe (Left err)
                           (\ z -> Right x{flagShuffle = z})
                   $ lookup y opts
    flagReqHeuristic
      = flagReq [flag] update "HEURISTIC"
      $ unlines
          [ "one of " ++ optsStr ++ "."
          , "Default: cs (count sum)."
          , "Choose the heuristic used to decide which pairs of states lie in the beam, i.e., state pairs that are considered for merging. The state pairs with the largest heuristic values are chosen."
          , "cs (count sum): The counts of the states are added and negated, i.e., states with low counts are preferred."
          , "pld (partial likelihood delta): A subterm of the actual likelihood delta induced by merging is used, namely log₂ x^x * y^y / (x+y)^(x+y) where x and y are the counts of the two states."
          ]
      where
        flag = "heuristic"
        err  = flag ++ " expects one of " ++ optsStr
        optsStr = intercalate ", " (map fst opts)
        opts = [ ("cs" , FHCountSum              )
               , ("pld", FHPartialLikelihoodDelta) ]
        update y x = maybe (Left err)
                           (\ z -> Right x{flagHeuristic = z})
                   $ lookup y opts
    flagReqBeamWidth
      = flagReq ["beam-width"]
                (readUpdate $ \ a x -> x{flagBeamWidth = a})
                "BEAMWIDTH"
                "Larger values refine the search for the best merge \
                \candidate"
    flagReqSeed
      = flagReq ["seed"]
                (readUpdate $ \ a x -> x{flagSeed = a})
                "SEED"
                "an Int used for initialization of the pseudo random number \
                \generator"
    flagReqIterations
      = flagReq ["iterations"]
                (readUpdate $ \ a x -> x{flagIterations = a})
                "ITERATIONS"
                "limit number of iterations"
    flagReqSaveCounter
      = flagReq ["save-counter"]
                (readUpdate $ \ a x -> x{flagSaveCounter = Just a})
                "N"
                "Save result every N-th iteration."
    flagReqSaveTimer
      = flagReq ["save-timer"]
                (readUpdate $ \ a x -> x{flagSaveTimer = Just (1000000 * a)})
                "N"
                "Save result every N seconds."
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
    flagArgMergeTreeMap
      = flagArg (\ a x -> Right x{argInfo = a}) "INFO-FILE"
    flagArgIntToTreeMap
      = flagArg (\ a x -> Right x{argIntToTreeMap = a}) "INT2TREE-FILE"
    flagArgGrammar
      = flagArg (\ a x -> Right x{argGrammar = a}) "GRAMMAR-FILE"
    flagNonePrintRecognizable
      = flagNone ["print-recognizable"] (\ x -> x{flagPrintRecognizable = True})
          "also print all recognizable trees out again"
    flagArgCount
      = flagArg (readUpdate $ \ a x -> x{argCount = a}) "COUNT"
    flagArgRenderBeamInput
      = flagArg (\ a x -> Right x{argRenderBeamInput = a}) "CSV-FILE"
    flagArgRenderBeamOutput
      = flagArg (\ a x -> Right x{argRenderBeamOutput = a}) "PNG-FILE"
    flagArgRenderableCats
      = flagArg (\ a x -> Right x{argRenderableCats = a}) "RENDERABLE-CATEGORIES"
    flagNoneRunLengthEncoding
      = flagNone ["rle"] (\ x -> x{flagRunLengthEncoding = True})
          "candidates of an iteration are run-length-encoding-compressed"
    flagArgColumn
      = flagArg (readUpdate $ \ a x -> x{argColumn = a}) "COLUMN"
    flagReqSortFormatString
      = flagReq ["sorting"] (\ a x -> Right x{flagSortFormatString = a})
                "SORTING-FORMAT-STRING"
                "comma-separated descriptors like '0a,m(NP|S|*|-),3d', where '|*|-' (all other pure merges|all mixed merges) are always implied as the last categories"
    flagReqColormapMin
      = flagReq ["colormapmin"] (readUpdate $ \ a x -> x{flagColormapMin = a})
                "COLORMAPMIN" "value mapped to the minimum color (default 0.0)"
    flagReqColormapMax
      = flagReq ["colormapmax"] (readUpdate $ \ a x -> x{flagColormapMax = a})
                "COLORMAPMAX" "value mapped to the maximum color (default 1.0)"
    flagReqChunkSize
      = flagReq ["chunksize"] (readUpdate $ \ a x -> x{flagChunkSize = a})
                "CHUNKSIZE" "chunk size (default 1)"
    flagReqChunkCruncher
      = flagReq [flag] update "MODE"
      $ unlines
          [ "one of " ++ optsStr ++ "."
          , "Determines candidate choice within a chunk."
          , "minimum/maximum: (according to Ord instance)."
          , "majority: most common element in chunk."
          , "minority: rarest element in chunk."
          , "median: middle element of ordering (second of these for even chunksize)"
          , "DEFAULT: median"
          ]
      where
        flag = "chunkcruncher"
        err  = flag ++ " expects one of " ++ optsStr
        optsStr = intercalate ", " (map fst opts)
        opts = [ ("minimum" , FCCMinimum)
               , ("maximum" , FCCMaximum)
               , ("minority", FCCMinority)
               , ("majority", FCCMajority)
               , ("median"  , FCCMedian) ]
        update y x = maybe (Left err)
                           (\ z -> Right x{flagChunkCruncher = z})
                   $ lookup y opts
