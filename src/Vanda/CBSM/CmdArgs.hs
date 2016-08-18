-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CBSM.CmdArgs
-- Copyright   :  (c) Technische Universität Dresden 2014–2016
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
import           Vanda.Corpus.Binarization.CmdArgs


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
    { flagBeamWidth :: Int
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
    }
  deriving (Read, Show)


data FlagOutputFormat
  = FOFPretty | FOFPenn | FOFYield deriving (Eq, Read, Show)

data FlagRestrictMerge
  = FRMBinLeaf | FRMBinMeta | FRMLeafs | FRMTerminals | FRMUnary
  deriving (Eq, Read, Show)

data FlagUnknownWords = FUWStrict | FUWArbitrary deriving (Eq, Read, Show)

data FlagUnknownWordOutput
       = FUWOOriginal | FUWOReplacement | FUWOBoth deriving (Eq, Read, Show)


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
        , flagReqOutputFormat
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
  , (modeEmpty $ CBSMContinue 1000 (pred maxBound) "")
    { modeNames = ["cbsm-continue"]
    , modeHelp = "Continue cbsm training with a grammar."
    , modeGroupFlags = toGroup
        [ flagReqBeamWidth
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
  , (modeEmpty $ RenderBeam "" "")
    { modeNames = ["render-beam"]
    , modeHelp = "Render statistics-evaluations.csv into a png image."
    , modeArgs =
        ( [ flagArgRenderBeamInput{argRequire = True}
          , flagArgRenderBeamOutput{argRequire = True}
          ]
        , Nothing
        )
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
    flagNoneUnbinarize
      = flagNone ["unbinarize"] (\ x -> x{flagUnbinarize = True})
          "Undo the binarization before the output. Might fail if a tree is \
          \no result of a binarization."
    flagReqOutputFormat
      = flagReq [flag] update "FORMAT" ("one of " ++ optsStr)
      where
        flag = "output-format"
        err  = flag ++ " expects one of " ++ optsStr
        optsStr = intercalate ", " (map fst opts)
        opts = [("pretty", FOFPretty), ("penn", FOFPenn), ("yield", FOFYield)]
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
    flagArgRenderBeamInput
      = flagArg (\ a x -> Right x{argRenderBeamInput = a}) "CSV-FILE"
    flagArgRenderBeamOutput
      = flagArg (\ a x -> Right x{argRenderBeamOutput = a}) "PNG-FILE"
