-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.GrammaticalInference.PDTA.CmdArgs
-- Copyright   :  (c) Technische Universität Dresden 2016-2017
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

-- for convenient cmdargs definitions:
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

module Vanda.GrammaticalInference.PDTA.CmdArgs where


import           Data.List (intercalate)
import           System.Console.CmdArgs.Explicit

import           System.Console.CmdArgs.Explicit.Misc
import           Vanda.Corpus.SExpression.CmdArgs


data Args
  = Help String
  | Infer
    { flagsCorpora :: CmdArgsCorpora
    , argAlpha     :: FlagAlpha
    }
  deriving (Read, Show)


data FlagAlpha
  = FAConst Double
  | FARecipCorpussize
  deriving (Read, Show)


cmdArgs :: Mode Args
cmdArgs
  = ( modes "pdta"
          (Help $ defaultHelp cmdArgs)
          "grammatical inference of probabilistic deterministic tree automata\
          \ (experimental!)"
  [ ( modeEmpty $ Infer
        { flagsCorpora = defaultCmdArgsCorpora
        , argAlpha     = undefined
        } )
    { modeNames = ["infer"]
    , modeHelp
        = flagHelpAlpha ++ " is expected to be one of " ++ optsStrAlpha ++ "."
    , modeArgs = ([flagArgAlpha], Just (flagArgCorpora lift))
    , modeGroupFlags = toGroup (modeFlagsCorpora lift)
    }
  ] )
  { modeHelpSuffix =
      [ "This is a reimplementation of the algorithm presented in \
        \“Stochastic Inference of Regular Tree Languages” \
        \by Rafael C. Carrasco, Jose Oncina, and Jorge Calera. \
        \There are two versions of this paper. Download them at:"
      , "  2001: https://doi.org/10.1023/A:1010836331703"
      , "  1998: https://doi.org/10.1007/BFb0054075"
      ]
  }
  where
    lift f = \ x -> x{flagsCorpora = f (flagsCorpora x)}
    flagArgAlpha
      = flip flagArg flagHelpAlpha
      $ \ a x -> case lookup a optsAlpha of
          Just y  -> Right x{argAlpha = y}
          Nothing -> case readEither a of
            Right y | 0 <= y && y <= 2 -> Right x{argAlpha = FAConst y}
            _ -> Left $ flagHelpAlpha ++ " is expected to be one of "
                     ++ optsStrAlpha
    flagHelpAlpha = "α"
    optsStrAlpha = intercalate ", " (map fst optsAlpha)
                ++ ", or a floating point value between 0 and 2 inclusive"
    optsAlpha = [("recip-corpussize", FARecipCorpussize)]
