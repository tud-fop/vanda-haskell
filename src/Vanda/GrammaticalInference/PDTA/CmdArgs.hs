-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.GrammaticalInference.PDTA.CmdArgs
-- Copyright   :  (c) Technische Universität Dresden 2016
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

module Vanda.GrammaticalInference.PDTA.CmdArgs where


import           System.Console.CmdArgs.Explicit

import           System.Console.CmdArgs.Explicit.Misc
import           Vanda.Corpus.SExpression.CmdArgs


data Args
  = Help String
  | Infer
    { flagsCorpora :: CmdArgsCorpora
    , argAlpha     :: Double
    }
  deriving (Read, Show)


cmdArgs :: Mode Args
cmdArgs
  = ( modes "pdta"
          (Help $ defaultHelp cmdArgs)
          "grammatical inference of probabilistic deterministic tree automata"
  [ ( modeEmpty $ Infer
        { flagsCorpora = defaultCmdArgsCorpora
        , argAlpha     = undefined
        } )
    { modeNames = ["infer"]
    , modeHelp =
        ""
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
    flagArgAlpha = flagArg (readUpdate $ \ a x -> x{argAlpha = a}) "α"
