-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Description :  unified Vanda command line tool
-- Copyright   :  (c) Technische Universität Dresden 2015
-- License     :  BSD-style
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Brings together all Vanda functionality to provide a unified command line
-- tool with several sub-commands.
--
-- "System.Console.CmdArgs.Explicit" is used to provide the command line interface. The actual work is done in other @Main@ modules. These modules must expose a data type @Args@ that represents the local command line options, a @'Mode' Args@ that defines the local command line interface of the module, and a function @Args -> 'IO' ()@ to do the actual work.
--
-- In this module, the local command line definitions are 'remap'ped to provide the unified command line interface. Note that the 'modeNames' of the local 'Mode's are thereby used as names for the sub-commands.
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Main
( main
, mainArgs
, cmdArgs
, Args()
) where


import           System.Console.CmdArgs.Explicit.Misc
import qualified VandaCLI.CBSM
import qualified VandaCLI.Brackets
import qualified VandaCLI.LCFRS
import qualified VandaCLI.PMCFG
import qualified VandaCLI.NGrams
import qualified VandaCLI.PDTA
import qualified VandaCLI.XRSNGrams
import qualified VandaCLI.PCFG
import qualified VandaCLI.PennToSentenceCorpus
import qualified VandaCLI.XRSToHypergraph
import qualified VandaCLI.XRSTranslate

#ifdef VERSION_gtk
import qualified VandaCLI.MATLearner
#endif

import           System.Console.CmdArgs.Explicit


data Args
  = Help String
  | Brackets VandaCLI.Brackets.Args
  | CBSM VandaCLI.CBSM.Args
  | LCFRS VandaCLI.LCFRS.Args
  | NGrams VandaCLI.NGrams.Args
  | PCFGs VandaCLI.PCFG.Args
  | PDTA VandaCLI.PDTA.Args
  | PMCFG VandaCLI.PMCFG.Args
  | PTSC VandaCLI.PennToSentenceCorpus.Args
  | XRSNGrams VandaCLI.XRSNGrams.Args
  | XRSTH VandaCLI.XRSToHypergraph.Args
  | XRST VandaCLI.XRSTranslate.Args
#ifdef VERSION_gtk
  | MATLearner VandaCLI.MATLearner.Args
#endif
  deriving Show


cmdArgs :: Mode Args
cmdArgs
  = modes "vanda" (Help $ defaultHelp cmdArgs) "Vanda"
  [ remap2 Brackets (\ (Brackets x) -> x) VandaCLI.Brackets.cmdArgs
  , remap2 CBSM (\ (CBSM x) -> x) VandaCLI.CBSM.cmdArgs
  , remap2 LCFRS (\ (LCFRS x) -> x) VandaCLI.LCFRS.cmdArgs
  , remap2 NGrams (\ (NGrams x) -> x) VandaCLI.NGrams.cmdArgs
  , remap2 PCFGs (\ (PCFGs x) -> x) VandaCLI.PCFG.cmdArgs
  , remap2 PDTA (\ (PDTA x) -> x) VandaCLI.PDTA.cmdArgs
  , remap2 PMCFG (\ (PMCFG x) -> x) VandaCLI.PMCFG.cmdArgs
  , remap2 PTSC (\ (PTSC x) -> x) VandaCLI.PennToSentenceCorpus.cmdArgs
  , remap2 XRSNGrams (\ (XRSNGrams x) -> x) VandaCLI.XRSNGrams.cmdArgs
  , remap2 XRSTH (\ (XRSTH x) -> x) VandaCLI.XRSToHypergraph.cmdArgs
  , remap2 XRST (\ (XRST x) -> x) VandaCLI.XRSTranslate.cmdArgs
#ifdef VERSION_gtk
  , remap2 MATLearner (\ (MATLearner x) -> x) VandaCLI.MATLearner.cmdArgs
#endif
  ]


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()
mainArgs (Help      cs) = putStrLn cs
mainArgs (Brackets  x ) = VandaCLI.Brackets.mainArgs x
mainArgs (CBSM      x ) = VandaCLI.CBSM.mainArgs x
mainArgs (LCFRS     x ) = VandaCLI.LCFRS.mainArgs x
mainArgs (NGrams    x ) = VandaCLI.NGrams.mainArgs x
mainArgs (PCFGs     x ) = VandaCLI.PCFG.mainArgs x
mainArgs (PDTA      x ) = VandaCLI.PDTA.mainArgs x
mainArgs (PMCFG     x ) = VandaCLI.PMCFG.mainArgs x
mainArgs (PTSC      x ) = VandaCLI.PennToSentenceCorpus.mainArgs x
mainArgs (XRSNGrams x ) = VandaCLI.XRSNGrams.mainArgs x
mainArgs (XRSTH     x ) = VandaCLI.XRSToHypergraph.mainArgs x
mainArgs (XRST      x ) = VandaCLI.XRSTranslate.mainArgs x
#ifdef VERSION_gtk
mainArgs (MATLearner x) = VandaCLI.MATLearner.mainArgs x
#endif
