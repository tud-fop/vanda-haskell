-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Main
-- Description :  unified Vanda command line tool
-- Copyright   :  (c) Technische Universität Dresden 2015-2016
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
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
import qualified Vanda.CBSM.Main
import qualified Vanda.Dyck.Main
import qualified Vanda.Grammar.XRS.LCFRS.Main
import qualified Vanda.Grammar.NGrams.Main
import qualified Vanda.GrammaticalInference.PDTA.Main
import qualified Vanda.Algorithms.IntersectWithNGrams.Main
import qualified Vanda.Grammar.PCFG.Main

#ifdef VERSION_gtk
import qualified Vanda.Algorithms.MATLearner.Main
#endif

import           System.Console.CmdArgs.Explicit


data Args
  = Help String
  | CBSM Vanda.CBSM.Main.Args
  | Dyck Vanda.Dyck.Main.Args
  | LCFRS Vanda.Grammar.XRS.LCFRS.Main.Args
  | NGrams Vanda.Grammar.NGrams.Main.Args
  | PDTA Vanda.GrammaticalInference.PDTA.Main.Args
  | XRSNGrams Vanda.Algorithms.IntersectWithNGrams.Main.Args
  | PCFGs Vanda.Grammar.PCFG.Main.Args
#ifdef VERSION_gtk
  | MATLearner Vanda.Algorithms.MATLearner.Main.Args
#endif
  deriving Show


cmdArgs :: Mode Args
cmdArgs
  = modes "vanda" (Help $ defaultHelp cmdArgs) "Vanda"
  [ remap2 Dyck (\ (Dyck x) -> x) Vanda.Dyck.Main.cmdArgs
  , remap2 CBSM (\ (CBSM x) -> x) Vanda.CBSM.Main.cmdArgs
  , remap2 LCFRS (\ (LCFRS x) -> x) Vanda.Grammar.XRS.LCFRS.Main.cmdArgs
  , remap2 NGrams (\ (NGrams x) -> x) Vanda.Grammar.NGrams.Main.cmdArgs
  , remap2 PDTA (\ (PDTA x) -> x) Vanda.GrammaticalInference.PDTA.Main.cmdArgs
  , remap2 XRSNGrams (\ (XRSNGrams x) -> x) Vanda.Algorithms.IntersectWithNGrams.Main.cmdArgs
  , remap2 PCFGs (\ (PCFGs x) -> x) Vanda.Grammar.PCFG.Main.cmdArgs
#ifdef VERSION_gtk
  , remap2 MATLearner (\ (MATLearner x) -> x) Vanda.Algorithms.MATLearner.Main.cmdArgs
#endif
  ]


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()
mainArgs (Help      cs) = putStrLn cs
mainArgs (CBSM      x ) = Vanda.CBSM.Main.mainArgs x
mainArgs (Dyck      x ) = Vanda.Dyck.Main.mainArgs x
mainArgs (LCFRS     x ) = Vanda.Grammar.XRS.LCFRS.Main.mainArgs x
mainArgs (NGrams    x ) = Vanda.Grammar.NGrams.Main.mainArgs x
mainArgs (PDTA      x ) = Vanda.GrammaticalInference.PDTA.Main.mainArgs x
mainArgs (XRSNGrams x ) = Vanda.Algorithms.IntersectWithNGrams.Main.mainArgs x
mainArgs (PCFGs     x ) = Vanda.Grammar.PCFG.Main.mainArgs x
#ifdef VERSION_gtk
mainArgs (MATLearner x) = Vanda.Algorithms.MATLearner.Main.mainArgs x
#endif