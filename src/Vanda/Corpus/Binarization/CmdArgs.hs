-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Corpus.Binarization.CmdArgs
-- Description :
-- Copyright   :  (c) Technische Universität Dresden 2016
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Provide predefined command line flags to deal with
-- "Vanda.Corpus.Binarization" (cf. "System.Console.CmdArgs.Explicit").
-----------------------------------------------------------------------------

module Vanda.Corpus.Binarization.CmdArgs where


import Data.List (intercalate)
import Data.Tree
import System.Console.CmdArgs.Explicit

import Vanda.Corpus.Binarization


data FlagBinarization = FBNone | FBFcns | FBLeftbranching deriving (Eq, Show)


flagReqBinarization :: (FlagBinarization -> a -> a) -> Flag a
flagReqBinarization update
  = flagReq [flag] update' "MODE"
  $ "one of " ++ optsStr
  where
    flag = "binarization"
    err  = flag ++ " expects one of " ++ optsStr
    optsStr = intercalate ", " (map fst opts)
    opts = [ ("none", FBNone)
           , ("fcns", FBFcns)
           , ("leftbranching", FBLeftbranching) ]
    update' y x = maybe (Left err)
                        (\ z -> Right $ update z x)
                $ lookup y opts


flaggedBinarization :: FlagBinarization -> Tree [Char] -> Tree [Char]
flaggedBinarization FBNone          = id
flaggedBinarization FBFcns          = fcns "NULL" id
flaggedBinarization FBLeftbranching = leftbranching "NULL" "CONS" id
