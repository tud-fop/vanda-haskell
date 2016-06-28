-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Corpus.Binarization.CmdArgs
-- Description :  command line helpers for "Vanda.Corpus.Binarization"
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
-- Provide predefined command line flags and helper functions to deal with
-- "Vanda.Corpus.Binarization" (cf. "System.Console.CmdArgs.Explicit").
-----------------------------------------------------------------------------

module Vanda.Corpus.Binarization.CmdArgs
( meta
, FlagBinarization(..)
, flagReqBinarization
, encodeByFlag
, decodeByFlag
, nodetypeByFlag
, yieldByFlag
)
where


import Data.List (intercalate)
import Data.Tree
import System.Console.CmdArgs.Explicit

import Vanda.Corpus.Binarization


-- | The symbol used for nodes introduced by an encoding with 'encodeByFlag'.
meta :: String
meta = "@"


data FlagBinarization
  = FBNone
  | FBFcns
  | FBLeftbranching0
  | FBLeftbranching1
  deriving (Eq, Read, Show)


-- | Define command line flag @binarization@ with the required options @none@,
-- @fcns@, @leftbranching0@, and @leftbranching1@ (cf. 'flagReq').
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
           , ("leftbranching0", FBLeftbranching0)
           , ("leftbranching1", FBLeftbranching1) ]
    update' y x = maybe (Left err)
                        (\ z -> Right $ update z x)
                $ lookup y opts


-- | Choose an encoding by 'FlagBinarization'.
encodeByFlag :: FlagBinarization -> Tree String -> Tree String
encodeByFlag FBNone           = id
encodeByFlag FBFcns           = encodeFcns meta id
encodeByFlag FBLeftbranching0 = encodeLeftbranching0 meta meta id
encodeByFlag FBLeftbranching1 = encodeLeftbranching1 meta meta id


-- | Choose a decoding by 'FlagBinarization'.
decodeByFlag :: FlagBinarization -> Tree String -> Tree String
decodeByFlag FBNone           = id
decodeByFlag FBFcns           = decodeFcns id
decodeByFlag FBLeftbranching0 = decodeLeftbranching id
decodeByFlag FBLeftbranching1 = decodeLeftbranching id


-- | Choose @nodetypeBy…@ function by 'FlagBinarization'.
nodetypeByFlag :: FlagBinarization -> Tree String -> Nodetype
nodetypeByFlag FBNone           = nodetypeById
nodetypeByFlag FBFcns           = nodetypeByFcns
nodetypeByFlag FBLeftbranching0 = nodetypeByLeftbranching0
nodetypeByFlag FBLeftbranching1 = nodetypeByLeftbranching1 (meta ==)


-- | Get list of 'Leaf' nodes in preorder.
yieldByFlag :: FlagBinarization -> Tree String -> [String]
yieldByFlag = yieldBy . nodetypeByFlag
