-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Grammar.NGrams.KenLM
-- Copyright   :  (c) Technische Universität Dresden 2013
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Tobias.Denkinger@mailbox.tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Vanda.Grammar.NGrams.KenLM
  ( loadNGrams
  , evaluateLine
  ) where

import Data.Text.Lazy as T
import Data.List as L
import Foreign.C
import Foreign.C.String
import Foreign.Ptr
import Control.Monad

data KenTrieModel = KenTrieModel

foreign import ccall "loadModel" cLoadNGrams :: CString -> IO (Ptr KenTrieModel)
foreign import ccall "score" cEvaluateLine :: (Ptr KenTrieModel) -> CString -> IO CFloat

loadNGrams
  :: FilePath
  -> IO (Ptr KenTrieModel)
loadNGrams s
  = withCString s cLoadNGrams

evaluateLine
  :: (Ptr KenTrieModel)
  -> T.Text
  -> IO Float
evaluateLine m s
  = withCString (T.unpack s) (fmap realToFrac . cEvaluateLine m)
