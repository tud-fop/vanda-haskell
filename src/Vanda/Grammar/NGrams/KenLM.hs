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

module KenLM
  ( loadNGrams
  , evaluateLine
  ) where

import Data.Text as T
import Foreign.C
import Foreign.C.String 
import Control.Monad

data KenTrieModel = KenTrieModel

foreign import ccall "loadModel" cLoadNGrams :: CString -> IO (Ptr KenTrieModel)
foreign import ccall "score" cEvaluateLine :: (Ptr KenTrieModel) -> CString -> CFloat

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
  = fmap fromRational
  . withCString s
  $ cEvaluateLine m
