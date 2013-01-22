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
-- Uses the C library KenLM from <http://kheafield.com/code/kenlm/> to load
-- APRA files and score sentences.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Vanda.Grammar.NGrams.KenLM
  ( -- * Loading models
    loadNGrams
    -- * Constants
  , beginSentenceState
  , nullContextState
    -- * Scoring
  , evaluate
  , evaluateLine
  ) where

import Data.Text.Lazy as T
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe

data KenTrieModel = KenTrieModel
data State = State

foreign import ccall "loadModel" cLoadNGrams
                 :: CString -> IO (Ptr KenTrieModel)

foreign import ccall "beginSentenceState" cBeginSentenceState
                 :: Ptr KenTrieModel -> Ptr State

foreign import ccall "nullContextState" cNullContextState
                 :: Ptr KenTrieModel -> Ptr State

foreign import ccall "lookup" cLookup
                 :: Ptr KenTrieModel -> Ptr State -> CString -> IO CFloat

foreign import ccall "score" cEvaluateLine
                 :: Ptr KenTrieModel -> CString -> IO CFloat

-- | Loads a KenTrieModel from a binary ARPA file containing a TrieModel OR
-- a textual ARPA file.
loadNGrams
  :: FilePath                  -- ^ file name
  -> Ptr KenTrieModel          -- ^ model
loadNGrams s
  = unsafePerformIO $ withCString s cLoadNGrams

-- | Returns the State to use when at the beginning of a sentence.
beginSentenceState
  :: Ptr KenTrieModel          -- ^ model
  -> Ptr State                 -- ^ 'State' for the beginning of a sentence
beginSentenceState
  = cBeginSentenceState

-- | Returns the State to use when there is no context.
nullContextState
  :: Ptr KenTrieModel          -- ^ model
  -> Ptr State                 -- ^ 'State' for empty context
nullContextState
  = cNullContextState

-- | Scores a phrase beginning with a 'State'.
evaluate
  :: Ptr KenTrieModel          -- ^ model
  -> Ptr State                 -- ^ 'State' to start with
  -> T.Text                    -- ^ phrase to score
  -> Float                     -- ^ score
evaluate m s t
  = unsafePerformIO
  $ withCString
      (T.unpack t)
      (fmap realToFrac . cLookup m s)

-- | Scores a whole sentence.
evaluateLine
  :: Ptr KenTrieModel          -- ^ model
  -> T.Text                    -- ^ sentence to score
  -> Float                     -- ^ score
evaluateLine m s
  = unsafePerformIO
  $ withCString
      (T.unpack s)
      (fmap realToFrac . cEvaluateLine m)
