-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Technische Universität Dresden 2018
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
--
-- Makes Negra data types instances of 'NFData' for strict evaluation.
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Vanda.Corpus.Negra.NFData where

import Control.DeepSeq

import Vanda.Corpus.Negra

instance NFData Sentence where
  rnf s = rnf (sId s)
    `seq` rnf (sEditorId s)
    `seq` rnf (sDate s)
    `seq` rnf (sOriginId s)
    `seq` rnf (sComment s)
    `seq` rnf (sData s)

instance NFData SentenceData where
  rnf sd@SentenceWord{} =
          rnf (sdWord sd)
    `seq` rnf (sdPostag sd)
    `seq` rnf (sdMorphtag sd)
    `seq` rnf (sdEdge sd)
    `seq` rnf (sdSecEdges sd)
    `seq` rnf (sdComment sd)
  rnf sd@SentenceNode{} =
          rnf (sdNum sd)
    `seq` rnf (sdPostag sd)
    `seq` rnf (sdMorphtag sd)
    `seq` rnf (sdEdge sd)
    `seq` rnf (sdSecEdges sd)
    `seq` rnf (sdComment sd)

instance NFData Edge where
  rnf e = rnf (eLabel e)
    `seq` rnf (eParent e)

