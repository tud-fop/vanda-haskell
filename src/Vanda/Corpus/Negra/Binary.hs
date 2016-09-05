-- | Makes Negra data types instances of 'B.Binary' for rapid
-- (de)serialization.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Vanda.Corpus.Negra.Binary where

import qualified Data.Binary as B
import Data.Word ( Word8 )

import Vanda.Corpus.Negra

instance B.Binary Sentence where
  put s = do
    B.put $ sId s
    B.put $ sEditorId s
    B.put $ sDate s
    B.put $ sOriginId s
    B.put $ sComment s
    B.put $ sData s
  get = Sentence <$> B.get <*> B.get <*> B.get <*> B.get <*> B.get <*> B.get

putCommon :: SentenceData -> B.Put
putCommon sd = do
  B.put $ sdPostag sd
  B.put $ sdMorphtag sd
  B.put $ sdEdge sd
  B.put $ sdSecEdges sd
  B.put $ sdComment sd

instance B.Binary SentenceData where
  put sd@SentenceWord{} = do
    B.put (0 :: Word8)
    B.put $ sdWord sd
    putCommon sd
  put sd@SentenceNode{} = do
    B.put (1 :: Word8)
    B.put $ sdNum sd
    putCommon sd
  get = B.get >>= \t -> case (t :: Word8) of
    0 -> SentenceWord
            <$> B.get <*> B.get <*> B.get <*> B.get <*> B.get <*> B.get
    1 -> SentenceNode
            <$> B.get <*> B.get <*> B.get <*> B.get <*> B.get <*> B.get
    _ -> error "Parser.Negra2.get"

instance B.Binary Edge where
  put e = do
    B.put $ eLabel e
    B.put $ eParent e
  get = Edge <$> B.get <*> B.get

