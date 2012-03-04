-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Token
-- Copyright   :  (c) Technische Universität Dresden 2012
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Matthias.Buechse@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- This module implements the maps and arrays used for converting tokens
-- from and to integers, as well as serialization and deserialization
-- of these data structures.
--
-----------------------------------------------------------------------------

module Vanda.Token
  ( Token
  , TokenMap
  , TokenArray
  , TokenStructure
    ( emptyTS
    , fromText
    , getBounds
    , toArray
    , toMap
    , toText )
  , getToken
  , getString
  , updateToken
  ) where

import Control.Arrow ( (&&&) )
import Control.DeepSeq ( NFData(..) )
import qualified Data.Array as A
import Data.Int ( Int32 )
-- import Data.Word ( Word16 )
import qualified Data.Map as M
import qualified Data.Text.Lazy as T

type Token = Int32
newtype TokenMap = TokenMap { unTokenMap :: M.Map String Token }
newtype TokenArray = TokenArray { unTokenArray :: A.Array Token String }

instance NFData TokenMap where
  rnf = rnf . M.toList . unTokenMap

getToken :: TokenMap -> String -> Token
getToken = (M.!) . unTokenMap

getString :: TokenArray -> Token -> String
getString = (A.!) . unTokenArray

updateToken :: TokenMap -> String -> (TokenMap, Token)
updateToken orig@(TokenMap m) s
  = let t' = fromIntegral $ M.size m
        f _ _ = id
    in
    case M.insertLookupWithKey' f s t' m of
      (Nothing, m') -> (TokenMap m', t')
      (Just t, m') -> (orig, t)
  -- = case M.lookup s m of
  --     Nothing -> let t = fromIntegral $ M.size m
  --                in (TokenMap $ M.insert s t m, t)
  --     Just t -> (orig, t)

-- | This class provides common operations for 'TokenArray's and 'TokenMap's.
class TokenStructure t where
  emptyTS :: t -- ^ empty token structure
  fromText :: T.Text -> t
  getBounds :: t -> (Token, Token)
  toArray :: t -> TokenArray
  toMap :: t -> TokenMap
  toText :: t -> T.Text

instance TokenStructure TokenArray where
  emptyTS = TokenArray $ A.array (0,-1) []
  getBounds = A.bounds . unTokenArray
  toText
    = T.unlines
    . uncurry (:)
    . ( T.pack . show . (+1) . snd . A.bounds
      &&& map T.pack . A.elems )
    . unTokenArray
  fromText
    = TokenArray
    . uncurry A.array
    . ( (,) 0 . (-1+) . read . T.unpack . head
      &&& zip [0..] . map T.unpack . tail )
    . T.lines
  toArray = id
  toMap
    = TokenMap . M.fromList . map (snd &&& fst) . A.assocs . unTokenArray

instance TokenStructure TokenMap where
  emptyTS = TokenMap M.empty
  getBounds = (,) 0 . (-1+) . fromIntegral . M.size . unTokenMap
  toText = toText . toArray
  fromText
    = TokenMap
    . M.fromList
    . flip zip [0..]
    . map T.unpack
    . tail
    . T.lines
  toArray
    = TokenArray
    . uncurry A.array
    . ( (,) 0 . (-1+) . fromIntegral . M.size
      &&& map (snd &&& fst) . M.toList )
    . unTokenMap
  toMap = id