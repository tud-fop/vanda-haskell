{-# LANGUAGE FlexibleInstances, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Token
-- Copyright   :  (c) Technische Universität Dresden 2012
-- License     :  BSD-style
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
    ( TokenArray
    , unTokenArray )
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
import qualified Data.Array as A
-- import Data.Word ( Word16 )
-- import qualified Data.Map as M
import qualified Data.HashMap.Strict as M
import Data.Interner
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import Vanda.Util

type Token = Int
type TokenMap = Interner TS.Text
newtype TokenArray = TokenArray { unTokenArray :: A.Array Token TS.Text }

getToken :: TokenMap -> TS.Text -> Token
getToken = flip (M.lookupDefault (-1)) . inMap

getString :: TokenArray -> Token -> TS.Text
getString a x = if   elem x (A.indices $ unTokenArray a)
                then (unTokenArray a) A.! x
                else error ("not found " ++ (show x) ++ " in " ++ (show (A.bounds (unTokenArray a))))

updateToken :: TokenMap -> TS.Text -> (TokenMap, Token)
updateToken = intern
  -- = let t' = fromIntegral $ M.size m
  --       f _ _ = id
  --   in
  --   case M.insertLookupWithKey' f s t' m of
  --     (Nothing, m') -> (TokenMap m', t')
  --     (Just t, _) -> (orig, t)

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
    . ( T.pack . show . (+ 1) . snd . A.bounds
      &&& map (T.pack . TS.unpack) . A.elems )
    . unTokenArray
  fromText
    = TokenArray
    . uncurry A.array
    . ( (,) 0 . (-1+) . read . T.unpack . head
      &&& zip [0..] . map (TS.pack . T.unpack) . tail )
    . T.lines
  toArray = id
  toMap TokenArray{ .. }
    = let (_, n) = A.bounds unTokenArray
          inSize = n + 1
          inMap = M.fromList $ map swap $ A.assocs $ unTokenArray
      in Interner{ .. }

instance TokenStructure TokenMap where
  emptyTS = Interner{ inMap = M.empty, inSize = 0 }
  getBounds = (,) 0 . (-1+) . inSize
  toText = toText . toArray
  fromText t
    = let (l : ls) = T.lines t
          inSize = read $ T.unpack $ l
          inMap = M.fromList $ flip zip [0..] $ map (TS.pack . T.unpack) $ ls
      in Interner{ .. }
  toArray Interner{ .. }
    = TokenArray
    $ A.array (0, inSize - 1)
    $ map (snd &&& fst)
    $ M.toList inMap
  toMap = id
