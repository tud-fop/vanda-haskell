-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Grammar.NGrams.Text
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
-- Parses textual ARPA format to provide a NGrams language model.
--
-----------------------------------------------------------------------------

module Vanda.Grammar.NGrams.Text
  ( parseNGrams
  ) where

import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Vanda.Grammar.NGrams.VandaNGrams as N

-- | Parses textual ARPA format to provide a NGrams language model.
parseNGrams
  :: T.Text                  -- ^ Text to parse
  -> N.NGrams T.Text         -- ^ generated NGrams
parseNGrams
  = (\(n, ts) -> L.foldl' parseLine (N.empty (T.pack "<unk>") (T.pack "<s>") (T.pack "</s>") n) ts)
  . L.foldl' filterLines (0, [])
  . T.lines

isAWantedLine
  :: T.Text                  -- ^ line to check
  -> Bool                    -- ^ true iff the line contains an NGram
isAWantedLine l
  = not
  . or
  . map (\ f -> f l)
  $ [ T.isPrefixOf (T.pack "\\") , T.isPrefixOf (T.pack "ngram "), T.null ]

filterLines
  :: (Int, [T.Text])
  -> T.Text
  -> (Int, [T.Text])
filterLines (nOld, xs) t
  = if   T.isPrefixOf (T.pack "ngram ") t
    then (maximum [read . T.unpack . head . T.split (== '=') . T.drop 7 $ t, nOld], xs)
    else if   isAWantedLine t
         then (nOld, t:xs)
         else (nOld, xs)

parseLine
  :: N.NGrams T.Text         -- ^ old NGrams
  -> T.Text                  -- ^ line to read from
  -> N.NGrams T.Text         -- ^ new NGrams
parseLine n t
  = let s1 = T.split (=='\t') t
        p  = read . T.unpack . head $ s1 :: Double
        ws = T.words . head . tail $ s1
        b  = if   L.length s1 == 2
             then Nothing
             else Just (read . T.unpack . last $ s1 :: Double)
    in  N.addNGram n ws p b
