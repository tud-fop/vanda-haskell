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
-----------------------------------------------------------------------------

module Vanda.Grammar.NGrams.Text
  ( parseNGrams
  ) where

import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Vanda.Grammar.NGrams as N

parseNGrams
  :: T.Text                  -- Text to parse
  -> N.NGrams T.Text Double  -- generated NGrams
parseNGrams t
  = L.foldl' parseLine N.empty
  . L.filter isWantedLine
  . T.lines

isAWantedLine
  :: T.Text                  -- line to check
  -> Bool                    -- true iff the line contains an NGram
isAWantedLine l
  = not
  . or
  . map (\ f. f l)
  . [ T.isPrefixOf "\\" , T.isPrefixOf "ngram ", T.null ]

parseLine
  :: N.NGrams T.Text Double  -- old NGrams
  -> T.Text                  -- line to read from
  -> N.NGrams T.Text Double  -- new NGrams
parseLine n t
  = let s1 = T.split (=="\t") t
        p  = read . head $ s1 :: Double
        ws = T.words . head . tail $ s1
        b  = if   L.length == 2
             then Nothing
             else Just (read . last $ s1 :: Double)
    in  N.addNGram n ws p b
