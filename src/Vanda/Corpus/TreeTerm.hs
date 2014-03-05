{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Corpus.TreeTerm
-- Copyright   :  (c) Technische Universität Dresden 2013-2014
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- A "Text.Parsec" parser for trees which are denoted as terms, e.g.
-- @S(NP(NNP(John)), VP(VPZ(loves), NP(NNP(Mary))))@.
--
-----------------------------------------------------------------------------

module Vanda.Corpus.TreeTerm
( parseTree
, parseTrees
, parsecTree
) where


import Control.Applicative
import Control.Monad (void)
import Data.Tree
import Text.Parsec hiding ((<|>), many)


-- | Parse exactly one tree. Throws an exception on a parse error.
parseTree :: String -> Tree String
parseTree cs
  = handleParseError
  $ parse (spaces *> parsecTree <* eof) cs cs


-- | Parse a sequence of trees separated by commas. Throws an exception on a
-- parse error.
parseTrees :: String -> Forest String
parseTrees cs
  = handleParseError
  $ parse (spaces *> parsecTree `sepBy` parsecChar ',' <* eof) cs cs


-- | Extract 'Right' value or terminate with 'error' 'show'ing the 'Left'
-- 'ParseError'.
handleParseError :: Either ParseError a -> a
handleParseError = either (error . show) id


-- ---------------------------------------------------------------------------
-- parsec…-functions shall remove trailing whitespaces
-- ---------------------------------------------------------------------------

-- | Parser for a single tree. Whitespaces before and after symbols and
--  subtrees are ignored.
parsecTree :: Stream s m Char => ParsecT s u m (Tree String)
parsecTree
  = Node
  <$> parsecSymbol
  <*> ( parsecChar '(' *> parsecTree `sepBy` parsecChar ',' <* parsecChar ')'
      <|> return []
      )


-- | Parser for a (terminal) symbol. Leading and trailing spaces are ignored
-- except there is an escaped whitespace. Any symbol can be escaped by a
-- backslash.
parsecSymbol :: Stream s m Char => ParsecT s u m String
parsecSymbol
  = manyTill escapedChar
  $ try $ spaces *> eof <|> lookAhead (void (oneOf "(,)"))


-- | The same as 'char', but removes trailing whitespaces.
parsecChar :: Stream s m Char => Char -> ParsecT s u m Char
parsecChar c = char c <* spaces


-- ---------------------------------------------------------------------------

-- | Parse a character while removing an escaping backslash if necessary.
escapedChar :: Stream s m Char => ParsecT s u m Char
escapedChar = do
  c <- anyChar
  case c of
    '\\' -> anyChar
    _ -> return c
