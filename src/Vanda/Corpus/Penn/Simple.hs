{-# LANGUAGE FlexibleContexts #-}  -- for 'Stream'

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Corpus.Penn.Simple
-- Copyright   :  (c) Technische Universität Dresden 2010-2014
-- License     :  BSD-style
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Corpus.Penn.Simple where

import Control.Applicative
import qualified Data.Tree as T
import Text.Parsec hiding (many, (<|>))

data Sentence = Node String [Sentence] | Leaf String String deriving Show

-- every p_-function should scan trailing spaces


p_penn :: (Stream s m Char) => ParsecT s u m [Sentence]
p_penn =
        spaces
     *> many (many p_comment *> p_Sentence)
    <*  eof


p_Sentence :: (Stream s m Char) => ParsecT s u m Sentence
p_Sentence =
        char '(' *> spaces
     *> (p_word >>= (\tag -> 
                (Node tag <$> many1 p_Sentence)
            <|> (Leaf tag <$> p_word))
        )
     <* char ')' <* spaces


p_comment :: (Stream s m Char) => ParsecT s u m ()
p_comment =
        lookAhead (string "%%")
     *> manyTill anyChar newline
     *> spaces


p_word :: (Stream s m Char) => ParsecT s u m String
p_word = many (noneOf " ()\t\n\r\f\v") <* spaces


toTree :: Sentence -> T.Tree String
toTree (Node x ts) = T.Node x $ map toTree ts
toTree (Leaf x y ) = T.Node x [T.Node y []]
