-- (c) 2010-2013 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}  -- for 'Stream'

module Parser.Penn where

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
