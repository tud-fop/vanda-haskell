module Parser.Penn where

import Parser.ApplicativeParsec

data Sentence = Node String [Sentence] | Leaf String String deriving Show

-- every p_-function should scan trailing spaces


p_penn =
        spaces
     *> many (many p_comment *> p_Sentence)
    <*  eof


p_Sentence =
        char '(' *> spaces
     *> (p_word >>= (\tag -> 
                (Node tag <$> many1 p_Sentence)
            <|> (Leaf tag <$> p_word))
        )
     <* char ')' <* spaces


p_comment =
        lookAhead (string "%%")
     *> manyTill anyChar newline
     *> spaces


p_word = many (noneOf " ()\t\n\r\f\v") <* spaces
