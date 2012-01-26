-- (c) 2012 Johannes Osterholzer <Johannes.Osterholzer@tu-dresden.de>
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

module Parser.StanfordGrammar where


import Tools.Miscellaneous(mapFst, mapSnd)

import           Control.Applicative
import           Control.DeepSeq
import           Text.Parsec   hiding (many, (<|>))
import           Text.Parsec.String

-- import Debug.Trace

-- Main grammar parser, parses whole file
p_grammar :: GenParser Char st ([(String, String, Double)], [(String, String, String, Double)])
p_grammar =
  (,) <$> (p_block "OPTIONS" p_options
      *>  p_block "STATE_INDEX" p_stateIndex
      *>  p_block "WORD_INDEX" p_wordIndex
      *>  p_block "TAG_INDEX" p_tagIndex
      *>  p_block "LEXICON" p_lexicon
      *>  many p_smooth
      *>  p_block "UNARY_GRAMMAR" p_unaryGrammar)
      <*> p_block "BINARY_GRAMMAR" p_binaryGrammar


-- Parses top level blocks
p_block :: String -> GenParser Char st a -> GenParser Char st [a]
p_block s p = string ("BEGIN " ++ s)
              *> newline
              *> many (p <* newline)
              <* spaces

p_smooth = string "smooth" >> many (noneOf "\n") >> newline >> return ()

-- The following parsers each parse one line of the corresponding block
p_options :: GenParser Char st String
p_options = many1 (noneOf "\n")

p_wordIndex :: GenParser Char st (String, String)
p_wordIndex = (,) <$> many1 digit
                  <*> (char '=' *> many1 (noneOf "\n"))

p_tagIndex = p_wordIndex

p_stateIndex = p_wordIndex

p_lexicon = (,,,) <$> (p_quotedIdent <* p_arr)
                  <*> (p_quotedIdent <* spaces1)
                  <*> (p_seen <* spaces1)
                  <*> p_num

p_unaryGrammar :: GenParser Char st (String, String, Double)
p_unaryGrammar = (,,) <$> (p_quotedIdent <* p_arr)
                      <*> (p_quotedIdent <* spaces1)
                      <*> p_num

p_binaryGrammar :: GenParser Char st (String, String, String, Double)
p_binaryGrammar = (,,,) <$> (p_quotedIdent <* p_arr)
                        <*> (p_quotedIdent <* spaces1)
                        <*> (p_quotedIdent <* spaces1)
                        <*> p_num

-- Auxiliary parsers
spaces1 :: GenParser Char st ()
spaces1 = space >> spaces

p_seen = (string "SEEN" >> return True) <|> (string "UNSEEN" >> return False)

p_num :: (Read a, Floating a) => GenParser Char st a  -- not really sure about the typeclass
p_num = many (digit <|> oneOf ".-E") >>= return . read

p_arr = spaces >> string "->" >> spaces >> return ()

p_quotedIdent = char '"' *> many1 p_quotedChar <* char '"'

p_quotedChar =
  noneOf "\"\\"
  <|> try (char '\\' >> ((char '"' *> return '"') <|> (char '\\' *> return '\\')))
  -- <|> try (string "\\\\" >> return '\\')



test = parseFromFile p_grammar "/home/gdp/oholzer/build/stanford-parser-2012-01-06/gram.txt"
test3 = parseFromFile p_grammar "/home/gdp/oholzer/build/stanford-parser-2012-01-06/gram3.txt"
