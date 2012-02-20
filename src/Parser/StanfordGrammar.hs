-- (c) 2012 Johannes Osterholzer <Johannes.Osterholzer@tu-dresden.de>
-- (c) 2012 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

module Parser.StanfordGrammar where


import Data.WCFG

import Control.Applicative
import Text.Parsec hiding (many, (<|>))
import Text.Parsec.String


parseGrammar
  :: FilePath -> IO (Either ParseError (WCFG String String Double Int))
parseGrammar fp = parseFromFile p_grammar fp


-- Main grammar parser, parses whole file. Note: in this parser, the
-- order of blocks is assumed to be fixed. This could be changed by
-- modifying p_block to select the correct parser after block
-- beginning
p_grammar :: GenParser Char st (WCFG String String Double Int)
p_grammar =
  create <$> (p_block "OPTIONS" p_options
              *>  p_block "STATE_INDEX" p_stateIndex)
         <*> (p_block "WORD_INDEX" p_wordIndex
              *>  p_block "TAG_INDEX" p_tagIndex
              *>  p_block "LEXICON" p_lexicon)
         <*> (many p_smooth -- lolz
              *>  p_block "UNARY_GRAMMAR" p_unaryGrammar)
         <*> p_block "BINARY_GRAMMAR" p_binaryGrammar
  where create _ l u b = wcfg "ROOT" (killCycles l ++ killCycles u ++ b)
        --create s l u b = wcfg (snd . head $ s) (l ++ u ++ b)
        -- note this does disturb the probability distribution
        killCycles = filter (\p -> pRight p /= [Left $ pLeft p])


-- Parses top level blocks
p_block :: String -> GenParser Char st a -> GenParser Char st [a]
p_block s p = string ("BEGIN " ++ s)
              *> newline
              *> many (p <* newline)
              <* spaces


-- throw away smoothing factors loitering about in the middle of the file
p_smooth :: GenParser Char st ()
p_smooth = string "smooth" >> many (noneOf "\n") >> spaces >> return ()


-- The following parsers each parse one line of the corresponding block
p_options :: GenParser Char st String
p_options = many1 (noneOf "\n")


p_wordIndex :: GenParser Char st (Int, String)
p_wordIndex = (,) <$> (read <$> many1 digit)
                  <*> (char '=' *> many1 (noneOf "\n"))

p_tagIndex :: GenParser Char st (Int, String)
p_tagIndex = p_wordIndex


p_stateIndex :: GenParser Char st (Int, String)
p_stateIndex = p_wordIndex


p_lexicon :: GenParser Char st (Production [Char] [Char] Double Int)
p_lexicon = p <$> (p_quotedIdent <* p_arr)
              <*> (p_quotedIdent <* spaces1)
              <*> (p_seen <* spaces1)
              <*> p_num
  where p l ".*." _ w = production l [Left ".*."] w 0
        p l r     _ w = production l [Right r] w 0


p_unaryGrammar :: GenParser Char st (Production String String Double Int)
p_unaryGrammar = p <$> (p_quotedIdent <* p_arr)
                   <*> (p_quotedIdent <* spaces1)
                   <*> p_num
  where p l r w = production l [Left r] (2 ** w) 0 --TODO: make up an ID


p_binaryGrammar :: GenParser Char st (Production String String Double Int)
p_binaryGrammar = p <$> (p_quotedIdent <* p_arr)
                    <*> (p_quotedIdent <* spaces1)
                    <*> (p_quotedIdent <* spaces1)
                    <*> p_num
  where p l r1 r2 w = production l [Left r1, Left r2] (2 ** w) 0


-- Auxiliary parsers
spaces1 :: GenParser Char st ()
spaces1 = space >> spaces


p_seen :: GenParser Char st Bool
p_seen = (string "SEEN" >> return True) <|> (string "UNSEEN" >> return False)


p_num :: GenParser Char st Double  -- tried typeclass first, caused headache
p_num = many (digit <|> oneOf ".-E") >>= return . read


p_arr :: GenParser Char st ()
p_arr = spaces >> string "->" >> spaces >> return ()


p_quotedIdent :: GenParser Char st String
p_quotedIdent = char '"' *> many1 p_quotedChar <* char '"'


p_quotedChar :: GenParser Char st Char
p_quotedChar = noneOf "\\\"" <|> try (char '\\' *> oneOf "\\\"")
