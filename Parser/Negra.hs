module Parser.Negra where

import Parser.ApplicativeParsec
import Data.Char(ord)

data Sentence = Sentence
    { sId :: Int
    , sEditorId :: Int
    , sDate :: String
    , sOriginId :: Int
    , sComment :: Maybe String
    , sData :: [SentenceData]
    }
    deriving Show

data SentenceData = SentenceData
    { sdWord :: String
    , sdPostag :: String
    , sdMorphtag :: String
    , sdEdge :: Edge
    , sdSecEdges :: [Edge]
    , sdComment :: Maybe String
    }
    deriving Show

data Edge = Edge
    { label :: String
    , parent :: Int
    }
    deriving Show


p_negra =
        p_ignoreLines
     *> p_format
     *> many p_table
     *> many p_Sentence
    <*  eof


p_format =  -- TODO stub
        try (string "#FORMAT")
     *> manyTill anyChar newline
    <*  p_ignoreLines


p_table =  -- TODO stub
        try (string "#BOT")
     *> manyTill anyChar (try (string "#EOT"))
    <*  manyTill anyChar newline
    <*  p_ignoreLines


p_Sentence =
        try (string "#BOS")
     *> negraSpaces1
     *> (p_Int >>= \num ->
            Sentence num
        <$> p_Int
        <*> p_date
        <*> p_Int
        <*> optionMaybe p_comment
        <*  negraNewline
        <*> manyTill p_SentenceData (try (string "#EOS"))
        <*  negraSpaces1
        <*  string (show num)
        <*  negraNewline
        )
    <*  p_ignoreLines


p_SentenceData =
        SentenceData
    <$> p_word
    <*  p_word  -- TODO: #FORMAT 4 seems to contain the lemma here
    <*> p_word
    <*> p_word
    <*> p_Edge
    <*> manyTill p_Edge (lookAhead (negraNewline <|> string "%%" *> return '%'))
    <*> optionMaybe p_comment
    <*  negraNewline
    <*  p_ignoreLines


p_Edge = Edge <$> p_word <*> p_Int


p_comment = string "%%" *> negraSpaces *> many (noneOf "\n")

p_emptyLine = negraSpaces *> negraNewline

p_ignoreLines = many p_ignoreLine

p_ignoreLine = try p_emptyLine *> return ()
           <|> try p_comment *> negraNewline *> return ()

p_word = many1 negraNonSpace <* tokenCleanup

p_Int :: GenParser Char st Int
p_Int = (many1 digit >>= return . read) <* tokenCleanup


p_date = many1 (oneOf "/0123456789") <* tokenCleanup

tokenCleanup = negraSpaces1 *> return () <|> lookAhead newline *> return ()

negraSpace   = oneOf " \t"
negraSpaces  = skipMany negraSpace
negraSpaces1 = skipMany1 negraSpace

negraNonSpace = satisfy $ (<) 32 . ord

-- negraNonSpace = satisfy $
--     \c -> let o = ord c in o >= 33 && o <= 127 || o >= 160 && o <= 255

negraNewline = char '\n'

-- loadHGraph file = parseFromFile p_WTA file
