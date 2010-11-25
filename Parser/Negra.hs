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


p_Sentence =
    (p_ignoreLines *> string "#BOS")
    >>  Sentence
    <$> (negraSpaces1 *> p_Int)
    <*> (negraSpaces1 *> p_Int)
    <*> (negraSpaces1 *> p_date)
    <*> (negraSpaces1 *> p_Int)
    <*> optionMaybe (negraSpaces1 *> p_comment)
    <*  negraNewline
    <*> manyTill p_SentenceData (try (p_ignoreLines *> string "#EOS"))
    <*  (negraSpaces1 *> p_Int)
    <*  negraNewline


p_SentenceData =
    p_ignoreLines
    >>  SentenceData
    <$> p_word
    <*> (negraSpaces1 *> p_word)
    <*> (negraSpaces1 *> p_word)
    <*> p_Edge
    <*> manyTill p_Edge (try $ lookAhead $ (negraSpaces *> negraNewline <|> negraSpaces1 *> string "%%" *> negraSpace))
    <*> optionMaybe (negraSpaces1 *> p_comment)
    <*  negraNewline


p_Edge = Edge
    <$> (negraSpaces >> p_word)
    <*> (negraSpaces >> p_Int)


p_comment = string "%%" *> negraSpaces1 *> many (noneOf "\n")

p_emptyLine = negraSpaces *> negraNewline

p_ignoreLines = many p_ignoreLine

p_ignoreLine = try p_emptyLine *> return ()
           <|> try p_comment   *> return ()

p_word = many1 negraNonSpace

p_Int :: GenParser Char st Int
p_Int = many1 digit >>= return . read


p_date = many1 $ oneOf "/0123456789"

negraSpace   = oneOf " \t"
negraSpaces  = skipMany negraSpace
negraSpaces1 = skipMany1 negraSpace

negraNonSpace = satisfy $ (<) 32 . ord

-- negraNonSpace = satisfy $
--     \c -> let o = ord c in o >= 33 && o <= 127 || o >= 160 && o <= 255

negraNewline = char '\n'

-- loadHGraph file = parseFromFile p_WTA file
