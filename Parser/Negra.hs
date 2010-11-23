module Parser.Negra where

import ApplicativeParsec
import Control.Monad

data Sentence = Sentence
    { sId :: Int
    , sEditorId :: Int
    , sDate :: String
    , sOriginId :: Int
    , sComment :: Maybe String
    , sData :: [String]
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
    string "#BOS" >>
    Sentence
    <$> (spaces1 *> p_Int)
    <*> (spaces1 *> p_Int)
    <*> (spaces1 *> p_date)
    <*> (spaces1 *> p_Int)
    <*> optionMaybe (spaces1 *> string "%%" *> spaces *> many (noneOf "\n"))
    <*  spaces
    <*  newline
    <*> (many undefined)
    <*  string "#EOS"
    <*  newline

p_SentenceData =
    SentenceData
    <$> manyTill anyChar space
    <*> (spaces >> nonSpaces)
    <*> (spaces >> nonSpaces)
    <*> p_Edge
    <*> many p_Edge
    -- <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

p_Edge = Edge
    <$> (spaces >> nonSpaces)
    <*> (spaces >> p_Int)


p_Int :: GenParser Char st Int
p_Int = many1 digit >>= return . read


p_date = many1 $ oneOf "/0123456789"

spaces1 = skipMany1 space

nonSpaces = manyTill anyChar space

-- loadHGraph file = parseFromFile p_WTA file
