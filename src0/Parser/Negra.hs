-- (c) 2010-2011 Toni Dietze <Toni.Dietze@tu-dresden.de>
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

module Parser.Negra (p_negra, module Data.Negra) where


import Data.Negra

import           Control.Applicative
import           Control.DeepSeq
import qualified Data.Binary   as B
import           Data.ByteString.Lazy (ByteString)
import           Data.Char     (ord)
import           Data.Word     (Word8)
import           Text.Parsec   hiding (many, (<|>))

-- import Debug.Trace


p_negra :: (Stream s m Char) => ParsecT s u m [Sentence]
p_negra = {-fmap (safeEncode . LazyBinaryList) $-}
        p_ignoreLines
     *> (p_format >>= \format ->
              many p_table
           *> many (p_Sentence (format == 4))
        )
    <*  eof


p_format :: (Stream s m Char) => ParsecT s u m Int
p_format =  -- TODO stub
        try (string "#FORMAT")
     *> negraSpaces1
     *> p_Int
    <*  negraSpaces
    <*  newline
    <*  p_ignoreLines


p_table :: (Stream s m Char) => ParsecT s u m String
p_table =  -- TODO stub
        try (string "#BOT")
     *> manyTill anyChar (try (string "#EOT"))
    <*  manyTill anyChar newline
    <*  p_ignoreLines


p_Sentence :: (Stream s m Char) => Bool -> ParsecT s u m Sentence
p_Sentence lemma =
        {-fmap B.encode
     $-}  try (string "#BOS")
     *> negraSpaces1
     *> (p_Int >>= \num -> {-traceShow num $-}
            Sentence num
        <$> p_Int
        <*> p_date
        <*> p_Int
        <*> optionMaybe p_comment
        <*  negraNewline
        <*> manyTill (p_SentenceData lemma) (try (string "#EOS"))
        <*  negraSpaces1
        <*  string (show num)
        <*  negraNewline
        )
    <*  p_ignoreLines


p_SentenceData :: (Stream s m Char) => Bool -> ParsecT s u m SentenceData
p_SentenceData lemma =
    (       SentenceNode <$> (char '#' *> p_Int)
        <|> SentenceWord <$> p_word
    )
    <*  (if lemma then p_word else return "")  -- TODO: #FORMAT 4 seems to contain the lemma here
    <*> p_word
    <*> p_word
    <*> p_Edge
    <*> manyTill p_Edge (lookAhead (negraNewline <|> string "%%" *> return '%'))
    <*> optionMaybe p_comment
    <*  negraNewline
    <*  p_ignoreLines


p_Edge :: (Stream s m Char) => ParsecT s u m Edge
p_Edge = Edge <$> p_word <*> p_Int


p_comment :: (Stream s m Char) => ParsecT s u m String
p_comment = string "%%" *> negraSpaces *> many (noneOf "\n")


p_emptyLine :: (Stream s m Char) => ParsecT s u m ()
p_emptyLine = negraSpaces *> negraNewline *> return ()


p_ignoreLines :: (Stream s m Char) => ParsecT s u m ()
p_ignoreLines = many p_ignoreLine *> return ()


p_ignoreLine :: (Stream s m Char) => ParsecT s u m ()
p_ignoreLine = try p_emptyLine *> return ()
           <|> try p_comment *> negraNewline *> return ()


p_word :: (Stream s m Char) => ParsecT s u m String
p_word = many1 negraNonSpace <* tokenCleanup


p_Int :: (Stream s m Char) => ParsecT s u m Int
p_Int = fmap read (many1 digit) <* tokenCleanup


p_date :: (Stream s m Char) => ParsecT s u m String
p_date = many1 (oneOf "/0123456789") <* tokenCleanup


tokenCleanup :: (Stream s m Char) => ParsecT s u m ()
tokenCleanup = negraSpaces1 <|> lookAhead newline *> return ()


negraSpace :: (Stream s m Char) => ParsecT s u m Char
negraSpace   = oneOf " \t"


negraSpaces :: (Stream s m Char) => ParsecT s u m ()
negraSpaces  = skipMany negraSpace


negraSpaces1 :: (Stream s m Char) => ParsecT s u m ()
negraSpaces1 = skipMany1 negraSpace


negraNonSpace :: (Stream s m Char) => ParsecT s u m Char
negraNonSpace = satisfy $ (<) 32 . ord

-- negraNonSpace = satisfy $
--     \c -> let o = ord c in o >= 33 && o <= 127 || o >= 160 && o <= 255


negraNewline :: (Stream s m Char) => ParsecT s u m Char
negraNewline = char '\n'

-- loadHGraph file = parseFromFile p_WTA file


-- ---------------------------------------------------------------------------

data BinaryContainer a = BinaryContainer a ByteString

instance Show (BinaryContainer a) where
  show (BinaryContainer _ x) = show x

safeEncode :: (B.Binary a) => a -> BinaryContainer a
safeEncode x = BinaryContainer undefined (B.encode x)

safeDecode :: (B.Binary a) => BinaryContainer a -> a
safeDecode (BinaryContainer _ x) = B.decode x

newtype LazyBinaryList a = LazyBinaryList {unLazyBinaryList :: [a]} deriving Show

instance (B.Binary a) => B.Binary (LazyBinaryList a) where
  put (LazyBinaryList xs) = go xs
    where
      go xs'
        = let maxL = 1
              (ys, zs) = splitAt maxL xs'
              l        = fromIntegral (if null zs then length ys else maxL) :: Word8
              putChunk = B.put l >> mapM_ B.put ys
          in if l /= 0
          then putChunk >> go zs
          else putChunk
  get = fmap LazyBinaryList (go 0)
    where
      go 0 = B.get >>= \ l -> if (l :: Word8) /= 0 then go l else return []
      go l = (:) <$> B.get  <*> go (l - 1)


-- !!! This violates DeepSeq properties  !!!
instance NFData ParseError where
  rnf _ = ()
