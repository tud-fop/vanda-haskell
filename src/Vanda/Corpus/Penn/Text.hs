{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Corpus.Penn.Text
-- Copyright   :  (c) Technische Universität Dresden 2012
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Matthias.Buechse@tu-dresden.de, Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Allows parsing and unparsing Penn tree banks using either 'String's or
-- 'Int's for grammar tags and tokens.
--
-----------------------------------------------------------------------------

module Vanda.Corpus.Penn.Text
  ( PennFamily ( parsePenn, unparsePenn, yield )
  , parsePennMap
  ) where

import Control.Applicative ( (<*), (*>), (<|>), (<$>), many )
import Control.Arrow ( (***) )
import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Data.Tree as T
import Data.Int ( Int32 )
import Text.Parsec hiding ( many, (<|>) )
import Text.Parsec.Text.Lazy

class PennFamily t where
  -- | Used in the parsing process to map identifiers to 'String' (via 'id')
  -- or to 'Int' (via 'read')
  fromString :: String -> t
  -- | Used in the unparsing process to map 'String' or 'Int' to String,
  -- using 'id' and 'show', respectively.
  toString :: t -> String
  parsePenn :: T.Text -> [T.Tree t]
  parsePenn = snd . parsePennMap (curry $ id *** fromString) ()
  unparsePenn :: [T.Tree t] -> T.Text
  unparsePenn = T.unlines . map (T.pack . treeToPenn toString)
  yield :: [T.Tree t] -> T.Text
  yield = T.unlines . map (T.pack . treeToYield toString)

instance PennFamily String where
  fromString = id
  toString = id

instance PennFamily Int where
  fromString = read
  toString = show

instance PennFamily Int32 where
  fromString = read
  toString = show

-- | Uses a mapping function to parse identifiers. One possibility is
-- 'Vanda.Token.updateToken'.
parsePennMap :: (u -> String -> (u, b)) -> u -> T.Text -> (u, [T.Tree b])
parsePennMap f ustate contents
  = go ustate $ zip [(0 :: Int)..] (T.lines contents)
  where
    go u [] = (u, [])
    go u ((i, x):xs) =
      let Right (u', x') = runParser p u ("line " ++ show i) x
          (u'', xs') = go u' xs
      in (u'', x':xs')
    p = do
      x' <- p_Sentence (p_tag f)
      u' <- getState
      return (u', x')

-- parsePennMap f
--   = lazyMany (many p_comment *> p_Sentence (p_tag f)) "penn tb"

treeToPenn :: (t -> String) -> T.Tree t -> String
treeToPenn f (T.Node t1 [T.Node t2 []]) = "(" ++ f t1 ++ " " ++ f t2 ++ ")"
treeToPenn f (T.Node t1 ts)
  = "(" ++ f t1 ++ " " ++ unwords (map (treeToPenn f) ts) ++ ")"

treeToYield :: (t -> String) -> T.Tree t -> String
treeToYield f (T.Node t1 []) = f t1
treeToYield f (T.Node _ ts) = L.intercalate " " $ map (treeToYield f) ts

-- every p_-function should scan trailing spaces

{-
lazyMany :: GenParser u a -> SourceName -> u -> T.Text -> (u, [a])
lazyMany p file ustate contents = lm state0
  where
    Right state0 = runParser getParserState ustate file contents
    lm state = case runParser p' undefined "" T.empty of
                    Left err -> error (show err)
                    Right x -> x
      where
        p' = do
          _ <- setParserState state
          choice
            [ do
                eof
                u <- getState
                return (u, [])
            , do
                x <- p
                state' <- getParserState
                return $ second (x:) $ lm state'
            ]
-}

p_Sentence :: GenParser u t -> GenParser u (T.Tree t)
p_Sentence p_word =
          char '(' *> spaces
       *> (p_word >>= (\tag -> 
                  (T.Node tag <$> many1 (p_Sentence p_word))
              <|> (T.Node tag <$> (\ s -> [T.Node s []]) <$> p_word))
          )
       <* char ')' <* spaces

{-
p_comment :: GenParser u ()
p_comment =
        lookAhead (string "%%")
     *> manyTill anyChar newline
     *> spaces
-}

p_tag :: (u -> String -> (u, b)) -> GenParser u b
p_tag lookupToken = do
  name <- many (noneOf " ()\t\n\r\f\v")
  spaces
  m <- getState
  let (m', i) = lookupToken m name
  setState m'
  return i

