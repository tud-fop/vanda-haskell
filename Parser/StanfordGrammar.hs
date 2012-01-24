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
import qualified Data.Binary   as B
import           Data.ByteString.Lazy (ByteString)
import           Data.Char     (ord)
import qualified Data.IntMap   as IntMap
import qualified Data.List     as L
import           Data.Ord      (comparing)
import qualified Data.Tree     as T
import           Data.Word     (Word8)
import           Text.Parsec   hiding (many, (<|>))
import           Text.Parsec.String

-- import Debug.Trace

--p_emptyline = spaces >> newline >> return ()

p_block s p = string ("BEGIN " ++ s) >> p `manyTill` (try $ lookAhead $ string "BEGIN")


p_grammar :: GenParser Char st (String, String)
p_grammar = do
  (,) <$> (p_block "OPTIONS" anyChar
           *> p_block "STATE_INDEX" anyChar
           *> p_block "WORD_INDEX" anyChar
           *> p_block "TAG_INDEX" anyChar
           *> p_block "LEXICON" anyChar
           *> p_block "UNARY_GRAMMAR" anyChar)
           <*> p_block "BINARY_GRAMMAR" anyChar

