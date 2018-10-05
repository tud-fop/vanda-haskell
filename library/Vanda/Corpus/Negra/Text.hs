-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Toni Dietze 2010
-- License     :  BSD-style
--
-- Maintainer  :  Toni Dietze <Toni.Dietze@tu-dresden.de>
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Corpus.Negra.Text
  ( parseNegra
  ) where

import Control.Arrow ( first )
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text.Lazy as T

import Vanda.Corpus.Negra


newtype S = S [(Int, String)]


currentLine :: S -> Maybe String
currentLine (S ((_, x) : _)) = Just x
currentLine _ = Nothing


nextState :: S -> S
nextState (S (_ : xs)) = S xs
nextState s = errorS s "Unexpected end of file"


errorS :: S -> String -> a
errorS (S ((n, _) : _)) cs
  = error $ "Parse error on line " ++ show n ++ ": " ++ cs
errorS _ cs
  = error $ "Parse error: " ++ cs


parseNegra :: T.Text -> Negra
parseNegra
  = parseLines
    ( Context
    $ error "Parse error: Negra format version not defined"
    )
  . S
  . filter (\ (_, l) -> not (all C.isSpace l || L.isPrefixOf "%%" l))
  . zip [1 ..]
  . map T.unpack
  . T.lines


newtype Context
  = Context
    { parseSentence :: S -> ([SentenceData], S)
    }


parseLines :: Context -> S -> Negra
parseLines c s
  = case fmap wordsComment $ currentLine s of
      Nothing ->
        Negra [] []
      Just (["#FORMAT", version], _) ->
        case version of
          "3" -> parseLines c{parseSentence = parseSentenceV3} $ nextState s
          "4" -> parseLines c{parseSentence = parseSentenceV4} $ nextState s
          _   -> errorS s "Unknown Negra format version"
      Just (["#BOT", "WORDTAG"], _) ->
        parseTableWordtag c $ nextState s
      Just ("#BOT" : _, _) ->
        parseLines c $ parseTable $ nextState s
      Just (["#BOS", num, editorId, date, originId], comment) ->
        let (sd, s') = parseSentence c $ nextState s
        in alter
            (Sentence (read num) (read editorId) date (read originId) comment sd :)
            (parseLines c s')
      _ ->
        errorS s "Expected one of: (optionally followed by %% ⟨comment⟩)\n\
                 \\t#FORMAT ⟨num⟩\n\
                 \\t#BOT WORDTAG\n\
                 \\t#BOT …\n\
                 \\t#BOS ⟨num⟩ ⟨editor id⟩ ⟨date⟩ ⟨origin id⟩"
  where
    alter f ~(Negra ws ss) = Negra ws (f ss)


parseTableWordtag :: Context -> S -> Negra
parseTableWordtag c s
  = case fmap (wordsN 3) $ currentLine s of
      Just ["#EOT", "WORDTAG"] ->
        alter (const []) $ parseLines c $ nextState s
      Just [tagId, tag, bound, descr] ->
        let bound' = case bound of
                      "Y" -> True
                      "N" -> False
                      _   -> errorS s "Expected Y or N"
        in alter (WordTag (read tagId) tag bound' descr :)
        $ parseTableWordtag c $ nextState s
      _ ->
        errorS s "Expected #EOT WORDTAG or wordtag table data"
  where
    alter f ~(Negra ws ss) = Negra (f ws) ss


parseTable :: S -> S
parseTable s
  = case fmap words $ currentLine s of
      Just ("#EOT" : _) -> nextState s
      _ -> parseTable $ nextState s


parseSentenceV3 :: S -> ([SentenceData], S)
parseSentenceV3 s
  = case fmap wordsComment $ currentLine s of
      Just (w : postag : morphtag : edge : parent : ws, comment) ->
        let (sd, s') = parseSentenceV3 (nextState s)
            ctor = case w of
                    ['#']    -> SentenceWord w
                    '#' : cs -> SentenceNode (read cs)
                    _        -> SentenceWord w
        in  ( ctor
                postag
                morphtag
                (Edge edge (read parent))
                (parseSecEdges ws s)
                comment
              : sd
            , s'
            )
      Just ("#EOS" : _, _) ->
        ([], nextState s)
      _ ->
        errorS s "Expected #EOS or sentence data in Negra format version 3"


parseSentenceV4 :: S -> ([SentenceData], S)
parseSentenceV4 s
  = case fmap wordsComment $ currentLine s of
      Just (w : _{-TODO-} : postag : morphtag : edge : parent : ws, comment) ->
        let (sd, s') = parseSentenceV4 (nextState s)
            ctor = case w of
                    ['#']    -> SentenceWord w
                    '#' : cs -> SentenceNode (read cs)
                    _        -> SentenceWord w
        in  ( ctor
                postag
                morphtag
                (Edge edge (read parent))
                (parseSecEdges ws s)
                comment
              : sd
            , s'
            )
      Just ("#EOS" : _, _) ->
        ([], nextState s)
      _ ->
        errorS s "Expected #EOS or sentence data in Negra format version 4"


parseSecEdges :: [String] -> S -> [Edge]
parseSecEdges [] _
  = []
parseSecEdges (l : p : ws) s
  = Edge l (read p) : parseSecEdges ws s
parseSecEdges _ s
  = errorS s "Expected secondary edge parent"


wordsN :: Int -> String -> [String]
wordsN 0 s = [dropWhile C.isSpace s]
wordsN n s
  = case dropWhile {-partain:Char.-}C.isSpace s of
      "" -> []
      s' -> w : wordsN (n - 1) s''
            where (w, s'') = break {-partain:Char.-}C.isSpace s'


wordsComment :: String -> ([String], Maybe String)
wordsComment s
  = case dropWhile {-partain:Char.-}C.isSpace s of
      "" -> ([], Nothing)
      '%' : '%' : cs -> ([], Just cs)
      s' -> first (w :) $ wordsComment s''
            where (w, s'') =
                    break {-partain:Char.-}C.isSpace s'

