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


module Parser.NegraLazy (parseNegra, readFileLatin1, module Data.Negra) where

import Data.Negra
import Tools.Miscellaneous (mapFst)

import qualified Data.Char as C
import qualified Data.List as L
import System.Environment (getArgs)
import System.IO


data S = S [(Int, String)]

currentLine :: S -> Maybe String
currentLine (S ((_, x) : _)) = Just x
currentLine _ = Nothing

-- currentLineNumber :: S -> Int
-- currentLineNumber (S ((n, _) : _)) = n
-- currentLineNumber s = errorS s "Unexpected end of file"

nextState :: S -> S
nextState (S (_ : xs)) = S xs
nextState s = errorS s "Unexpected end of file"

-- hasNext :: S -> Bool
-- hasNext (S (_ : _)) = True
-- hasNext _ = False


errorS :: S -> [Char] -> a
errorS (S ((n, _) : _)) cs
  = error $ "Parse error on line " ++ show n ++ ": " ++ cs
errorS _ cs
  = error $ "Parse error: " ++ cs


parseNegra :: String -> [Sentence]
parseNegra cs
  = parseLines (error "Parse error: Negra format version not defined")
  . S
  . filter (\ (_, l) -> not (all C.isSpace l || L.isPrefixOf "%%" l))
  . zip [1 ..]
  . lines
  $ cs


parseLines :: (S -> ([SentenceData], S)) -> S -> [Sentence]
parseLines parseSentence s
  = case fmap wordsComment $ currentLine s of
      Nothing ->
        []
      Just (["#FORMAT", version], _) ->
        case version of
          "3" -> parseLines parseSentenceV3 $ nextState s
          "4" -> parseLines parseSentenceV4 $ nextState s
          _   -> errorS s "Unknown Negra format version"
      Just ("#BOT" : _, _) ->
        parseLines parseSentence $ parseTable $ nextState s
      Just (["#BOS", num, editorId, date, originId], comment) ->
        let (sd, s') = parseSentence $ nextState s
        in Sentence (read num) (read editorId) date (read originId) comment sd
        : parseLines parseSentence s'
      _ ->
        errorS s "Expected #FORMAT, #BOT or #BOS"


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


wordsComment :: String -> ([String], Maybe String)
wordsComment s
  = case dropWhile {-partain:Char.-}C.isSpace s of
      "" -> ([], Nothing)
      '%' : '%' : cs -> ([], Just cs)
      s' -> mapFst (w :) $ wordsComment s''
            where (w, s'') =
                    break {-partain:Char.-}C.isSpace s'


-- dropWords :: Int -> String -> String
-- dropWords 0 s = dropWhile C.isSpace s
-- dropWords n s
--   = case dropWhile C.isSpace s of
--       "" -> []
--       s' -> dropWords (n - 1) $ dropWhile (not . C.isSpace) s'


readFileLatin1 :: FilePath -> IO String
readFileLatin1 name = do
  h <- openFile name ReadMode
  hSetEncoding h latin1
  hGetContents h


main :: IO ()
main
  =   getArgs
  >>= readFileLatin1 . head
  >>= \ x -> let y = parseNegra x
      in {-rnf y `seq`-} print (length y)