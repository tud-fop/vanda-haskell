{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Corpus.SExpression
-- Description :  lazy parsing of s-expressions
-- Copyright   :  (c) Technische Universität Dresden 2014
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Lazy parsing of s-expressions.
--
-- The results must be evaluated in depth-first, left-to-right order,
-- otherwise you will suffer from the effects of too much laziness.
--
-- To achieve laziness, any parse error results in a call to 'error'.
-----------------------------------------------------------------------------

module Vanda.Corpus.SExpression
( SExpression(..)
, -- * Conversion
  toTree
, toForest
, -- * Parsing
  parse
, parseFromFile
, parseFromFiles
, pSExpression
, pSExpressions
) where


import Control.Applicative
import Control.Monad.State.Lazy
import Data.Char
import Data.Tree
import System.IO.Unsafe (unsafeInterleaveIO)
import Text.Parsec.Error
import Text.Parsec.Pos


moduleName :: String
moduleName = "Vanda.Corpus.SExpression"


errorHere :: String -> String -> a
errorHere fun msg = error $ moduleName ++ "." ++ fun ++ ": " ++ msg


data SExpression = Atom { sExpPos :: SourcePos, sExpAtom :: String }
                 | List { sExpPos :: SourcePos, sExpList :: [SExpression] }
                 deriving Show


-- | Lazily convert an 'SExpression' to a 'Tree'.
--
-- An 'SExpression' represents a 'Tree', if it is an 'Atom' or it is a 'List'
-- of an 'Atom' and at least one 'SExpression' representing a 'Tree'.
toTree :: SExpression -> Tree String
toTree (Atom _ s) = Node s []
toTree (List _ (Atom _ s : xs@(_ : _))) = Node s (map toTree xs)
toTree x = errorHere "toTree"
         $ show $ newErrorMessage (Expect "tree") (sExpPos x)


-- | Lazily convert an 'SExpression' to a 'Forest'.
--
-- An 'SExpression' represents a forest, if it is a 'List' of 'SExpression's
-- representing a 'Tree', respectively.
toForest :: SExpression -> Forest String
toForest (List _ xs) = map toTree xs
toForest x = errorHere "toForest"
           $ show $ newErrorMessage (Expect "forest") (sExpPos x)


data S = S
  { sPos   :: !SourcePos
  , sInput :: String
  } deriving Show

type Parser a = State S a


class FinallySeqable a where
  seqFinally :: b -> a -> a

instance FinallySeqable Char where
  seqFinally x c = x `seq` c

instance FinallySeqable a => FinallySeqable [a] where
  seqFinally x ys@[]    = x `seq` ys
  seqFinally x (y : ys) = y : seqFinally x ys

instance FinallySeqable SExpression where
  seqFinally x (Atom src atom) = Atom src $ seqFinally x atom
  seqFinally x (List src ys  ) = List src $ seqFinally x ys


-- | Lazily apply a parser to a 'String'.
parse :: FinallySeqable a => Parser a -> SourceName -> String -> a
parse p src input
  = let (output, s) = runState p $ S (initialPos src) input
    in s `seqFinally` output
    -- since we parse lazily, we have to force the state in the end,
    -- otherwise we may miss a parsing error


-- | Lazily apply a parser to a file.
parseFromFile :: FinallySeqable a => Parser a -> FilePath -> IO a
parseFromFile p file
  =   parse p file
  <$> readFile file


-- | Lazily apply a parser to several files using 'parseFromFile'.
--
-- The result must be /fully/ evaluated from left to right, otherwise you may
-- get too many open files.
parseFromFiles :: FinallySeqable a => Parser a -> [FilePath] -> IO [a]
parseFromFiles p = unsafeLazySequence . map (parseFromFile p)


unsafeLazySequence :: [IO a] -> IO [a]
unsafeLazySequence ms = unsafeInterleaveIO $ foldr step (return []) ms
  where
    step m m' = do
      x  <- m
      xs <- unsafeInterleaveIO m'
      return (x : xs)


-- | Parser for a single s-expression.
pSExpression :: Parser SExpression
pSExpression  = pSpace *> pSExpression'  <* pEOF

pSExpression' :: Parser SExpression
pSExpression' = do
  c <- peek
  if c /= '('
    then Atom <$> gets sPos <*> pAtom
    else pChar '(' *> pSpace
      *> (List <$> gets sPos <*> pSExpressions')
      <* pChar ')' <* pSpace


-- | Parser for several consecutive s-expressions (different from a 'List',
-- because there are no outer parentheses).
pSExpressions :: Parser [SExpression]
pSExpressions = pSpace *> pSExpressions' <* pEOF

pSExpressions' :: Parser [SExpression]
pSExpressions' = do
  cM <- peekMaybe
  case cM of
    Nothing           -> return []
    Just c | c == ')' -> return []
    _ -> do x  <- pSExpression'
            xs <- pSExpressions'
            return (x : xs)


pAtom :: Parser String
pAtom = do
  atom <- takeWhileS (\ c -> c /= '(' && c /= ')' && not (isSpace c))
  case atom of
    [] -> gets sPos >>= parseError (Expect "atom")
    cs -> pSpace >> return cs


pSpace :: Parser ()
pSpace = void (takeWhileS isSpace)


pEOF :: Parser ()
pEOF = do
  s <- get
  if null (sInput s)
    then return ()
    else parseError (Expect "end of input") (sPos s)


pChar :: Char -> Parser Char
pChar c = do
  c' <- pop
  if c' == c
    then return c'
    else gets sPos
     >>= parseError (Expect $ show c ++ ", but got " ++ show c')


peek :: Parser Char
peek = get >>= \ s -> case sInput s of
  (x : _) -> return x
  []      -> parseError (UnExpect "end of input") (sPos s)


peekMaybe :: Parser (Maybe Char)
peekMaybe = get >>= \ s -> case sInput s of
  (x : _) -> return (Just x)
  []      -> return Nothing


pop :: Parser Char
pop = popMaybe
  >>= maybe (gets sPos >>= parseError (UnExpect "end of input")) return


popMaybe :: Parser (Maybe Char)
popMaybe = get >>= \ s -> case sInput s of
  (x : xs) -> do put $! S (updatePosChar (sPos s) x) xs
                 return (Just x)
  []       -> do return Nothing


takeWhileS :: (Char -> Bool) -> Parser [Char]
takeWhileS p = do
  s <- get
  -- let (result, Position l' c', i') = spanAccum p advance (Position l c) i
  let (prefix, rest) = span p (sInput s)
  put $! S (updatePosString (sPos s) prefix) rest
  return prefix

{-
spanAccum :: (a -> Bool) -> (a -> acc -> acc) -> acc -> [a] -> ([a], acc, [a])
spanAccum p f = go
  where
    go !acc xs@[] = (xs, acc, xs)
    go !acc xs@(x : xs')
      | p x       = let (ys, acc', zs) = go (f x acc) xs' in (x : ys, acc', zs)
      | otherwise = ([], acc, xs)


advance :: Char -> Position -> Position
advance '\n' (Position l _) = Position (l + 1) 1
advance _    (Position l c) = Position l (c + 1)
-}

parseError :: Message -> SourcePos -> a
parseError msg sourcePos
  = error $ moduleName ++ ": " ++ show (newErrorMessage msg sourcePos)
