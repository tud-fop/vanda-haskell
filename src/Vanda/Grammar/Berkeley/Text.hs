-- (c) 2012 Linda Leuschner <Leuschner.Linda@mailbox.tu-dresden.de>
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

module Vanda.Grammar.Berkeley.Text where

import Control.Applicative ( (<*>), (<*), (*>), (<|>), (<$>), many )
import Control.Arrow ( second )
import qualified Data.Text.Lazy as T
import Data.Int ( Int8 )
import qualified Data.IntMap as IM
import Text.Parsec hiding ( many, (<|>) )
import Text.Parsec.Text.Lazy

import Vanda.Hypergraph.Basic ( Hyperedge, mkHyperedge )

parseBerkeleyMap
  :: (u -> String -> (u, a))  -- ^ token mapping function, e.g. 'updateToken'
  -> u                        -- ^ initial token structure
  -> T.Text                   -- ^ lexicon file
  -> T.Text                   -- ^ grammar file
  -> (u, [(Hyperedge (a, Int8) (Maybe a) Int, Double)])
                              -- ^ resulting token structure and hyperedges
parseBerkeleyMap mapper u0 lex gr = (snd iu2, concat es1 ++ es2)
  where
    (iu1, es1) = lazyMany (p_lexicon mapper) "lexicon" (0, u0) lex
    (iu2, es2) = lazyMany (p_grammar mapper) "grammar" iu1 gr

lazyMany :: GenParser u a -> SourceName -> u -> T.Text -> (u, [a])
lazyMany p file ustate contents = lm state0
  where
    Right state0 = runParser getParserState ustate file contents
    lm state = case runParser p' undefined "" T.empty of
                    Left err -> error (show err)
                    Right x -> x
      where
        p' = do
          setParserState state
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

p_mwrapper
  :: (u -> String -> (u, a))
  -> GenParser (Int, u) String
  -> GenParser (Int, u) a
p_mwrapper mapper p = p >>= p_mapper mapper

p_mapper
  :: (u -> String -> (u, a))
  -> String
  -> GenParser (Int, u) a
p_mapper mapper s = do
  (i, u) <- getState
  let (u', a) = mapper u s
  setState (i, u')
  return a

p_grammar
  :: (u -> String -> (u, a))
  -> GenParser (Int, u) (Hyperedge (a, Int8) (Maybe a) Int, Double)
p_grammar mapper
  = let mwrapper = p_mwrapper mapper
    in do
    { lhs <- mwrapper $ many1 $ noneOf "_"
    ; char '_'
    ; lhsi <- many1 $ oneOf "1234567890"
    ; spaces; string "->"; spaces
    ; rhs1 <- mwrapper $ many1 $ noneOf "_"
    ; char '_'
    ; rhs1i <- many1 $ oneOf "1234567890"
    ; spaces
    ; (i, u) <- getState
    ; setState (i+1, u)
    ; do
      { wgt <- p_weight
      ; spaces
      ; return
          ( mkHyperedge (lhs, read lhsi) [(rhs1, read rhs1i)] Nothing i
          , wgt
          )
      }
    <|> do
        { rhs2 <- mwrapper $ many1 $ noneOf "_"
        ; char '_'
        ; rhs2i <- many1 $ oneOf "1234567890"
        ; spaces
        ; wgt <- p_weight
        ; spaces
        ; return
            ( mkHyperedge
                (lhs, read lhsi)
                [(rhs1, read rhs1i), (rhs2, read rhs2i)]
                Nothing
                i
            , wgt
            )
        }
    }

p_weight :: GenParser u Double
p_weight
  = do
    { c <- oneOf "0123456789"
    ; cs <- many (oneOf "0123456789.E-")
    ; return $ read (c:cs)
    }

p_lexicon
  :: (u -> String -> (u, a))
  -> GenParser (Int, u) [(Hyperedge (a, Int8) (Maybe a) Int, Double)]
p_lexicon mapper
  = let mwrapper = p_mwrapper mapper
        pmapper = p_mapper mapper
    in do
    { lhs <- mwrapper $ many1 $ noneOf " "
    ; spaces
    ; rhs <- mwrapper $ many1 $ noneOf " "
    ; spaces
    ; char '['
    ; wgts <- sepBy p_weight (string ", ")
    ; char ']'
    ; spaces
    ; (i', u) <- getState
    ; setState (i' + length wgts, u)
    ; return [ (mkHyperedge (lhs, i) [] (Just rhs) i'', wgt)
             | (i, wgt) <- zip [0..] wgts
             , let i'' = i' + fromIntegral i
             ] 
    }

     {-; let lists = unzip
            [ ((i'', wgt), mkHyperedge (lhs, i) [] (Just rhs) i'')
            | (i, wgt) <- zip [0..] wgts
            , let i'' = i'+ (fromIntegral i)
            ]
    ; setState (i' + length wgts, IM.union im $ IM.fromList (fst lists), u)
    ; return (snd lists)-}

