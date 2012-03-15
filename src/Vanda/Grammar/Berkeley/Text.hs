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

import Control.Applicative ( (<|>), many )
import Control.Arrow ( first )
import Control.DeepSeq ( NFData, ($!!) )
import Data.Int ( Int32 )
import qualified Data.Text.Lazy as T
import Text.Parsec hiding ( many, (<|>) )
import Text.Parsec.Text.Lazy

import Vanda.Hypergraph.Basic ( Hyperedge, mkHyperedge )
import Vanda.Hypergraph.NFData ()

parseBerkeleyMap
  :: (NFData v, NFData l, NFData ul, NFData uv)
  => (ul -> String -> (ul, l))-- ^ symbol mapping function, e.g. 'updateToken'
  -> ul                       -- ^ initial token structure
  -> (uv -> String -> (uv, v))-- ^ node mapping function, e.g. 'updateToken'
  -> uv                       -- ^ initial token structure
  -> T.Text                   -- ^ lexicon file
  -> T.Text                   -- ^ grammar file
  -> ((ul, uv), [(Hyperedge v l Int32, Double)])
                              -- ^ resulting token structure and hyperedges
parseBerkeleyMap mapl ul0 mapv uv0 lx gr = (snd iu2, concat es1 ++ es2)
  where
    (iu1, es1) = lazyMany (p_lexicon mapl mapv) "lexicon" (0, (ul0, uv0)) lx
    (iu2, es2) = lazyMany (p_grammar mapl mapv) "grammar" iu1 gr

lazyMany :: NFData a => GenParser u a -> SourceName -> u -> T.Text -> (u, [a])
lazyMany p file ustate contents
  = either (error . show) id $ runParser mp ustate file contents
  where
    mp = do
      xs <- many p
      u <- getState
      return $! (u, id $!! xs)
{-lazyMany p file ustate contents = lm state0
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
                -- let rs = second (x:) $ lm state'
                -- rs `seq` return rs
                return $ second (x:) $ lm state'
            ]
-}

p_mapl
  :: (ul -> String -> (ul, l))
  -> String
  -> GenParser (Int32, (ul, uv)) l
p_mapl mapper !s = do
  (i, (ul, uv)) <- getState
  let (u', l) = mapper ul s
  let ist = (u', uv)
  let st = (i, ist)
  ist `seq` st `seq` setState st
  return l

p_mapv
  :: (uv -> String -> (uv, v))
  -> String
  -> GenParser (Int32, (ul, uv)) v
p_mapv mapper !s = do
  (i, (ul, uv)) <- getState
  let (u', v) = mapper uv s
  let ist = (ul, u')
  let st = (i, ist)
  ist `seq` st `seq` setState st 
  return v

p_grammar
  :: (NFData v, NFData l)
  => (ul -> String -> (ul, l))
  -> (uv -> String -> (uv, v))
  -> GenParser (Int32, (ul, uv)) (Hyperedge v l Int32, Double)
p_grammar mapl mapv
  = do
    { lhss <- many1 $ noneOf "_"
    ; _ <- char '_'
    ; lhsi <- many1 $ oneOf "1234567890"
    ; symbol <- p_mapl mapl lhss
    ; lhs <- p_mapv mapv $!! lhss ++ "_" ++ lhsi
    ; spaces; _ <- string "->"; spaces
    ; rhs1 <- p_mapv mapv =<< many1 (noneOf " ")
    ; spaces
    ; i <- fmap fst getState
    ; updateState (first (+1))
    ; do
      { wgt <- p_weight
      ; spaces
      ; return $!! (mkHyperedge lhs [rhs1] symbol i, wgt)
      }
    <|> do
        { rhs2 <- p_mapv mapv =<< (return $!!) =<< many1 (noneOf " ")
        ; spaces
        ; wgt <- p_weight
        ; spaces
        ; return $!! (mkHyperedge lhs [rhs1, rhs2] symbol i, wgt)
        }
    }

p_lexicon
  :: (NFData v, NFData l)
  => (ul -> String -> (ul, l))
  -> (uv -> String -> (uv, v))
  -> GenParser (Int32, (ul, uv)) [(Hyperedge v l Int32, Double)]
p_lexicon mapl mapv
  = do
    { lhs <- many1 $ noneOf " "
    ; spaces
    ; rhs <- p_mapl mapl =<< many1 (noneOf " ")
    ; spaces
    ; _ <- char '['
    ; wgts <- sepBy p_weight (string ", ")
    ; _ <- char ']'
    ; spaces
    ; sequence $!
      [ do
        { lhs' <- p_mapv mapv $!! lhs ++ "_" ++ show i
        ; i' <- fmap fst getState
        ; updateState (first (+1))
        ; return $!! (mkHyperedge lhs' [] rhs i', wgt) 
        }
      | (i, wgt) <- zip [(0::Int)..] wgts
      ]
    }

p_weight :: GenParser u Double
p_weight
  = do
    { c <- oneOf "0123456789"
    ; cs <- many (oneOf "0123456789.E-")
    ; return $! read $! (c:cs)
    }
