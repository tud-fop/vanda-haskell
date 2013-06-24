-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Grammar.NGrams.WTA
-- Copyright   :  (c) Technische Universität Dresden 2013
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Tobias.Denkinger@mailbox.tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Queries an Automaton that represents an n-gram model.
--
-----------------------------------------------------------------------------


module Vanda.Grammar.NGrams.WTA 
  ( emptyNState
  , mkNState
  , mergeNState
  , mergeNStates
  , NState (Unary, Binary)
  ) where

import Vanda.Grammar.LM
import Data.Hashable
import Data.List (intercalate)

data NState v
  = Unary  [v]
  | Binary [v] [v]
  deriving (Eq, Ord)

instance Hashable i => Hashable (NState i) where
  hashWithSalt s (Unary a) = s `hashWithSalt` a
  hashWithSalt s (Binary a b) = s `hashWithSalt` a `hashWithSalt` b

instance Show v => Show (NState v) where
  show (Unary x)
    = intercalate "_" . map (dropFirstLast . show) $ x
  show (Binary x y)
    = (intercalate "_" . map (dropFirstLast . show) $ x) ++ "*" ++ (intercalate "_" . map (dropFirstLast . show) $ y) 

dropFirstLast
  :: [x]
  -> [x]
dropFirstLast = drop 1 . reverse . drop 1 . reverse

emptyNState :: NState i
emptyNState = Unary []

mkNState
  :: LM a
  => a
  -> [Int]
  -> (NState Int, Double)
mkNState lm s
  = let n = order lm
    in  if   n - 1 <= (length s)
        then ( Binary (take (n - 1) s) (last' (n - 1) s)
             , score lm s
             )
        else (Unary s, 0)

mergeNState
  :: LM a
  => a
  -> (NState Int, Double)
  -> NState Int
  -> (NState Int, Double)
mergeNState lm (Unary s1, w1) (Unary s2)
  = (\ (x, w2) -> (x, w1 + w2))
  . mkNState lm
  $ (s1 ++ s2)
mergeNState lm (Unary s1, w1) (Binary s2 s3)
  = ( Binary (take ((order lm) - 1) (s1 ++ s2)) s3
    , w1 + (score lm  (s1 ++ s2))
    )
mergeNState lm (Binary s1 s2, w1) (Unary s3)
  = ( Binary s1 (last' ((order lm) - 1) (s2 ++ s3))
    , w1 + (score lm  (s2 ++ s3))
    )
mergeNState lm (Binary s1 s2, w1) (Binary s3 s4)
  = ( Binary (take ((order lm) - 1) (s1 ++ s2))
             (last' ((order lm) - 1) (s3 ++ s4))
    , w1 + (score lm (s1 ++ s2))
         + (score lm (s3 ++ s4))
    )

mergeNStates
  :: LM a
  => a
  -> [NState Int]
  -> (NState Int, Double)
mergeNStates lm (x:xs)
  = foldl (mergeNState lm) (x, 0) xs
mergeNStates _ []
  = (Unary [], 0)

last' :: Int -> [v] -> [v]
last' n xs = drop ((length xs) - n) xs