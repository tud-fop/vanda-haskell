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
  ( f
  , g
  , NState (Unary, Binary)
  ) where

import Vanda.Grammar.LM
import Data.Hashable
import Data.List (foldl',intercalate)

data NState v
  = Unary  [v]
  | Binary [v] [v]
  deriving (Eq, Ord)

instance Hashable i => Hashable (NState i) where
  hashWithSalt s (Unary a) = s `hashWithSalt` a
  hashWithSalt s (Binary a b) = s `hashWithSalt` a `hashWithSalt` b

instance Show v => Show (NState v) where
  show (Unary x)
    = intercalate "_" . map show $ x
  show (Binary x y)
    = (intercalate "_" . map show $ x) ++ "*" ++ (intercalate "_" . map show $ y) 

-- | helper for state behaviour
f :: LM a => a -> [NState v] -> NState v
f lm xs
  = let go (Unary x) = x
        go (Binary x y) = x ++ y
        str = concatMap go xs
        n = order lm
    in  if   length str < order lm
        then Unary str
        else Binary (take (n - 1) str) (last' (n-1) str)

-- | helper for transition weights
g :: LM a => a -> [NState Int] -> Double
g lm xs
  = let go (rs, p) (Unary x) = (rs, p ++ x)
        go (rs, p) (Binary x y) = (rs ++ [(p ++ x)], y)
        n = order lm
        strs = (\ (rs, p) -> rs ++ [p]) . foldl' go ([], []) $ xs
        strs2 = filter (\ x -> length x >= n) strs
    in  sum . map (score lm) $ strs2


last' :: Int -> [v] -> [v]
last' n xs = drop ((length xs) - n) xs
