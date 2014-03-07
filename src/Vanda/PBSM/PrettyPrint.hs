-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.PBSM.PrettyPrint
-- Copyright   :  (c) Technische Universität Dresden 2014
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.PBSM.PrettyPrint where


import Vanda.PBSM.Types

import Data.List (intercalate)
import Data.Tree
import qualified Data.Set as S


type SForest a = S.Set (Tree a)
type NT = SForest String
type T = String


drawRTG :: RTG NT T -> String
drawRTG g
  = unlines
  $ ("initials: " ++ intercalate ", " (map drawNT (initials g)))
  : map drawRule (rules g)


drawRule :: Rule NT T -> String
drawRule r
  = drawNT (lhs r)
  ++ " -> "
  ++ lab r
  ++ if null (succs r)
     then ""
     else "("
          ++ intercalate ", " (map drawNT (succs r))
          ++ ")"


drawNT :: NT -> String
drawNT
  = ('{' :)
  . (++ "}")
  . intercalate ", "
  . map drawTreeAsTerm
  . S.toList


drawTreeAsTerm :: Tree T -> String
drawTreeAsTerm (Node x [])
  = x
drawTreeAsTerm (Node x ts)
  = x ++ "(" ++ intercalate ", " (map drawTreeAsTerm ts) ++ ")"


drawDerivation :: Tree (NT, Either (Tree T) T) -> String
drawDerivation = drawTree . fmap drawNode
  where
    drawNode (nt, t) = drawNT nt ++ ": " ++ either drawTreeAsTerm id t
