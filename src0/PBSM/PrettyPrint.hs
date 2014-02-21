module PBSM.PrettyPrint where


import PBSM.Types

import Data.List (intercalate)
import Data.Tree
import qualified Data.Set as S


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
