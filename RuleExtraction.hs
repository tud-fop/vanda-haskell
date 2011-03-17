-- Copyright (c) 2010, Toni Dietze

module RuleExtraction where

import Data.Hypergraph
import qualified Data.WTA as WTA

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.Tree as T


extractHypergraph
  :: (Fractional w, Ord v) => T.Forest v -> Hypergraph v v w i
extractHypergraph forest
  = properize
  . hypergraph
  . map (\((hd, tl), c) -> hyperedge hd tl hd c undefined)
  . M.toList
  . extract
  $ forest


extractWTA
  :: (Ord q, Fractional w) => T.Forest q -> WTA.WTA q q w
extractWTA forest
  = WTA.properize
  . flip WTA.create []
  . map (\((x, xs), c) -> WTA.Transition x x xs c)
  . M.toList
  . extract
  $ forest


extract :: (Ord a, Num n) => T.Forest a -> M.Map (a, [a]) n
extract forest = M.fromListWith (+) . map (flip (,) 1) . extractF $ forest


extractF :: T.Forest a -> [(a, [a])]
extractF forest = concatMap extractT forest


extractT :: T.Tree a -> [(a, [a])]
extractT (T.Node {T.rootLabel = r, T.subForest = f})
  = (r, map T.rootLabel f) : extractF f
