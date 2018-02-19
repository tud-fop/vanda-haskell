-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Toni Dietze 2011
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

-- |
-- Maintainer  :  Toni Dietze
-- Stability   :  unbekannt
-- Portability :  portable

module Algorithms.RuleExtraction(
-- * Construction
  extractHypergraph
)where 

import Data.Hypergraph

import qualified Data.Map  as M
import qualified Data.Tree as T



-- | Construct a proper 'Hypergraph' from a 'Forest' of derivation trees.
extractHypergraph
  :: (Fractional w, Ord v) => T.Forest v -> Hypergraph v v w ()
extractHypergraph forest
  = properize
  . hypergraph
  . map (\((hd, tl), c) -> hyperedge hd tl hd c ())
  . M.toList
  . extract
  $ forest

-- | construct a map (key,value)=(edge,weight) representing the edges of a 'Hypergraph' where weight counts the occurences of the rule/edge in the forest of derivation trees.
extract :: (Ord a, Num n) => T.Forest a -> M.Map (a, [a]) n
extract forest = M.fromListWith (+) . map (flip (,) 1) . extractF $ forest

-- | extract all rules (lefthandside,righthandside) from a derivation forest
extractF :: T.Forest a -> [(a, [a])]
extractF forest = concatMap extractT forest

-- | extract all rules (lefthandside,righthandside) from a derivation tree
extractT :: T.Tree a -> [(a, [a])]
extractT (T.Node {T.rootLabel = r, T.subForest = f})
  = (r, map T.rootLabel f) : extractF f
