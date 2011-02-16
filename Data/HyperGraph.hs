module Data.HyperGraph (
-- * Types
  HyperEdge()
, HyperGraph()
-- * Construction
, hyperEdge
, hyperGraph
-- * Decomposition
-- ** Decomposition of 'HyperEdge's
, eHead, eTail, eLabel, eWeight
-- ** Decomposition of 'HyperGraph's
, edges, edgesM
, vertices, verticesS
-- * Pretty Printing
, drawHyperGraph, drawHyperEdge
) where


import qualified Data.Map as M
import qualified Data.Set as S


data HyperEdge v l w = HyperEdge
  { eHead   :: v
  , eTail   :: [v]
  , eLabel  :: l
  , eWeight :: w
  }

data HyperGraph v l w = HyperGraph
  { -- | Get a 'S.Set' of all vertices of a 'HyperGraph'.
    verticesS :: S.Set v
  , -- | Get a 'M.Map' containigng all 'HyperEdge's of the given 'HyperGraph'.
    -- The 'M.Map' maps a vertex to a list of 'HyperEdge's which have this
    -- vertex as head vertex.
    edgesM    :: M.Map v [HyperEdge v l w]
  }


-- | Create a 'HyperGraph' from a list of 'HyperEdge's.
hyperGraph :: (Ord v) => [HyperEdge v l w] -> HyperGraph v l w
hyperGraph es
  = HyperGraph
      (S.fromList . concatMap (\ e ->  eHead e : eTail e) $ es)
      (M.fromListWith (++) . map (\ e -> (eHead e, [e]))  $ es)


-- | Create a 'HyperEdge'.
hyperEdge
  :: v    -- ^ head vertex
  -> [v]  -- ^ tail vertices
  -> l    -- ^ label
  -> w    -- ^ weight
  -> HyperEdge v l w
hyperEdge = HyperEdge


-- | Get a list of all vertices of a 'HyperGraph'. The list contains only one
-- instance of equal vertices, respectively.
vertices :: HyperGraph v l w -> [v]
vertices = S.toList . verticesS


-- | Get a list of all 'HyperEdge's of a 'HyperGraph'.
edges :: HyperGraph v l w -> [HyperEdge v l w]
edges = concat . M.elems . edgesM


-- | Pretty print a 'HyperEdge'.
drawHyperEdge :: (Show v, Show l, Show w) => HyperEdge v l w -> String
drawHyperEdge (HyperEdge h t l w)
  = show h ++ " -- " ++ show t ++ " | " ++ show l ++ " | " ++ show w


-- | Pretty print a 'HyperGraph'.
drawHyperGraph :: (Show v, Show l, Show w) => HyperGraph v l w -> String
drawHyperGraph g = unlines . map drawHyperEdge . edges $ g
