module Data.Hypergraph (
-- * Types
  Hyperedge()
, Hypergraph()
-- * Construction
, hyperedge
, hypergraph
-- * Decomposition
-- ** Decomposition of 'Hyperedge's
, eHead
, eTail
, eLabel
, eWeight
, eId
-- ** Decomposition of 'Hypergraph's
, edges
, edgesM
, vertices
, verticesS
-- * Pretty Printing
, drawHypergraph
, drawHyperedge
) where


import qualified Data.Map as M
import qualified Data.Set as S


data Hyperedge v l w i = Hyperedge
  { eHead   :: v
  , eTail   :: [v]
  , eLabel  :: l
  , eWeight :: w
  , eId     :: i
  }

data Hypergraph v l w i = Hypergraph
  { -- | Get a 'S.Set' of all vertices of a 'Hypergraph'.
    verticesS :: S.Set v
  , -- | Get a 'M.Map' containigng all 'Hyperedge's of the given 'Hypergraph'.
    -- The 'M.Map' maps a vertex to a list of 'Hyperedge's which have this
    -- vertex as head vertex.
    edgesM    :: M.Map v [Hyperedge v l w i]
  }


-- | Create a 'Hypergraph' from a list of 'Hyperedge's.
hypergraph :: (Ord v) => [Hyperedge v l w i] -> Hypergraph v l w i
hypergraph es
  = Hypergraph
      (S.fromList . concatMap (\ e ->  eHead e : eTail e) $ es)
      (M.fromListWith (++) . map (\ e -> (eHead e, [e]))  $ es)


-- | Create a 'Hyperedge'.
hyperedge
  :: v    -- ^ head vertex
  -> [v]  -- ^ tail vertices
  -> l    -- ^ label
  -> w    -- ^ weight
  -> i    -- ^ identifier
  -> Hyperedge v l w i
hyperedge = Hyperedge


-- | Get a list of all vertices of a 'Hypergraph'. The list contains only one
-- instance of equal vertices, respectively.
vertices :: Hypergraph v l w i -> [v]
vertices = S.toList . verticesS


-- | Get a list of all 'Hyperedge's of a 'Hypergraph'.
edges :: Hypergraph v l w i -> [Hyperedge v l w i]
edges = concat . M.elems . edgesM


-- | Pretty print a 'Hyperedge'.
drawHyperedge :: (Show v, Show l, Show w) => Hyperedge v l w i -> String
drawHyperedge (Hyperedge h t l w i)
  = show h ++ " -- " ++ show t ++ " | " ++ show l ++ " | " ++ show w


-- | Pretty print a 'Hypergraph'.
drawHypergraph :: (Show v, Show l, Show w) => Hypergraph v l w i -> String
drawHypergraph g = unlines . map drawHyperedge . edges $ g
