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
-- * Map
, eMapVertices
, mapVertices
, mapVerticesMonotonic
-- * Weight Manipulation
, properize
-- * Pretty Printing
, drawHypergraph
, drawHyperedge
) where


import Tools.Miscellaneous (sumWith)

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


-- | Apply a function to an edge's head and tail vertices.
eMapVertices :: (v -> v') -> Hyperedge v l w i -> Hyperedge v' l w i
eMapVertices f e = e{eHead = f (eHead e), eTail = map f (eTail e)}


-- | Apply a function to all vertices in a 'Hypergraph'.
mapVertices
  :: (Ord v') => (v -> v') -> Hypergraph v l w i -> Hypergraph v' l w i
mapVertices f
  = hypergraph
  . map (eMapVertices f)
  . edges


-- | Apply a function @f@ to all vertices in a 'Hypergraph'.
-- The function must preserve Ordering with respect to 'compare', i.e.
-- @compare x y == compare (f x) (f y)@.
-- /The precondition is not checked./
-- Note that this precondition is a bit stricter than the precondition of
-- 'S.mapMonotonic' for 'S.Set's.
mapVerticesMonotonic
  :: (Ord v') => (v -> v') -> Hypergraph v l w i -> Hypergraph v' l w i
mapVerticesMonotonic f (Hypergraph vs es)
  = Hypergraph
      (S.mapMonotonic f vs)
      ( M.fromAscList
      . map (\ (k, v) -> (f k, map (eMapVertices f) v))
      . M.toAscList
      $ es
      )


-- | Make a Hypergraph proper, i.e. the sum of the weights of edges with the
-- same head vertex is one.
properize :: (Fractional w) => Hypergraph v l w i -> Hypergraph v l w i
properize g
  = let normalize es
          = let s = sumWith eWeight es
            in map (\ e -> e{eWeight = eWeight e / s}) es
    in g{edgesM = M.map normalize (edgesM g)}


-- | Pretty print a 'Hyperedge'.
drawHyperedge :: (Show v, Show l, Show w) => Hyperedge v l w i -> String
drawHyperedge (Hyperedge h t l w i)
  = show h ++ " -- " ++ show t ++ " | " ++ show l ++ " | " ++ show w


-- | Pretty print a 'Hypergraph'.
drawHypergraph :: (Show v, Show l, Show w) => Hypergraph v l w i -> String
drawHypergraph g = unlines . map drawHyperedge . edges $ g
