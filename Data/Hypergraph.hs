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
, eMapHead
, eMapTail
, eMapHeadTail
, eMapVertices
, mapVertices
, mapVerticesMonotonic
, eMapWeight
, mapWeights
-- * Weight Manipulation
, properize
, randomizeWeights
-- * Parsing
, parseTree
-- * Pretty Printing
, drawHypergraph
, drawHyperedge
) where


import Tools.Miscellaneous (sumWith, mapRandomR)

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Random as R

-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------

-- | Get a list of all vertices of a 'Hypergraph'. The list contains only one
-- instance of equal vertices, respectively.
vertices :: Hypergraph v l w i -> [v]
vertices = S.toList . verticesS


-- | Get a list of all 'Hyperedge's of a 'Hypergraph'.
edges :: Hypergraph v l w i -> [Hyperedge v l w i]
edges = concat . M.elems . edgesM

-- ---------------------------------------------------------------------------

-- | Apply a function to a 'Hyperedge''s head.
eMapHead :: (v -> v) -> Hyperedge v l w i -> Hyperedge v l w i
eMapHead f = \ e -> e{eHead = f (eHead e)}


-- | Apply a function to a 'Hyperedge''s tail.
eMapTail :: ([v] -> [v]) -> Hyperedge v l w i -> Hyperedge v l w i
eMapTail f = \ e -> e{eTail = f (eTail e)}


-- | Apply two functions to a 'Hyperedge''s head and tail, respectively.
eMapHeadTail
  :: ( v  ->  v' )
  -> ([v] -> [v'])
  -> Hyperedge v  l w i
  -> Hyperedge v' l w i
eMapHeadTail f g = \ e -> e{eHead = f (eHead e), eTail = g (eTail e)}


-- | Apply a function to a 'Hyperedge''s head and tail vertices.
eMapVertices :: (v -> v') -> Hyperedge v l w i -> Hyperedge v' l w i
eMapVertices f e = e{eHead = f (eHead e), eTail = map f (eTail e)}

{-
eMapAccumLVertices f acc
  = \ e ->
    let (acc', hd : tl) = L.mapAccumL f acc (eHead e : eTail e)
    in (acc', e{eHead = hd, eTail = tl})
-}

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


-- | Apply a function to the weight of a 'Hyperedge'.
eMapWeight :: (w -> w') -> Hyperedge v l w i -> Hyperedge v l w' i
eMapWeight f e = e{eWeight = f (eWeight e)}


-- | Apply a funciton to the weights of all 'Hyperedge's.
mapWeights :: (w -> w') -> Hypergraph v l w i -> Hypergraph v l w' i
mapWeights f g = g{edgesM = M.map (map (eMapWeight f)) (edgesM g)}

-- ---------------------------------------------------------------------------

-- | Make a 'Hypergraph' proper, i.e. the sum of the weights of 'Hyperedge's
-- with the same head vertex is one.
properize :: (Fractional w) => Hypergraph v l w i -> Hypergraph v l w i
properize g
  = let normalize es
          = let s = sumWith eWeight es
            in map (\ e -> e{eWeight = eWeight e / s}) es
    in g{edgesM = M.map normalize (edgesM g)}


-- | @randomizeWeights r g gen@ multiplies every weight of 'Hypergraph' by a
-- random number in the range @(1-r, 1+r)@.
randomizeWeights
  :: (Num w, R.Random w, R.RandomGen gen)
  => w -> Hypergraph v l w i -> gen -> (Hypergraph v l w i, gen)
randomizeWeights r g gen = mapWeightsRandomR (1-r, 1+r) (*) g gen


-- | 'mapRandomR' for the weights of the edges in a 'Hypergraph'.
mapWeightsRandomR
  :: (R.Random r, R.RandomGen gen)
  => (r, r)
  -> (w -> r -> w')
  -> Hypergraph v l w i
  -> gen
  -> (Hypergraph v l w' i, gen)
mapWeightsRandomR range f g gen
  = let (gen', eM) = M.mapAccum h gen (edgesM g)
    in (g{edgesM = eM}, gen')
  where
    h = flipSwap
      $ mapRandomR
          range
          (\e r -> e{eWeight = f (eWeight e) r})
    flipSwap f x y = let (y', x') = f y x in (x', y')

-- ---------------------------------------------------------------------------

-- | Creates a 'Hypergraph' which represents the given 'T.Tree' based on the
-- given 'Hypergraph'. The resulting 'Hypergraph' is empty, iff no execution
-- of the given 'Hypergraph' represents the given 'T.Tree'.
parseTree
  :: (Ord v, Eq l)
  => v -> T.Tree l -> Hypergraph v l w i -> Hypergraph (v, [Int]) l w i
parseTree target t g
  = let eM = parseTree' [] target look t
    in Hypergraph
        ( S.fromList
          . concatMap (\ e ->  eHead e : eTail e)
          . concat
          . M.elems
          $ eM
        )
        eM
  where
    look v l n
      = maybe [] (filter $ \ e -> eLabel e == l && length (eTail e) == n)
      . M.lookup v
      $ edgesM g


-- This implementation is terrible. If anyone has a better idea, feel free to
-- improve it.
-- Things to keep in mind (things which make the implementation ugly):
--    * calculate stuff for subtrees only once
--    * output edges only once
--    * don't calculate unused stuff (be lazy)
parseTree'
  :: (Ord v)
  => [Int]
  -> v
  -> (v -> l -> Int -> [Hyperedge v l w i])
  -> T.Tree l
  -> M.Map (v, [Int]) [Hyperedge (v, [Int]) l w i]
parseTree' pos target look (T.Node l [])
  = let target' = (target, pos)
    in case look target l 0 of
      [] -> M.empty
      xs -> M.singleton target'
            $ map (eMapHeadTail (const target') (const [])) xs
parseTree' pos target look (T.Node l ts)
  = (\ (accEs, accM) -> M.fold M.union (M.singleton target' accEs) accM)
  $ L.foldl'
      (\ acc@(accEs, accM) e ->
        let xs = map (\ (m, n, v) -> ((n, v), fromJust $ M.lookup v m))
               $ zip3 ms [1 ..] (eTail e)
        in if any (M.null . snd) xs
        then acc
        else
          ( eMapHeadTail
              (const target')
              (flip zip $ map (: pos) [1 ..]) e
            : accEs
          , L.foldl' (flip $ uncurry M.insert) accM xs
          )
      )
      ([], M.empty)
      es
  where
    target' = (target, pos)
    es = look target l (length ts)
    ms = map
           (\ (n, t, vs) ->
               M.fromList
             $ map (\ v -> (v, parseTree' (n : pos) v look t)) vs
           )
       $ zip3 [1 ..] ts
       $ L.transpose
       $ map eTail es

-- ---------------------------------------------------------------------------

-- | Pretty print a 'Hyperedge'.
drawHyperedge :: (Show v, Show l, Show w) => Hyperedge v l w i -> String
drawHyperedge (Hyperedge h t l w i)
  = show h ++ " -- " ++ show t ++ " | " ++ show l ++ " | " ++ show w


-- | Pretty print a 'Hypergraph'.
drawHypergraph :: (Show v, Show l, Show w) => Hypergraph v l w i -> String
drawHypergraph g = unlines . map drawHyperedge . edges $ g
