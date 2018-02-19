-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Matthias Büchse, Toni Dietze 2011
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

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
, mapTails
, eMapHeadTail
, eMapVertices
, eMapId
, mapVertices
, mapVerticesMonotonic
, eMapLabel
, mapLabels
, eMapWeight
, mapWeights
, mapWeights'
, mapIds
, mapAccumIds
-- * Weight Manipulation
, properize
, randomizeWeights
-- * Simplification
, dropUnreachables
, dropZeroWeighted
, verticesToInt
, binarize
, binarize'
-- * Parsing
, parseTree
, nBest
, nBest'
-- * Pretty Printing
, drawHypergraph
, drawHyperedge
) where


import qualified Algorithms.NBest as NBest
import qualified Data.Queue as Q
import Tools.FastNub (nub)
import Tools.Miscellaneous (mapFst, mapSnd, sumWith, mapRandomR)

import Control.DeepSeq
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified System.Random as R

-- ---------------------------------------------------------------------------

data Hyperedge v l w i = Hyperedge
  { eHead   :: v
  , eTail   :: [v]
  , eLabel  :: l
  , eWeight :: w
  , eId     :: i
  } deriving (Read, Show)

data Hypergraph v l w i = Hypergraph
  { -- | Get a 'S.Set' of all vertices of a 'Hypergraph'.
    verticesS :: S.Set v
  , -- | Get a 'M.Map' containigng all 'Hyperedge's of the given 'Hypergraph'.
    -- The 'M.Map' maps a vertex to a list of 'Hyperedge's which have this
    -- vertex as head vertex.
    edgesM    :: M.Map v [Hyperedge v l w i]
  } deriving (Read, Show)

-- ---------------------------------------------------------------------------

-- | Create a 'Hypergraph' from a list of 'Hyperedge's.
hypergraph :: (Ord v) => [Hyperedge v l w i] -> Hypergraph v l w i
hypergraph es
  = Hypergraph
      (S.fromList . concatMap (\ e ->  eHead e : eTail e) $ es)
      (M.fromListWith (++) . map (\ e -> (eHead e, [e]))  $ es)


-- | Create a 'Hypergraph' from a 'M.Map' mapping a vertex to all
-- 'Hyperedge's which have this vertex as head.
-- /The precondition is not checked!/
hypergraphM :: (Ord v) => M.Map v [Hyperedge v l w i] -> Hypergraph v l w i
hypergraphM eM
  = Hypergraph
      ( S.fromList
      . concatMap (\ e ->  eHead e : eTail e)
      . concat
      $ M.elems eM
      )
      eM


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


-- | Apply a function to the tails of the 'Hyperedge's of a 'Hypergraph'.
mapTails
  :: (Ord v)
  => ([v] -> [v]) -> Hypergraph v l w i -> Hypergraph v l w i
mapTails f = hypergraphM . M.map (map (eMapTail f)) . edgesM


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


-- | Apply a function to the label of a 'Hyperedge'.
eMapLabel :: (l -> l') -> Hyperedge v l w i -> Hyperedge v l' w i
eMapLabel f = \ e -> e{eLabel = f (eLabel e)}


-- | Apply a function to the labels of all 'Hyperedge's.
mapLabels :: (Ord v) => (l -> l') -> Hypergraph v l w i -> Hypergraph v l' w i
mapLabels f
  = hypergraphM
  . fmap (map (eMapLabel f))
  . edgesM


-- | Apply a function to the weight of a 'Hyperedge'.
eMapWeight :: (w -> w') -> Hyperedge v l w i -> Hyperedge v l w' i
eMapWeight f e = e{eWeight = f (eWeight e)}


-- | Apply a function to the weights of all 'Hyperedge's.
mapWeights :: (w -> w') -> Hypergraph v l w i -> Hypergraph v l w' i
mapWeights f g = g{edgesM = M.map (map (eMapWeight f)) (edgesM g)}


-- | Apply a function to all 'Hyperedge's resulting in a new weight,
-- respectively.
mapWeights'
  :: (Hyperedge v l w i -> w') -> Hypergraph v l w i -> Hypergraph v l w' i
mapWeights' f g = g{edgesM = M.map (map (\ e -> e{eWeight = f e})) (edgesM g)}


-- | Apply a function to a 'Hyperedge''s id.
eMapId :: (i -> i') -> Hyperedge v l w i -> Hyperedge v l w i'
eMapId f = \ e -> e{eId = f (eId e)}


-- | Apply a function to the ids of all 'Hyperedge's.
mapIds :: (i -> i') -> Hypergraph v l w i -> Hypergraph v l w i'
mapIds f g = g{edgesM = M.map (map (eMapId f)) (edgesM g)}


-- | Alter 'Hyperedge' ids while accumulating a value.
-- The traversal order is undefined.
mapAccumIds
  :: (a -> i -> (a, i'))
  -> a
  -> Hypergraph v l w i
  -> (a, Hypergraph v l w i')
mapAccumIds f accStart g
  = (accEnd, g{edgesM = eM'})
  where
    (accEnd, eM') = M.mapAccum (L.mapAccumL f') accStart (edgesM g)
    f'  acc e = let (acc', i) = f acc (eId e)
                in (acc', e{eId = i})

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
-- random number in the range @(1, 1+r)@.
randomizeWeights
  :: (Num w, R.Random w, R.RandomGen gen)
  => w -> Hypergraph v l w i -> gen -> (Hypergraph v l w i, gen)
randomizeWeights r g gen = mapWeightsRandomR (1, 1+r) (*) g gen


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
    flipSwap f' x y = let (y', x') = f' y x in (x', y')

-- ---------------------------------------------------------------------------

-- | Remove all unreachable vertices and 'Hyperedge's which use unreachable
-- vertices.
dropUnreachables :: (Ord v) => v -> Hypergraph v l w i -> Hypergraph v l w i
dropUnreachables target g
  = hypergraphM
  . M.mapMaybe (\ (b, val) -> if b then Just val else Nothing)
  $ go (Q.singleton target) (M.map ((,) False) $ edgesM g)
  where
    go q m
      | Q.null q = m
      | otherwise =
        let (v, q') = Q.deq q
        in case M.lookup v m of
          Nothing -> go q' m
          Just (True, _) -> go q' m
          Just (_, es) ->
            go
              (Q.enqList (concatMap eTail es) q')
              (M.adjust (mapFst $ const True) v m)


-- | Remove all zero weighted 'Hyperedge's from a 'Hypergraph'.
dropZeroWeighted
  :: (Ord v, Eq w, Num w) => Hypergraph v l w i -> Hypergraph v l w i
dropZeroWeighted g
  = hypergraphM
  . M.mapMaybe
      ( \es ->
        let es' = filter ((0 /=) . eWeight) es
        in if null es'
          then Nothing
          else Just es'
      )
  $ edgesM g


-- | Map the vertices of a 'Hypergraph' to distinct 'Int's where the given
-- vertex is mapped to 0.
-- Returns a pair of the vertex mapping and the new 'Hypergraph'.
verticesToInt
  :: (Ord v) => v -> Hypergraph v l w i -> (M.Map v Int, Hypergraph Int l w i)
verticesToInt target g
  = let (_, vM) = mapSnd (M.insert target 0)
                $ mapSnd M.fromDistinctAscList
                $ L.mapAccumL (\ (i : is) v -> (is, (v, i))) [1 ..]
                $ S.toAscList
                $ verticesS g
    in {-i `seq`-}
    ( vM
    , mapVertices  -- mapVerticesMonotonic  -- TODO: optimize
        ( \ v -> M.findWithDefault
          (error "Data.Hypergraph.verticesToInt: This should not happen ...")
          v
          vM
        )
        g
    )


-- | Right-binarize a 'Hypergraph', i.e. replace every
-- @hyperedge v [v1, ..., vn] l w i@ by
-- @hyperedge [v] [[v1, ..., vn]] (Just l) w ()@ and
-- @hyperedge [vi, ..., vn] [[vi], [v(i+1), ..., vn]] Nothing 1 ()@
-- for every @i@ in @[1 .. n - 1]@.
binarize
  :: (Ord v, Num w) => Hypergraph v l w i -> Hypergraph [v] (Maybe l) w ()
binarize g
  =   hypergraph
  $   [ Hyperedge [v] (if null vs then [] else [vs]) (Just l) w ()
      | Hyperedge v vs l w _ <- edges g
      ]
  ++  [ Hyperedge vvs [[v], vs] Nothing 1 ()
      | vvs@(v:vs@(_:_))
          <- nub $ concatMap (L.tails . eTail) $ edges g
      ]


-- | Left-binarize a 'Hypergraph', i.e. replace every
-- @hyperedge v [v1, ..., vn] l w i@ by
-- @hyperedge [v] [[vn, ..., v1]] (Just l) w ()@ and
-- @hyperedge [vi, ..., v1] [[v(i-1), ..., v1], [vi]] Nothing 1 ()@
-- for every @i@ in @[2 .. n]@.
binarize'
  :: (Ord v, Num w) => Hypergraph v l w i -> Hypergraph [v] (Maybe l) w ()
binarize' g
  =   hypergraph
  $   [ Hyperedge [v] (if null vs then [] else [reverse vs]) (Just l) w ()
      | Hyperedge v vs l w _ <- edges g
      ]
  ++  [ Hyperedge vvs [vs, [v]] Nothing 1 ()
      | vvs@(v:vs@(_:_))
          <- nub $ concatMap (L.tails . reverse . eTail) $ edges g
      ]

-- ---------------------------------------------------------------------------

-- | Creates a 'Hypergraph' which represents the given 'T.Tree' based on the
-- given 'Hypergraph'. The resulting 'Hypergraph' is empty, iff no execution
-- of the given 'Hypergraph' represents the given 'T.Tree'.
parseTree
  :: (Ord v, Ord l)
  => v -> T.Tree l -> Hypergraph v l w i -> Hypergraph (v, [Int]) l w i
parseTree target t g
  = let (m, _) = parseTree' [] target look t M.empty
    in dropUnreachables (target, [])
    $ hypergraphM
        ( M.fromAscList
        . map (\ ((v, pos, _), val) -> ((v, pos), val))
        . filter (not . null . snd)
        $ M.toAscList m
        )
  where
    look v l n
      = maybe [] (filter $ \ e -> eLabel e == l && length (eTail e) == n)
      . M.lookup v
      $ edgesM g

parseTree'
  :: (Ord v, Ord t)
  => [Int]
  -> v
  -> (v -> t -> Int -> [Hyperedge v l w i])
  -> T.Tree t
  ->  M.Map (v, [Int], t) [Hyperedge (v, [Int]) l w i]
  -> (M.Map (v, [Int], t) [Hyperedge (v, [Int]) l w i], Bool)
parseTree' pos target look (T.Node l ts) m
  = maybe
      ((,) mNext . null . fromJust $ M.lookup key mNext)
      ((,) m . null)
      (M.lookup key m)
  where
    key = (target, pos, l)
    target' = (target, pos)
    mNext
      = for (M.insert key [] m) (look target l (length ts)) $ \ m' e ->
          let (m'', b) = checkChildren m' (zip3 (eTail e) [(1 :: Int) ..] ts)
              e' = eMapHeadTail
                      (const target')
                      (flip zip $ map (: pos) [1 ..])
                      e
          in if b then m'' else M.insertWith (++) key [e'] m''
    checkChildren m' [] = (m', False)
    checkChildren m' ((v, n, t) : xs)
      = let (m'', isNull) = parseTree' (n : pos) v look t m'
        in if isNull then (m'', True) else checkChildren m'' xs
    for x ys f = L.foldl' f x ys


-- | @nBest n vertex hypergraph@ computes the weights of the @n@ best
-- executions of @hypergraph@ beginning at @vertex@, where best means the
-- executions with the highest weights with respect to 'Ord'.
nBest :: (Num w, Ord v, Ord w) => Int -> v -> Hypergraph v l w i -> [w]
nBest n target g
  = NBest.worst h target n
  where (h, _) = nBestHelper g


-- | @nBest' n vertex hypergraph@ computes the @n@ best executions and their
-- weights of @hypergraph@ beginning at @vertex@, where best means the
-- executions with the highest weights with respect to 'Ord'.
nBest'
  :: (Num w, Ord v, Ord w)
  => Int -> v -> Hypergraph v l w i -> [(T.Tree (Hyperedge v l w i), w)]
nBest' n target g
  = map (mapFst (fmap (ieA A.!)))
  $ NBest.worst' h target n
  where (h, ieA) = nBestHelper g


nBestHelper
  :: (Num w, Ord v)
  => Hypergraph v l w i
  -> ( ([v], v -> [(Int, [v])], Int -> [w] -> w)
     , A.Array Int (Hyperedge v l w i)
     )
nBestHelper g
  = (h, ieA)
  where
    ieA = A.listArray (0, length (edges g) - 1) $ edges g
    hBackM
      = M.fromListWith (++)
      $ map (\ (i, e) -> (eHead e, [(i, eTail e)]))
      $ A.assocs ieA
    hWeightA = fmap eWeight ieA
    h = ( vertices g
        , \ v -> M.findWithDefault [] v hBackM
        , \ i ws -> product ws * (hWeightA A.! i)
        )

-- ---------------------------------------------------------------------------

-- | Pretty print a 'Hyperedge'.
drawHyperedge :: (Show v, Show l, Show w, Show i) => Hyperedge v l w i -> String
drawHyperedge (Hyperedge hd tl l w i)
  = show hd
  ++ " -- " ++ show tl
  ++ " | "  ++ show l
  ++ " | "  ++ show w
  ++ " | "  ++ show i


-- | Pretty print a 'Hypergraph'.
drawHypergraph :: (Show v, Show l, Show w, Show i) => Hypergraph v l w i -> String
drawHypergraph g
  =   (unlines . map drawHyperedge . edges $ g)
  ++  show (length $ vertices g) ++ " vertices; "
  ++  show (length $ edges g) ++ " edges\n"

-- ---------------------------------------------------------------------------

instance
  (NFData v, NFData l, NFData w, NFData i)
  => NFData (Hyperedge v l w i) where
  rnf (Hyperedge hd tl l w i)
    = rnf hd `seq` rnf tl `seq` rnf l `seq` rnf w `seq` rnf i


instance
  (NFData v, NFData l, NFData w, NFData i)
  => NFData (Hypergraph v l w i) where
  rnf (Hypergraph vS eM) = rnf vS `seq` rnf eM
