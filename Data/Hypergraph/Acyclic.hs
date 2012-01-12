-- (c) 2011 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

-- | In the following documentation the term /edge tree/ refers to a 'Tree' of
-- the type @Tree [Hyperedge v l w i]@. An edge tree represents a 'Hypergraph'
-- with the following 'Hyperedge's:
--
-- @[hyperedge (q, p) [(q1, p ++ [1]), ..., (qn, p ++ [n])] l w i | @
-- the 'Tree' contains @(hyperedge q [q1, ..., qn] l w i)@ at position @p]@.
module Data.Hypergraph.Acyclic where

import Data.Hypergraph
import Tools.Miscellaneous (mapSnd)

-- import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tree

{-
import qualified Algorithms.InsideOutsideWeights as IO
import qualified Parser.NegraLazy as N
import TestData.TestHypergraph

import Control.DeepSeq
import Data.Traversable (Traversable, mapAccumL)
import Debug.Trace
import System.Environment (getArgs)


findWithDefault3
  :: (Ord k2, Ord k3)
  => a -> (IM.Key, k2, k3) -> IM.IntMap (M.Map k2 (M.Map k3 a)) -> a
findWithDefault3 def (k1, k2, k3)
  =  M.findWithDefault def     k3
  .  M.findWithDefault M.empty k2
  . IM.findWithDefault M.empty k1


fromListWith3
  :: (Ord k2, Ord k3)
  => (a -> a -> a)
  -> [((IM.Key, k2, k3), a)]
  -> IM.IntMap (M.Map k2 (M.Map k3 a))
fromListWith3 f
  = IM.map (M.map (M.fromListWith f) . M.fromListWith (++))
  . IM.fromListWith (++)
  . map (\ ((k1, k2, k3), x) -> (k1, [(k2, [(k3, x)])]))


foo :: (Ord l, Ord v) => Hypergraph v l w i -> IM.IntMap (M.Map l (M.Map v [Hyperedge v l w i]))
foo g = fromListWith3 (++) [ ((length (eTail e), eLabel e, eHead e), [e]) | e <- edges g ]
-}

-- | Intersect a 'Hypergraph' with a 'Tree'. The result represents all
-- executions of the given 'Hypergraph' beginning at any of the given target
-- nodes and resulting in the given 'Tree'.
--
-- Instead of passing the 'Hypergraph' directly, a hyperedge lookup function
-- is required.
--
-- A naive construction would result in a 'Hypergraph' with vertices which are
-- pairs of vertices of the given 'Hypergraph' and positions of the given
-- 'Tree'.
-- This construction ommits the construction of many useless 'Hyperedge's.
-- Moreover, the resulting 'Hypergraph' is represented by an edge tree, which
-- reuses the given 'Hyperedge's.
edgeTree
  :: (Ord v, Eq l)
  => (Int -> l -> M.Map v [Hyperedge v l w i])  -- ^ hyperedge lookup function
  -> [v]                                        -- ^ target vertices
  -> Tree l                                     -- ^ 'Tree' to be parsed
  -> Tree [Hyperedge v l w i]                   -- ^ output edge tree
edgeTree f targets tree = goT tree targets
  where
    goT (Node l ts) qs
      = let eM = f (length ts) l
            (es, ts') = goF ts
                      $ map (\ e -> (eTail e, e))
                      $ concatMap (\ q -> M.findWithDefault [] q eM) qs
        in Node es ts'
    goF [] xs = (map snd xs, [])
    goF (t : ts) xs
      = let t' = goT t $ L.nub $ map (head . fst) xs
            vS = S.fromList $ map eHead $ rootLabel t'
            fltr [] = []
            fltr ((v : vs, e) : ys)
              | S.member v vS = (vs, e) : fltr ys
              | otherwise     = fltr ys
            fltr _ = error "Algorithms.InsideOutsideWeightsTree.edgeTree.goF.fltr"
        in mapSnd (t' :) $ goF ts $ fltr xs


-- | Drop unreachable 'Hyperedge's from a edge tree, which means 'Hyperedge's
-- which are unreachable from all target vertices (extended by the position
-- @[]@) in the 'Hypergraph' encoded by the edge tree.
dropUnreach
  :: (Ord v)
  => [v]                       -- ^ target vertices
  -> Tree [Hyperedge v l w i]  -- ^ input edge tree
  -> Tree [Hyperedge v l w i]  -- ^ output edge tree
dropUnreach targets (Node es ts)
  = let vS  = S.fromList targets
        es' = filter (flip S.member vS . eHead) es
    in Node es'
    $ map (uncurry dropUnreach)
    $ flip zip ts
    $ L.transpose
    $ map eTail es'


-- | Compute the inside weights of the 'Hypergraph' represented by an edge
-- tree. In the result the lacking positions in the vertices are represented
-- by the position in the output 'Tree'.
inside
  :: (Num w, Ord v)
  => (Hyperedge v l w' i -> w)  -- ^ hyperedge weight accessor function
  -> Tree [Hyperedge v l w' i]  -- ^ edge tree
  -> Tree (M.Map v w)           -- ^ inside weights
inside eW (Node es ts)
  = Node
      ( M.fromListWith (+)
      $ map (\ e -> (eHead e, go ts' (eTail e) (eW e))) es
      )
      ts'
  where
    ts' = map (inside eW) ts
    go (x : xs) (v : vs) p
      = let p' = p * M.findWithDefault err v (rootLabel x)
              where err = error "Algorithms.InsideOutsideWeightsTree.inside.go.p'"
        in p' `seq` go xs vs p'
    go [] [] p = p
    go _  _  _ = error "Algorithms.InsideOutsideWeightsTree.inside.go"


-- | Compute the outside weights of the 'Hypergraph' represented by an edge
-- tree, based on the given inside weights. In the result the lacking
-- positions in the vertices are represented by the position in the output
-- 'Tree'.
outside
  :: (Num w, Ord v)
  => (Hyperedge v l w' i -> w)  -- ^ hyperedge weight accessor function
  -> Tree (M.Map v w)           -- ^ inside weights
  -> Tree [Hyperedge v l w' i]  -- ^ input edge tree
  -> M.Map v w                  -- ^ outside weight(s) of the target node(s)
  -> Tree (M.Map v w)           -- ^ outside weights
outside eW (Node _ is) (Node es ts) om
  = Node om
  $ zipWith3 (outside eW) is ts
  $ map (M.fromListWith (+))
  $ L.transpose
  $ flip map es
    (\ e ->
      let w = M.findWithDefault 0 (eHead e) om * eW e  -- 0 if unreachable
          ws = zipWith (flip (M.findWithDefault err) . rootLabel) is (eTail e)
            where err = error "Algorithms.InsideOutsideWeightsTree.outside.ws"
          ls = scanl (*) 1 ws
          rs = scanr (*) 1 ws
      in zipWith3 (\ v l r -> (v, w * l * r)) (eTail e) ls (tail rs)
    )

{-
zip3Trees :: Tree a -> Tree b -> Tree c -> Tree (a, b, c)
zip3Trees (Node r1 ts1) (Node r2 ts2) (Node r3 ts3)
  = Node (r1, r2, r3) $ zipWith3 zip3Trees ts1 ts2 ts3


mapWithPos :: ([Int] -> a -> b) -> Tree a -> Tree b
mapWithPos f t = go (t, [])
  where
    go (Node l ts, p)
      = Node (f p l)
      $ map go
      $ zip ts
      $ map (: p) [1 :: Int ..]



smalltest :: Int -> IO ()
smalltest i = do
  let tree = (Node 's' [Node 's' [Node 'b' [], Node 'a' []], Node 's' [Node 'a' [], Node 'a' []]])
  let target = 'A'
  let g  = parseTree target tree (testHypergraphs !! i)
  let i1 = IO.inside eWeight g :: M.Map (Char, [Int]) Double
  let o1 = IO.outside eWeight i1 (target, []) g
  let t  = edgeTree [target] (testHypergraphs !! i) tree
  let i2 = inside t :: Tree (M.Map Char Double)
  let o2 = outside i2 t (M.singleton target 1)
  putStrLn $ unlines $ map show $ M.toList i1
  putStrLn $ drawTree $ fmap show i2
  putStrLn $ unlines $ map show $ M.toList o1
  putStrLn $ drawTree $ fmap show o2


-- test1 i = IO.inside eWeight $ parseTree 'A' tree (testHypergraphs !! i) :: M.Map (Char, [Int]) Double
-- -- test2 i = snd $ inside tree 'A' (testHypergraphs !! i) :: M.Map (Char, [Int]) Double
-- test2 i = inside $ edgeTree ['A'] (testHypergraphs !! i) tree :: Tree (M.Map Char Double)
-- 
-- testo1 i = IO.inside eWeight $ parseTree 'A' tree (testHypergraphs !! i) :: M.Map (Char, [Int]) Double
-- testo2 i = inside $ edgeTree ['A'] (testHypergraphs !! i) tree :: Tree (M.Map Char Double)




corpusToInt
  :: (Ord k, Traversable a, Traversable b)
  => a (b k) -> (M.Map k Int, a (b Int))
corpusToInt
  = mapAccumL
      ( mapAccumL
        (\ m k -> let s = M.size m
                  in maybe (M.insert k s m, s) ((,) m) (M.lookup k m)
        )
      )
      M.empty


-- | Convert a corpus in NEGRA format to a list of 'Data.Tree's with pos tags
-- as leaves.
convertNegra :: N.Negra -> [Tree String]
convertNegra n
  = map N.negraTreeToTree
  . concatMap N.negraToForest
  . filter (not . null)
  . map (filter fltr)
  . map N.sData
  $ N.sentences n
  where
    -- unbound = ["UNKNOWN","--","$,","$.","$("]
    unbound = map N.wtTag $ filter (not . N.wtBound) $ N.wordtags n
    fltr N.SentenceWord{N.sdPostag = tag} = notElem tag unbound
    fltr _ = True


-- | Remove the leaves from a 'T.Tree'.
defoliate :: Tree t -> Tree t
defoliate (Node _ []) = error "Cannot defoliate a leaf-only tree."
defoliate (Node x xs)
  = Node x $ map defoliate $ filter (not . null . subForest) xs


yield :: Tree a -> [a]
yield (Node r []) = [r]
yield (Node _ ts) = concatMap yield ts


getData :: IO [Tree String]
getData
  = fmap (convertNegra . N.parseNegra)
  $ N.readFileLatin1
      "/var/local/share/gdp/nlp/resources/tigercorpus2.1/corpus/tiger_release_aug07.export"


main :: IO ()
main = do
  [hgFile, treeIndex, defol] <- getArgs
  test2 hgFile (read treeIndex) (read defol)

test :: String -> Int -> Bool -> IO ()
test hgFile treeIndex defol = do
  let target = 0
  let f = if defol then map defoliate else id
  g <-  fmap (read :: String -> Hypergraph Int Int Double Int)
    $   readFile hgFile
  ts <- fmap (f . snd . corpusToInt) getData
  -- intDict <- fmap (IM.fromList . read :: String -> IM.IntMap String) $ readFile "IntDict.txt"
  -- let intDictLookup v = IM.findWithDefault (show v) v intDict
  print $ rnf $ take treeIndex ts
  flip mapM_ (drop treeIndex ts) $ \ t -> do
    -- let target' = (0, target, length $ yield t)
    let g' = parseTree target t g
    let i1 = IO.inside eWeight g'
    let o1 = IO.outside eWeight i1 (target, []) g'
    let t' = dropUnreach [target] $ edgeTree [target] g t
    let i2 = inside t'
    let o2 = outside i2 t' (M.singleton target 1)
    let im2 = M.unions $ flatten $ mapWithPos (\ p -> M.mapKeysMonotonic (\ k -> (k, p))) $ i2
    let om2 = M.unions $ flatten $ mapWithPos (\ p -> M.mapKeysMonotonic (\ k -> (k, p))) $ o2
--     rnf i1 `seq` rnf o1 `seq` putStrLn "blub"
    if check (M.toList i1) (M.toList im2)
      then putStr "i-OK\t"
      else putStr "i-:(\t"
    if check (M.toList o1) (M.toList om2)
      then putStr "o-OK\t"
      else putStr "o-:(\t"
--     putStrLn ""
--     putStrLn $ drawTree $ fmap show $ t
--     putStrLn $ unlines $ map show $ M.toList i1
--     putStrLn $ unlines $ map show $ M.toList im2
--     putStrLn $ unlines $ map show $ M.toList o1
--     putStrLn $ unlines $ map show $ M.toList om2
--     putStrLn $ replicate 80 '='
  where
    check xs [] = all (0 ==) $ map snd xs
    check [] xs = all (0 ==) $ map snd xs
    check xs@((k, v) : xs') ys@((k', v') : ys')
      | k < k' = v  == 0 && check xs' ys
      | k > k' = v' == 0 && check xs  ys'
      | True   = convergedRatio 1e-15 v' v && check xs' ys'
    convergedRatio :: (Ord a, Num a) => a -> a -> a -> Bool
    convergedRatio epsilon x y
      = let (mi, ma) = if x < y then (x, y) else (y, x)
        in ma - mi <= ma * epsilon


test2 :: FilePath -> Int -> Bool -> IO ()
test2 hgFile treeCnt defol = do
  let target = 0
  let f = if defol then map defoliate else id
  g <-  fmap (read :: String -> Hypergraph Int Int Double Int)
    $   readFile hgFile
  ts <- fmap (f . snd . corpusToInt) getData
  -- let foo = map (\ t -> parseTree target t g) (take treeCnt ts)
  let foo = map (\ t -> dropUnreach [target] $ edgeTree [target] g t) (take treeCnt ts)
  print $ L.foldl' (+) 0 [1 :: Int .. 20000000]
  print $ rnf $ foo
  print $ L.foldl' (+) 0 [1 :: Int .. 20000000]
  print $ rnf $ foo
-}