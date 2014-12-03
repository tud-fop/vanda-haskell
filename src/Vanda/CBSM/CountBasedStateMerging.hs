{-# LANGUAGE LambdaCase, RecordWildCards, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CBSM.CountBasedStateMerging
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

module Vanda.CBSM.CountBasedStateMerging
( Rule(..)
, CRTG(..)
, MergeTree(..)
, forestToGrammar
, cbsm
, toHypergraph
, asBackwardStar
, bests
, cbsmStep2
, refineRanking
, mergeRanking
, enrichRanking
, ruleEquivalenceClasses
, forwardStar
, bidiStar
, createMerge
, likelihoodDelta
, saturateMerge
, sortedCartesianProductWith
, sortedCartesianProductWith'
) where


import qualified Control.Error
import qualified Data.RevMap as RM
import           Data.RevMap (RevMap)
import qualified Vanda.Features as F
import qualified Vanda.Hypergraph as H
import           Vanda.Util.Histogram (histogram)
import           Vanda.Util.Tree as T

import           Control.Applicative ((<*>), (<$>))
import           Control.Arrow ((***), first)
import           Control.Monad.State.Lazy
import           Control.Parallel.Strategies
import qualified Data.Binary as B
import           Data.List (foldl', groupBy, minimumBy, sortBy)
import           Data.Function (on)
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ord (comparing, Down(..))
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Tree
import           Data.Tuple (swap)
import qualified Data.Vector as V
import           Numeric.Log (Log(..))

import Debug.Trace


errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.CBSM.CountBasedStateMerging"


data Rule s t = Rule
  { to    :: !s
  , from  :: ![s]
  , label :: !t
  } deriving (Eq, Ord)

instance (Show s, Show t) => Show (Rule s t) where
  show Rule{..} = "Rule " ++ show to ++ " " ++ show from ++ " " ++ show label

instance (B.Binary v, B.Binary l) => B.Binary (Rule v l) where
  put (Rule x y z) = B.put x >> B.put y >> B.put z
  get = Rule <$> B.get <*> B.get <*> B.get


-- | Count RTG
data CRTG v l = CRTG
  { cntRule  :: !(Map (Rule v l) Int)
  , cntState :: !(Map v Int)
  , cntInit  :: !(Map v Int)
  } deriving Show

instance (B.Binary v, B.Binary l) => B.Binary (CRTG v l) where
  put (CRTG x y z) = B.put x >> B.put y >> B.put z
  get = CRTG <$> B.get <*> B.get <*> B.get


rules :: CRTG v l -> [Rule v l]
rules = M.keys . cntRule


type ForwardStar v l = Map v (Map l [Rule v l])


forwardStar :: (Ord v, Ord l) => [Rule v l] -> ForwardStar v l
forwardStar
  = fmap (M.fromListWith (++)) . M.fromListWith (++) . concatMap step
  where
    step r@(Rule _ vs l)
      = map (\ v -> (v, [(l, [r])]))
      $ (S.toList . S.fromList) vs


-- | bidirectional star: finding rules with state
type BidiStar v l = Map v [Rule v l]


bidiStar :: (Ord v, Ord l) => [Rule v l] -> BidiStar v l
bidiStar = M.fromListWith (++) . concatMap step
  where
    step r@(Rule v vs _)
      = map (\ v' -> (v', [r]))
      $ (S.toList . S.fromList) (v : vs)


{-
fromList :: (Ord s, Ord t) => [Rule s t] -> RTG s t
fromList = unions . concatMap step
  where
    step r@(Rule v vs l _) = singletonBW v l r : map (\ v' -> singletonFW v' l r) vs
    singletonBW v l r = M.singleton v $ M.singleton l (S.singleton r :-> S.empty)
    singletonFW v l r = M.singleton v $ M.singleton l (S.empty :-> S.singleton r)
    union = M.unionWith (M.unionWith unionRuleSets)
    unions = foldl' union M.empty
-}
{-
unionRuleSets
  :: (Ord l, Ord v) => RuleSets v l -> RuleSets v l -> RuleSets v l
unionRuleSets (bw1 :-> fw1) (bw2 :-> fw2)
  = (S.union bw1 bw2 :-> S.union fw1 fw2)
-}

toHypergraph
  :: (H.Hypergraph h, Ord v) => CRTG v l -> (h v l Double, Map v Double)
  -- not the most general type: Double is specific
toHypergraph CRTG{..}
  = ( H.mkHypergraph
      $ map (\ (Rule{..}, count) -> (H.mkHyperedge to from label
                       (fromIntegral count / fromIntegral (cntState M.! to))))
      $ M.toList cntRule
    , M.map (((1 / (fromIntegral $ sum $ M.elems cntInit)) *) . fromIntegral)
            cntInit
    )


bests :: (Ord v, Eq l) => CRTG v l -> [(Double, H.Derivation v l Double)]
bests g
  = mergesBy (comparing (Down . fst))
  $ M.elems
  $ M.intersectionWith (\ w' -> map (\ (F.Candidate w d _) -> (w' * w, d))) ini
--   $ M.map (map (\ (F.Candidate w d _) -> (w, d)))
  $ H.bests (asBackwardStar hg) feature (V.singleton 1)
  where
    (hg, ini) = toHypergraph g
    feature = F.Feature (\ _ i xs -> i * product xs) V.singleton


asBackwardStar :: H.BackwardStar v l i -> H.BackwardStar v l i
asBackwardStar = id


-- | Merge sorted lists to a single sorted list.
mergesBy :: (a -> a -> Ordering) -> [[a]] -> [a]
mergesBy cmp = foldl (mergeBy cmp) []


-- | Merge two sorted lists to a single sorted list.
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp xs@(x:xs') ys@(y:ys')
  = case x `cmp` y of
      GT ->  y : mergeBy cmp xs  ys'
      _  ->  x : mergeBy cmp xs' ys
mergeBy _ [] ys = ys
mergeBy _ xs [] = xs


{-
data ShowTree a
  = a :< [ShowTree a]
  | L a
  deriving (Eq, Ord, Show)

showTree   (x `Node` []) = L x
showTree   (x `Node` ts) = x :<     map showTree   ts
unshowTree (L x        ) = x `Node` []
unshowTree (x :<     ts) = x `Node` map unshowTree ts
-}

newtype OrdTree a = OrdTree (Tree a) deriving Eq


unOrdTree :: OrdTree t -> Tree t
unOrdTree (OrdTree t) = t


instance Ord a => Ord (OrdTree a) where
  compare (OrdTree (Node x1 ts1)) (OrdTree (Node x2 ts2))
    = case compare x1 x2 of
        EQ -> compare (map OrdTree ts1) (map OrdTree ts2)
        o -> o


instance Show a => Show (OrdTree a) where
  showsPrec d (OrdTree (Node x [])) = showsPrec d x
  showsPrec _ (OrdTree (Node x ts)) = showsPrec 11 x . showsPrec 11 (map OrdTree ts)
--   show (OrdTree (Node x [])) = stripQuotes (show x)
--   show (OrdTree (Node x ts)) = stripQuotes (show x) ++ show (map OrdTree ts)

instance (B.Binary a) => B.Binary (OrdTree a) where
  put (OrdTree x) = B.put x
  get = OrdTree <$> B.get


{-
stripQuotes :: String -> String
stripQuotes cs@[_]                           = cs
stripQuotes cs@('"' : cs') | last cs' == '"' = init cs'
stripQuotes cs                               = cs


data Term a = Lit a
            | a :++ a
            | Term a :+ Term a
            | Term a :* Term a deriving (Read, Show)
infixl 6 :+
infixl 7 :*
infixl 5 :++

x1, x2 :: Term Int
x1 = Lit 1 :+ Lit 2 :* Lit 3
x2 = Lit 4 :* Lit 5 :+ Lit 6
x3 = OrdTree $ Node x1 [Node x2 [], Node x1 [Node x2 []]]
x4 = OrdTree $ Node x1 []
x5 = x4 :++ x4


instance Read a => Read (OrdTree a) where
  readsPrec d = readParen False $ \ cs0 ->
      [ (OrdTree (Node x (map unpack ts)), cs2)
      | (x , cs1) <- readsPrec d cs0
      , (ts, cs2) <- case lex cs1 of
                      ("(", _) : _ -> readsPrec 11 cs1
                      ("[", _) : _ -> readsPrec 11 cs1
                      _ ->  [([], cs1)]
      ]
    where unpack (OrdTree t) = t
-}

forestToGrammar
  :: Ord l
  => [Tree l]
  -> ((CRTG Int l, Map Int (MergeTree Int)), Map Int (Tree l))
forestToGrammar corpus
  = ( ( CRTG
          (M.mapKeys toRule cntTrees)
          (M.mapKeysMonotonic (ints M.!) cntTrees)
          (M.mapKeysMonotonic (ints M.!) $ histogram $ map OrdTree corpus)
      , M.fromAscList $ map (\ v -> (v, State v)) $ M.elems ints
      )
    , M.map unOrdTree $ M.fromAscList $ map swap $ M.toAscList $ ints
    )
  where
    cntTrees = histogram $ map OrdTree $ concatMap T.subTrees corpus
    ints = snd $ M.mapAccum (\ i _ -> (i + 1, i)) 0 $ cntTrees
    toRule t@(OrdTree (Node x ts))
      = Rule (ints M.! t) (map ((ints M.!) . OrdTree) ts) x


(****) :: (a -> b -> c) -> (d -> e -> f) -> (a, d) -> (b, e) -> (c, f)
f **** g = \ (xf, xg) (yf, yg) -> (f xf yf, g xg yg)
--       = uncurry (***) . (f *** g)
--(****) = ((uncurry (***) .) .) . (***)


-- cbsm :: CRTG v l -> [CRTG v l]
-- cbsm = iterate cbsmStep

-- cbsmStep :: CRTG v l -> CRTG v l



data MergeTree v = State v | Merge Int [MergeTree v]


instance (B.Binary a) => B.Binary (MergeTree a) where
  put (State x)    = B.putWord8 0 >> B.put x
  put (Merge i xs) = B.putWord8 1 >> B.put i >> B.put xs
  get = do ctor <- B.getWord8
           case ctor of
             0 -> State <$> B.get
             1 -> Merge <$> B.get <*> B.get
             _ -> errorHere "get/MergeTree" "invalid binary data"


instance Functor MergeTree where
  fmap f (State v   ) = State (f v)
  fmap f (Merge i xs) = Merge i (fmap (fmap f) xs)


cbsm
  :: (Ord v, Ord l)
  => Int
  ->  (Int, (CRTG v l, Map v (MergeTree v)))
  -> [(Int, (CRTG v l, Map v (MergeTree v)))]
cbsm beamWidth prev@(n, (g, mtM))
  = (prev :)
  $ seq n
  $ seq g
  $ seq mtM
  $ let n' = n + 1
        cands = mergeRanking g
        mrg = fst $ snd
            $ minimumBy (comparing (Down . snd . snd))
            $ take beamWidth  -- TODO: Group?
            $ enrichRanking cands
        g' = mergeCRTG mrg g
        mtM' = M.map (\ case [x] -> x; xs -> Merge n' xs)
             $ M.mapKeysWith (++) (mergeState mrg)
             $ M.map (: []) mtM
    in if null (fst cands) then [] else cbsm beamWidth (n', (g', mtM'))
--   = g
--   : ( g `seq` case refineRanking $ enrichRanking $ mergeRanking g of
--         ((_, ((v1, _), (v2, _))), _) : _
--           -> let g' = flip mergeCRTG g
--                     $ saturateMerge (forwardStar (rules g)) (createMerge [[v1, v2]])
--             in cbsm g'
--         _ -> []
--     )


cbsmStep2 :: (Ord v, Ord l) => CRTG v l -> CRTG v l
cbsmStep2 g
  = flip mergeCRTG g
  $ (\ ((_, ((v1, _), (v2, _))) : _) -> saturateMerge (forwardStar (rules g)) (createMerge [[v1, v2]]))
  $ fst $ mergeRanking g


cbsmStep1
  :: (Ord v, Ord l)
  => CRTG v l
  -> [((Int, ((v, Int), (v, Int))), ([[v]], Log Double))]
cbsmStep1 g
  = map (\ x@(_, ((v1, _), (v2, _))) ->
        (,) x
      $ let mrg = saturateMerge (forwardStar (rules g)) (createMerge [[v1, v2]]) in
        (map S.toList $ M.elems $ RM.backward mrg, likelihoodDelta g mrg)
      )
  $ fst $ mergeRanking g


refineRanking
  :: Eq a
  => [((a, b), (c, Log Double))]
  -> [((a, b), (c, Log Double))]
refineRanking
  = concatMap (sortBy (comparing (Down . snd . snd)))
  . groupBy ((==) `on` fst . fst)


enrichRanking
  :: (Ord v, Ord l)
  => ([(a, ((v, b), (v, c)))], CRTG v l)
  -> [((a, ((v, b), (v, c))), (RevMap v v, Log Double))]
enrichRanking (xs, g)
  = map (\ x@(_, ((v1, _), (v2, _))) ->
          ( x
          , let mrg = satMrg $ createMerge [[v1, v2]] in
            (mrg, lklhdDelta mrg)
        ) )
  $ xs
  where lklhdDelta = likelihoodDelta g
        satMrg = saturateMerge (forwardStar (rules g))


mergeRanking :: CRTG v l -> ([(Int, ((v, Int), (v, Int)))], CRTG v l)
mergeRanking g
  = (sortedCartesianProductWith' ((+) `on` snd) vs (tail vs), g)
    -- ToDo: instead of (+) maybe use states part of likelihood
  where
    vs = sortBy (comparing snd) (M.toList (cntState g))



saturateMerge
  :: forall s t
  .  (Ord s, Ord t)
  => ForwardStar s t
  -> RevMap s s  -- ^ merges (must be an equivalence relation)
  -> RevMap s s
-- saturateMerge
--   :: RevMap Char Char  -- ^ merges (must be an equivalence relation)
--   -> RTG Char Char
--   -> RevMap Char Char
saturateMerge g = \ m0 -> evalState go (M.keysSet (RM.forward m0), m0)
  where
    go :: State (Set s, RevMap s s) (RevMap s s)
    -- go :: State (Set Char, RevMap Char Char) (RevMap Char Char)
    go = do
      modify $ \ (todo, mrgs) -> (S.map (mergeState mrgs) todo, mrgs)
      gets (S.minView . fst) >>= \ case
        Nothing -> gets snd
        Just (s, sS) -> do
          putFst sS
          mrgs <- gets snd
          forM_ ( M.elems
                $ M.unionsWith (++)
                $ mapMaybe (flip M.lookup g)
                $ maybe [] S.toList (RM.equivalenceClass s mrgs)
                )
            $ mapM_ (\ mrg -> modify (S.insert (S.findMin mrg) *** addMerge mrg))
            . filter ((2 <=) . S.size)
            . M.elems
            . M.fromListWith S.union
            . map (\ Rule{..} -> (map (mergeState mrgs) from, S.singleton (mergeState mrgs to)))
          go


traceShow' :: Show a => [Char] -> a -> a
traceShow' cs x = trace (cs ++ ": " ++ show x) x


putFst :: Monad m => s1 -> StateT (s1, s2) m ()
putFst x = modify (\ (_, y) -> (x, y))
-- putSnd y = modify (\ (x, _) -> (x, y))


createMerge :: Ord k => [[k]] -> RevMap k k
createMerge = foldl' (flip addMerge) RM.empty . map S.fromList


addMerge :: Ord k => Set k -> RevMap k k -> RevMap k k
addMerge new old
  | S.size new < 2 = old
  | otherwise
    = insertList (S.toList new)
    $ flip insertList old
    $ S.toList
    $ S.unions
    $ map ((RM.backward old !) . (RM.forward old !))  -- equivalence class
    $ S.toList
    $ S.intersection new
    $ M.keysSet
    $ RM.forward old
  where
    representative = S.findMin new
    insertList = flip $ foldl' (\ m k -> RM.insert k representative m)


-- | Lazily calculate the Cartesian product of two lists sorted by a score
-- calculated from the elements. /The following precondition must hold:/
-- For a call with arguments @f@, @[x1, …, xm]@, and @[y1, …, yn]@ for every
-- @xi@ and for every @yj@, @yk@ with @j <= k@: @f xi yj <= f xi yk@ must
-- hold, and analogously for @xi@, @xj@, and @yk@.
sortedCartesianProductWith
  :: Ord c
  => (a -> b -> c)  -- ^ calculates a score
  -> [a]
  -> [b]
  -> [(c, (a, b))]  -- ^ Cartesian product ('snd') sorted by score ('fst')
sortedCartesianProductWith
  = sortedCartesianProductWithInternal (\ _ _ -> True)


-- | The same as 'sortedCartesianProductWith', but only pairs @(xi, yj)@ with
-- @i <= j@ are returned.
sortedCartesianProductWith'
  :: Ord c => (a -> b -> c) -> [a] -> [b] -> [(c, (a, b))]
sortedCartesianProductWith'
  = sortedCartesianProductWithInternal (<=)


sortedCartesianProductWithInternal
  :: forall a b c . Ord c
  => (Int -> Int -> Bool)  -- ^ filter combinations
  -> (a -> b -> c)  -- ^ calculates a score
  -> [a]
  -> [b]
  -> [(c, (a, b))]  -- ^ Cartesian product ('snd') sorted by score ('fst')
sortedCartesianProductWithInternal (?) (>+<) (x0 : xs0) (y0 : ys0)
  = go1 $ M.singleton (x0 >+< y0) (M.singleton (0, 0) ((x0, y0), (xs0, ys0)))
  where
    go1 :: Map c (Map (Int, Int) ((a, b), ([a], [b]))) -> [(c, (a, b))]
    go1 = maybe [] go2 . M.minViewWithKey

    go2
      :: (    (c, Map (Int, Int) ((a, b), ([a], [b])))
         , Map c (Map (Int, Int) ((a, b), ([a], [b]))) )
      -> [(c, (a, b))]
    go2 ((mini, srcM), m)
      = map ((,) mini . fst) (M.elems srcM)
      ++ go1 (M.foldrWithKey' adjust m srcM)

    adjust
      ::            (Int, Int)
      ->                       ((a, b), ([a], [b]))
      -> Map c (Map (Int, Int) ((a, b), ([a], [b])))
      -> Map c (Map (Int, Int) ((a, b), ([a], [b])))
    adjust _ (_, ([], [])) = id
    adjust (i, j) ((x1, _), ([], y2 : ys2))
      = insert i (j + 1) x1 y2 [] ys2
    adjust (i, j) ((_, y1), (x2 : xs2, []))
      = insert (i + 1) j x2 y1 xs2 []
    adjust (i, j) ((x1, y1), (xs1@(x2 : xs2), ys1@(y2 : ys2)))
      = insert i (j + 1) x1 y2 xs1 ys2
      . insert (i + 1) j x2 y1 xs2 ys1

    insert
      ::             Int->Int -> a->b -> [a]->[b]
      -> Map c (Map (Int, Int) ((a, b), ([a], [b])))
      -> Map c (Map (Int, Int) ((a, b), ([a], [b])))
    insert i j x y xs ys | i ? j = M.insertWith M.union (x >+< y)
                                 $ M.singleton (i, j) ((x, y), (xs, ys))
    insert _ _ _ _ _  _          = id

sortedCartesianProductWithInternal _ _ _ _ = []


likelihoodDelta :: (Ord l, Ord v) => CRTG v l -> RevMap v v -> Log Double
likelihoodDelta g@CRTG{..} = \ mrgs
 -> product  -- initial states
    [ p (sum cs) / product (map p cs)
    | vS <- M.elems $ RM.backward mrgs
    , let cs = M.elems $ M.intersection cntInit (M.fromSet (const ()) vS)
      -- only consider explicitly given counts from cntInit
      -- these must be > 0
    , not (null cs)
    ]
  * product  -- rules
    [ p (sum cs) / product (map p cs)
    | cs <- map (map (cntRule M.!)) $ M.elems $ ruleEquivalenceClasses (bidiStar (rules g)) mrgs
    ]
  * product  -- states
    [ product (map p cs) / p (sum cs)
    | vS <- M.elems $ RM.backward mrgs
    , let cs = map (getCnt cntState) $ S.toList vS
    ]
  where
    -- | power with itself
    p :: Int -> Log Double
    p n = Exp (x * log x)  -- = Exp (log (x ** x))
      where x = fromIntegral n

    getCnt m k = M.findWithDefault 0 k m


ruleEquivalenceClasses
  :: (Ord l, Ord v) => BidiStar v l -> RevMap v v -> Map (Rule v l) [Rule v l]
ruleEquivalenceClasses g mrgs
  = M.fromListWith (++)
  $ map (\ r -> (mergeRule mrgs r, [r]))
  $ (S.toList . S.fromList)
  $ concat
  $ M.elems
  $ M.intersection g (RM.forward mrgs)


mergeState :: Ord v => RevMap v v -> v -> v
mergeState m = \ v -> M.findWithDefault v v (RM.forward m)


mergeRule :: Ord v => RevMap v v -> Rule v l -> Rule v l
mergeRule mrgs Rule{..} = Rule (mrg to) (map mrg from `using` evalList rseq) label
  where mrg = mergeState mrgs


mergeCRTG :: (Ord l, Ord v) => RevMap v v -> CRTG v l -> CRTG v l
mergeCRTG mrgs CRTG{..}
  = CRTG
      (M.mapKeysWith (+) (mergeRule mrgs) cntRule)
      (mergeKeysWith (+) mrgs cntState)
      (mergeKeysWith (+) mrgs cntInit)


-- | Similar to 'M.mapKeysWith', but optimized for merging, because many keys
-- are left unchanged.
mergeKeysWith
  :: Ord k => (a -> a -> a) -> RM.RevMap k k -> M.Map k a -> M.Map k a
mergeKeysWith (?) mrgs
  = uncurry (M.unionWith (?))
  . first (M.mapKeysWith (?) (mergeState mrgs))
  . M.partitionWithKey (\ k _ -> M.member k (RM.forward mrgs))


whileM :: Monad m => m Bool -> m a -> m ()
whileM cond act = do
  b <- cond
  case b of
    True  -> act >> whileM cond act
    False -> return ()
