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

module Vanda.CBSM.CountBasedStateMerging where


import qualified Data.RevMap as RM
import           Data.RevMap (RevMap)
import qualified Vanda.Features as F
import qualified Vanda.Hypergraph as H
import           Vanda.Util.Histogram (histogram)
import           Vanda.Util.Tree as T

import           Control.Applicative ((<*>), (<$>))
import           Control.Arrow ((***), first, second)
import           Control.Monad.State.Lazy
import qualified Data.Binary as B
import           Data.List (foldl', groupBy, sortBy)
import           Data.Function (on)
import qualified Data.Map as M
import           Data.Map (Map, (!))
import           Data.Maybe
import           Data.Ord (comparing, Down(..))
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Tree
import qualified Data.Vector as V
import           Numeric.Log (Log(..))

import Debug.Trace


data Rule s t = Rule
  { to    :: s
  , from  :: [s]
  , label :: t
  , count :: Int
  }

instance (Eq s, Eq t) => Eq (Rule s t) where
  Rule  x1  y1  z1  _  ==  Rule  x2  y2  z2  _
    =  (x1, y1, z1)    ==       (x2, y2, z2)
  Rule  x1  y1  z1  _  /=  Rule  x2  y2  z2  _
    =  (x1, y1, z1)    /=       (x2, y2, z2)

instance (Ord s, Ord t) => Ord (Rule s t) where
  Rule  x1  y1  z1  _  `compare`  Rule  x2  y2  z2  _
    =  (x1, y1, z1)    `compare`       (x2, y2, z2)

instance (Show s, Show t) => Show (Rule s t) where
  show Rule{..} = "Rule " ++ show to ++ " " ++ show from ++ " " ++ show label ++ " " ++ show count

instance (B.Binary v, B.Binary l) => B.Binary (Rule v l) where
  put (Rule w x y z) = B.put w >> B.put x >> B.put y >> B.put z
  get = Rule <$> B.get <*> B.get <*> B.get <*> B.get


-- | helper type to prevent confusion of forward and backward star
data RuleSets v l = (:->)
  { backward :: Set (Rule v l)  -- ^ reserved for backward star
  , forward  :: Set (Rule v l)  -- ^ reserved for forward star
  } deriving Show

instance (B.Binary v, B.Binary l) => B.Binary (RuleSets v l) where
  put (bw :-> fw) = B.put bw >> B.put fw
  get = (:->) <$> B.get <*> B.get


-- v … vertices/states, l … labels/terminals
type RTG v l = Map v (Map l (RuleSets v l))


-- | Count RTG
data CRTG v l = CRTG
  { getRTG   :: RTG v l
  , cntState :: Map v Int
  , cntInit  :: Map v Int
  } deriving Show

instance (B.Binary v, B.Binary l) => B.Binary (CRTG v l) where
  put (CRTG x y z) = B.put x >> B.put y >> B.put z
  get = CRTG <$> B.get <*> B.get <*> B.get


fromList :: (Ord s, Ord t) => [Rule s t] -> RTG s t
fromList = unions . concatMap step
  where
    step r@(Rule v vs l _) = singletonBW v l r : map (\ v' -> singletonFW v' l r) vs
    singletonBW v l r = M.singleton v $ M.singleton l (S.singleton r :-> S.empty)
    singletonFW v l r = M.singleton v $ M.singleton l (S.empty :-> S.singleton r)
    union = M.unionWith (M.unionWith unionRuleSets)
    unions = foldl' union M.empty


unionRuleSets
  :: (Ord l, Ord v) => RuleSets v l -> RuleSets v l -> RuleSets v l
unionRuleSets (bw1 :-> fw1) (bw2 :-> fw2)
  = (S.union bw1 bw2 :-> S.union fw1 fw2)


toHypergraph
  :: (H.Hypergraph h, Ord v) => CRTG v l -> (h v l Double, Map v Double)
  -- not the most general type: Double is specific
toHypergraph CRTG{..}
  = ( H.mkHypergraph
      $ map (\ Rule{..} -> (H.mkHyperedge to from label (fromIntegral count / fromIntegral (cntState M.! to))))
      $   S.toList . backward
      =<< M.elems
      =<< M.elems getRTG
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

forestToGrammar :: Ord l => [Tree l] -> CRTG (OrdTree l) l
forestToGrammar corpus
  = CRTG
      (fromList $ map toRule $ M.toList cntState')
      cntState'
      (histogram $ map OrdTree corpus)
  where
    cntState' = histogram $ map OrdTree $ concatMap T.subTrees corpus
    toRule (t@(OrdTree (Node x ts)), c) = Rule t (map OrdTree ts) x c


(****) :: (a -> b -> c) -> (d -> e -> f) -> (a, d) -> (b, e) -> (c, f)
f **** g = \ (xf, xg) (yf, yg) -> (f xf yf, g xg yg)
--       = uncurry (***) . (f *** g)
--(****) = ((uncurry (***) .) .) . (***)


-- cbsm :: CRTG v l -> [CRTG v l]
-- cbsm = iterate cbsmStep

-- cbsmStep :: CRTG v l -> CRTG v l


cbsm g@CRTG{..}
  = case fst (mergeRanking g) of
      (_, ((v1, _), (v2, _))) : _
        -> let g' = flip mergeCRTG g
                  $ saturateMerge getRTG (createMerge [[v1, v2]])
           in g' : cbsm g'
      _ -> []


cbsmStep2 g@CRTG{..}
  = flip mergeCRTG g
  $ (\ ((_, ((v1, _), (v2, _))) : _) -> saturateMerge getRTG (createMerge [[v1, v2]]))
  $ fst $ mergeRanking g


cbsmStep1 g@CRTG{..}
  = map (\ x@(_, ((v1, _), (v2, _))) ->
        (,) x
      $ let mrg = saturateMerge getRTG (createMerge [[v1, v2]]) in
        (map S.toList $ M.elems $ RM.backward mrg, likelihoodDelta mrg g)
      )
  $ fst $ mergeRanking g


refineRanking (xs, g@CRTG{..})
  = concatMap (sortBy (comparing (Down . snd . snd)))
  $ groupBy ((==) `on` fst . fst)
  $ map (\ x@(_, ((v1, _), (v2, _))) ->
          ( x
          , let mrg = saturateMerge getRTG (createMerge [[v1, v2]]) in
            (mrg, likelihoodDelta mrg g)
        ) )
  $ xs


mergeRanking g@CRTG{..}
  = (sortedCartesianProductWith' ((+) `on` snd) vs (tail vs), g)
    -- ToDo: instead of (+) maybe use states part of likelihood
  where
    vs = sortBy (comparing snd) (M.toList cntState)



saturateMerge
  :: forall s t
  .  (Ord s, Ord t)
  => RTG s t
  -> RevMap s s  -- ^ merges (must be an equivalence relation)
  -> RevMap s s
-- saturateMerge
--   :: RevMap Char Char  -- ^ merges (must be an equivalence relation)
--   -> RTG Char Char
--   -> RevMap Char Char
saturateMerge g m0 = evalState go (M.keysSet (RM.forward m0), m0)
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
                $ M.unionsWith S.union
                $ map (fmap forward)
                $ mapMaybe (flip M.lookup g)
                $ maybe [] S.toList (RM.equivalenceClass s mrgs)
                )
            $ mapM_ (\ mrg -> modify ((S.insert (head mrg)) *** addMerge (S.fromList mrg)))
            . map S.toList
            . filter ((2 <=) . S.size)
            . M.elems
            . M.fromListWith S.union
            . map (\ Rule{..} -> (map (mergeState mrgs) from, S.singleton (mergeState mrgs to)))
            . S.toList
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


likelihoodDelta :: (Ord l, Ord v) => RevMap v v -> CRTG v l -> Log Double
likelihoodDelta mrgs CRTG{..}
  = product  -- initial states
    [ p (sum cs) / product (map p cs)
    | vS <- M.elems $ RM.backward mrgs
    , let cs = M.elems $ M.intersection cntInit (M.fromSet undefined vS)
      -- only consider explicitly given counts from cntInit
      -- these must be > 0
    , not (null cs)
    ]
  * product  -- rules
    [ p (sum cs) / product (map p cs)
    | cs <- map (map count) $ M.elems $ ruleEquivalenceClasses mrgs getRTG
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
  :: (Ord l, Ord v) => RevMap v v -> RTG v l -> Map (Rule v l) [Rule v l]
ruleEquivalenceClasses mrgs g
  = M.fromListWith (++)
  $ map (\ r -> (mergeRule mrgs r, [r]))
  $ S.elems
  $ S.unions
  $ map (\ (bw :-> fw) -> S.union bw fw)
  $ concatMap M.elems
  $ M.elems
  $ M.intersection g (RM.forward mrgs)


mergeState :: Ord v => RevMap v v -> v -> v
mergeState m = \ v -> M.findWithDefault v v (RM.forward m)


mergeRule :: Ord v => RevMap v v -> Rule v l -> Rule v l
mergeRule mrgs Rule{..} = Rule (mrg to) (map mrg from) label count
  where mrg = mergeState mrgs


mergeRTG :: (Ord l, Ord v) => RevMap v v -> RTG v l -> RTG v l
mergeRTG mrgs
  = M.mapKeysWith (M.unionWith unionRuleSets) (mergeState mrgs)
  . M.map (M.map (mergeRuleS *->* mergeRuleS))
  where
    mergeRuleS = S.map (mergeRule mrgs)
    f *->* g = \ (bw :-> fw) -> (f bw :-> g fw)


mergeCRTG :: (Ord l, Ord v) => RevMap v v -> CRTG v l -> CRTG v l
mergeCRTG mrgs CRTG{..}
  = CRTG
      (mergeRTG mrgs getRTG)
      (M.mapKeysWith (+) (mergeState mrgs) cntState)
      (M.mapKeysWith (+) (mergeState mrgs) cntInit)


whileM :: Monad m => m Bool -> m a -> m ()
whileM cond act = do
  b <- cond
  case b of
    True  -> act >> whileM cond act
    False -> return ()
