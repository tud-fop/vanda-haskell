{-# LANGUAGE LambdaCase, RecordWildCards, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CBSM.Main
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

module Vanda.CBSM.Main where


import Control.Arrow ((***))
import qualified Vanda.CBSM.BiMap as BM
import           Vanda.CBSM.BiMap (BiMap)

import Control.Monad.State.Lazy
import Data.List (foldl')
import qualified Data.Map as M
import           Data.Map (Map, (!))
import Data.Maybe
import Numeric.Log (Log(..))
import qualified Data.Set as S
import           Data.Set (Set)

import Debug.Trace


data Rule s t = Rule
  { to    :: s
  , from  :: [s]
  , label :: t
  } deriving (Eq, Ord)

instance (Show s, Show t) => Show (Rule s t) where
  show Rule{..} = "Rule " ++ show to ++ " " ++ show from ++ " " ++ show label


-- s … states, t … terminals
type RTG s t = Map s (Map t (Set (Rule s t), Set (Rule s t)))
                          -- ↳ forward star  ↳ backward star


-- | Count RTG
data CRTG v l i = CRTG
  { getRTG   :: RTG v l
  , cntState :: Map v Int
  , cntInit  :: Map v Int
  , cntRule  :: Map i Int
  }


fromList :: (Ord s, Ord t) => [Rule s t] -> RTG s t
fromList = unions . concatMap step
  where
    step r@(Rule s ss t) = singletonFst s t r : map (\ s' -> singletonSnd s' t r) ss
    singletonFst k1 k2 v = M.singleton k1 $ M.singleton k2 (S.singleton v, S.empty)
    singletonSnd k1 k2 v = M.singleton k1 $ M.singleton k2 (S.empty, S.singleton v)
    union = M.unionWith (M.unionWith (S.union **** S.union))
    unions = foldl' union M.empty


(****) :: (a -> b -> c) -> (d -> e -> f) -> (a, d) -> (b, e) -> (c, f)
f **** g = \ (xf, xg) (yf, yg) -> (f xf yf, g xg yg)
--       = uncurry (***) . (f *** g)
--(****) = ((uncurry (***) .) .) . (***)


saturateMerge
  :: forall s t
  .  (Ord s, Ord t)
  => BiMap s s  -- ^ merges (must be an equivalence relation)
  -> RTG s t
  -> BiMap s s
-- saturateMerge
--   :: BiMap Char Char  -- ^ merges (must be an equivalence relation)
--   -> RTG Char Char
--   -> BiMap Char Char
saturateMerge m0 g = evalState go (M.keysSet (BM.forward m0), m0)
  where
    go :: State (Set s, BiMap s s) (BiMap s s)
    -- go :: State (Set Char, BiMap Char Char) (BiMap Char Char)
    go = do
      modify $ \ (todo, mrgs) -> (S.map (mergeState mrgs) todo, mrgs)
      gets (S.minView . fst) >>= \ case
        Nothing -> gets snd
        Just (s, sS) -> do
          putFst sS
          mrgs <- gets snd
          forM_ ( M.elems
                $ M.unionsWith S.union
                $ map (fmap snd)
                $ mapMaybe (flip M.lookup g)
                $ maybe [] S.toList (BM.equivalenceClass s mrgs)
                )
            $ mapM_ (\ mrg -> modify ((S.insert (head mrg)) *** addMerge (S.fromList mrg)))
            . filter lengthGE2
            . map S.toList
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


lengthGE2 :: [a] -> Bool
lengthGE2 (_ : _ : _) = True
lengthGE2          _  = False


createMerge :: Ord k => [[k]] -> BiMap k k
createMerge = foldl' (flip addMerge) BM.empty . map S.fromList


addMerge :: Ord k => Set k -> BiMap k k -> BiMap k k
addMerge new old
  | S.size new < 2 = old
  | otherwise
    = insertList (S.toList new)
    $ flip insertList old
    $ S.toList
    $ S.unions
    $ map ((BM.backward old !) . (BM.forward old !))  -- equivalence class
    $ S.toList
    $ S.intersection new
    $ M.keysSet
    $ BM.forward old
  where
    representative = S.findMin new
    insertList = flip $ foldl' (\ m k -> BM.insert k representative m)


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


likelihoodDelta :: Ord v => BiMap v v -> CRTG v l i -> Log Double
likelihoodDelta mrgs CRTG{..}
  = product  -- initial states
    [ p (sum cs) / product (map p cs)
    | vS <- M.elems $ BM.backward mrgs
    , let cs = M.elems $ M.intersection cntInit (M.fromSet undefined vS)
      -- only consider explicitly given counts from cntInit
      -- these must be > 0
    , not (null cs)
    ]
  * product  -- rules
    [ p (sum cs) / product (map p cs)
    | vs <- map S.toList $ M.elems $ BM.backward mrgs
    , let cs = undefined
    ]
  * product  -- states
    [ product (map p cs) / p (sum cs)
    | vS <- M.elems $ BM.backward mrgs
    , let cs = map (getCnt cntState) $ S.toList vS
    ]
  where
    -- | power with itself
    p :: Int -> Log Double
    p n = Exp (x * log x)  -- = Exp (log (x ** x))
      where x = fromIntegral n

    getCnt m k = M.findWithDefault 0 k m


ruleEquivalenceClasses
  :: (Ord l, Ord v) => BiMap v v -> RTG v l -> Map (Rule v l) [Rule v l]
ruleEquivalenceClasses mrgs g
  = M.fromListWith (++)
  $ map (\ r -> (mergeRule mrgs r, [r]))
  $ S.elems
  $ S.unions
  $ map (uncurry S.union)
  $ concatMap M.elems
  $ M.elems
  $ M.intersection g (BM.forward mrgs)


mergeState :: Ord k => BiMap k k -> k -> k
mergeState m = \ s -> M.findWithDefault s s (BM.forward m)


mergeRule :: Ord v => BiMap v v -> Rule v l -> Rule v l
mergeRule mrgs Rule{..} = Rule (mrg to) (map mrg from) label
  where mrg = mergeState mrgs


whileM :: Monad m => m Bool -> m a -> m ()
whileM cond act = do
  b <- cond
  case b of
    True  -> act >> whileM cond act
    False -> return ()



rtg0 :: RTG Char Char
rtg0 = fromList
  [ Rule 'A' "BC" 's'
  , Rule 'a' "bc" 's'
  , Rule 'B' "AC" 's'
  , Rule 'b' "ac" 's'
  , Rule 'C' "" 'C'
  , Rule 'c' "" 'c'
  ]

test0 :: BiMap Char Char
test0 = saturateMerge (createMerge ["Bb", "Cc"]) rtg0
