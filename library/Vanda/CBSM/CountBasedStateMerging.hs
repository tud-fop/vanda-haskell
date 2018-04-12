{-# LANGUAGE LambdaCase, Rank2Types, RecordWildCards, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CBSM.CountBasedStateMerging
-- Copyright   :  (c) Technische Universität Dresden 2014–2017
-- License     :  BSD-style
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.CBSM.CountBasedStateMerging
( MergeTree(..)
, Info(..)
, BeamEntry(..)
, initialInfo
, ConfigCBSM(..)
, cbsm
, heuristicCountSum
, heuristicPartialLikelihoodDelta
, normalizeLklhdByMrgdStates
, cbsmStep2
, refineRanking
, mergeRanking
, enrichRanking
, ruleEquivalenceClasses
, likelihoodDelta
, saturateMerge
, sortedCartesianProductWith
, sortedCartesianProductWith'
) where


import Debug.Trace

import           Control.Applicative ((<*>), (<$>))
import           Control.Arrow ((***), first, second)
import           Control.DeepSeq (NFData(rnf))
import           Control.Monad.Trans.State.Lazy
                   (StateT, evalState, modify, runState, state)
import           Control.Parallel.Strategies
import qualified Data.Array as A
import qualified Data.Binary as B
import           Data.Coerce (coerce)
import           Data.List (foldl', groupBy, sortBy)
import           Data.List.Extra (mergeListsBy, minimaBy)
import           Data.Function (on)
import qualified Data.Map.Lazy as ML
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ord (comparing, Down(..))
import qualified Data.Set as S
import           Data.Set (Set)
import qualified Data.IntSet as IS
import           Numeric.Log (Log(..))
import           System.Random (RandomGen, split)

import qualified Control.Error
import           Data.List.Extra (isMultiton)
import           Data.List.Shuffle (shuffle)
import           Data.Maybe.Extra (nothingIf)
import           Vanda.CBSM.CRTG
import           Vanda.CBSM.Dovetailing
import           Vanda.CBSM.Merge (Merge)
import qualified Vanda.CBSM.Merge as Merge


errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.CBSM.CountBasedStateMerging"


(****) :: (a -> b -> c) -> (d -> e -> f) -> (a, d) -> (b, e) -> (c, f)
f **** g = \ (xf, xg) (yf, yg) -> (f xf yf, g xg yg)
--       = uncurry (***) . (f *** g)
--(****) = ((uncurry (***) .) .) . (***)


-- cbsm :: CRTG v l -> [CRTG v l]
-- cbsm = iterate cbsmStep

-- cbsmStep :: CRTG v l -> CRTG v l


-- | Type to keep track of the merges done by 'cbsm'. A state is mapped to
-- a 'MergeTree' representing how the state resulted from merging.
type MergeHistory v = Map v (MergeTree v)

-- | Type to keep track of the merges done by 'cbsm' leading to a specific
-- state. A 'MergeTree' corresponds to a state of a 'CRTG'.
data MergeTree v
  = State v Count            -- ^ state and count before any merge
  | Merge Int [MergeTree v]  -- ^ iteration and merged states


instance B.Binary a => B.Binary (MergeTree a) where
  put (State x i ) = B.putWord8 0 >> B.put x >> B.put i
  put (Merge i xs) = B.putWord8 1 >> B.put i >> B.put xs
  get = do ctor <- B.getWord8
           case ctor of
             0 -> State <$> B.get <*> B.get
             1 -> Merge <$> B.get <*> B.get
             _ -> errorHere "get/MergeTree" "invalid binary data"


instance NFData v => NFData (MergeTree v) where
  rnf (State v c) = rnf v `seq` rnf c
  rnf (Merge i t) = rnf i `seq` rnf t


instance Functor MergeTree where
  fmap f (State v i ) = State (f v) i
  fmap f (Merge i xs) = Merge i (fmap (fmap f) xs)


-- | Information record holding various information about an iteration of
-- 'cbsm'.
data Info g v = Info
  { infoRandomGen             :: !g
  -- ^ state of the prng after an iteration
  , infoIteration             :: !Int
  -- ^ number of iteration
  , infoMergePairs            :: !Int
  -- ^ total number of possible merges
  , infoBeamWidth             :: !Int
  -- ^ number of actually explored merges
  , infoBeamIndex             :: !Int
  -- ^ index of chosen merge within the beam
  , infoBeam                  :: ![BeamEntry v]
  -- ^ detailed information about explored merges. This list is called the
  -- /beam/. It is sorted in descending order w.r.t. 'beHeuristic'.
  , infoEquivalentBeamIndizes :: ![Int]
  -- ^ indices of merges that were evaluated as good as the chosen merge
  , infoMergeTreeMap          :: !(MergeHistory v)
  -- ^ the merges that led to the current states
  }


-- | Information record holding various information about a merge candidate
-- explored by 'cbsm'.
data BeamEntry v = BeamEntry
  { beIndex           :: !Int
  -- ^ index of the merge within the beam (cf. 'infoBeam'). The lowest index
  -- is @1@.
  , beHeuristic       :: !Double
  -- ^ the heuristic value for this merge (cf. 'confHeuristic')
  , beEvaluation      :: !(Log Double)
  -- ^ the evaluation of this merge (cf. 'confEvaluate')
  , beLikelihoodDelta :: !(Log Double)
  -- ^ the change in likelihood induced by this merge (cf. 'likelihoodDelta').
  -- We have:
  -- @'beLikelihoodDelta' = 'beFactorRules' * 'beFactorStates' * 'beFactorInitials'@
  , beFactorRules     :: !(Log Double)
  -- ^ the factor in 'beLikelihoodDelta' contributed by merged rules
  , beFactorStates    :: !(Log Double)
  -- ^ the factor in 'beLikelihoodDelta' contributed by merged states
  , beFactorInitials  :: !(Log Double)
  -- ^ the factor in 'beLikelihoodDelta' contributed by merged initial states
  , beMergedRules     :: !Int
  -- ^ number of rules merged by this merge
  , beMergedStates    :: !Int
  -- ^ number of states merged by this merge
  , beMergedInitials  :: !Int
  -- ^ number of initial states merged by this merge
  , beMergeSeed       :: !(v, v)
  -- ^ the states that led to this merge via 'saturateMerge'
  , beSaturationSteps :: !Int
  -- ^ number of iterations of 'saturateMerge' for this merge
  , beMergeSaturated  :: !(Merge v)
  -- ^ the actual merge (result of 'saturateMerge')
  }


instance (B.Binary v, Ord v, Read g, Show g) => B.Binary (Info g v) where
  put (Info gen a b c d e f g)
    = B.put (show gen)
   >> B.put a >> B.put b >> B.put c >> B.put d >> B.put e
   >> B.put f >> B.put g
  get = Info <$> (read <$> B.get)
    <*> B.get <*> B.get <*> B.get <*> B.get <*> B.get
    <*> B.get <*> B.get


instance (B.Binary v, Ord v) => B.Binary (BeamEntry v) where
  put (BeamEntry a b c d e f g h i j k l m)
    = B.put a >> B.put b >> B.put c >> B.put d >> B.put e
   >> B.put f >> B.put g >> B.put h >> B.put i >> B.put j
   >> B.put k >> B.put l >> B.put m
  get = BeamEntry
    <$> B.get <*> B.get <*> B.get <*> B.get <*> B.get
    <*> B.get <*> B.get <*> B.get <*> B.get <*> B.get
    <*> B.get <*> B.get <*> B.get

instance NFData v => NFData (Info g v) where
  rnf Info{..} = rnf infoBeam
           `seq` rnf infoEquivalentBeamIndizes
           `seq` rnf infoMergeTreeMap

instance NFData v => NFData (BeamEntry v) where
  rnf BeamEntry{..} = rnf beMergeSeed `seq` rnf beMergeSaturated


-- | Create 'Info' for the zeroth iteration of 'cbsm'.
initialInfo :: g -> Map v Count -> Info g v
initialInfo gen m
  = Info
      { infoRandomGen             = gen
      , infoIteration             = 0
      , infoMergePairs            = -1
      , infoBeamWidth             = -1
      , infoBeamIndex             = -1
      , infoBeam                  = []
      , infoEquivalentBeamIndizes = []
      , infoMergeTreeMap          = M.mapWithKey State m
      }


data ConfigCBSM g v = ConfigCBSM
  { confNumCapabilities  :: Int
  -- ^ result of 'getNumCapabilities' for parallelization, 1 for none
  , confMergeGroups      :: [Set v]
  -- ^ partition on the states; only states from the same equivalence class
  --   will be merged
  , confEvaluate         :: (Int, Int, Int) -> Log Double -> Log Double
  -- ^ evaluation function for a merge given number of merged rules, states,
  --   and initial states, and the loss of likelihood
  , confHeuristic       :: Count -> Count -> Double
  -- ^ the heuristic used to decide which pairs of states lie in the beam,
  --   i.e., state pairs that are considered for merging. The state pairs with
  --   the largest heuristic values are chosen.
  --   /Note: The heuristic has to be monotonically decreasing in both arguments./
  , confBeamWidth        :: Int
  -- ^ (minimal) beam width. Number of merges that are (at least) explored by
  -- 'cbsm' per iteration.
  , confDynamicBeamWidth :: Bool
  -- ^ actual beam width is at least 'confBeamWidth', but if
  --   @'confDynamicBeamWidth' == 'True'@, then the actual beam width is
  --   extended to capture all candidates that have a heuristic value that is
  --   as good as for the worst candidate within 'confBeamWidth'
  , confShuffleStates    :: Bool
  -- ^ optionally randomize the order of states with the same count
  , confShuffleMerges    :: Bool
  -- ^ optionally randomize the order of merges with the same heuristic value
  -- (subsumes 'confShuffleStates')
  }


-- | The Count-Based State Merging algorithm.
cbsm
  :: (Ord v, Ord l, RandomGen g)
  => ConfigCBSM g v
  ->  (CRTG v l, Info g v)
  -- ^ starting point; is returned as the first element of the resulting list
  -> [(CRTG v l, Info g v)]
cbsm = cbsmGo M.empty


cbsmGo
  :: (Ord v, Ord l, RandomGen g)
  => Map (v, v) (Merge v)
  -> ConfigCBSM g v
  ->  (CRTG v l, Info g v)
  -> [(CRTG v l, Info g v)]
cbsmGo cache conf@ConfigCBSM{..} prev@(g, info@Info{..})
  = (prev :)
  $ seq g
  $ seq info
  $ let n = infoIteration + 1
        likelihoodDelta' = likelihoodDelta g
        saturateMerge' = saturateMerge $ forwardStar $ rules g
        (genNext, (genState, genMerge)) = second split $ split infoRandomGen
        (mergePairs, cands)
          = sum *** processMergePairs
          $ unzip
          $ zipWith (\ grpS -> fst . compileMergePairs confHeuristic (cntState g) grpS confShuffleStates)
              confMergeGroups
              (evalState (sequence $ repeat $ state $ split) genState)
        processMergePairs
          = ( if confNumCapabilities > 1
              then withStrategy (parListChunk (confBeamWidth `div` (2 * confNumCapabilities)) rseq)
              else id )
          . zipWith ( \ i (h, ((v1, _), (v2, _)))
                      -> processMergePair i h (v1, v2)
                    ) [1 ..]
          . ( if confDynamicBeamWidth
              then takeAtLeastOn fst confBeamWidth
              else take confBeamWidth
            )
          . ( if confShuffleMerges
              then \ xs -> fst $ shuffleGroupsBy ((==) `on` fst) xs genMerge
              else id
            )
          . mergeListsBy (comparing (Down . fst))
        processMergePair i h pair@(v1, v2)
          = BeamEntry
              { beIndex           = i
              , beHeuristic       = h
              , beEvaluation      = confEvaluate sizes l
              , beLikelihoodDelta = l
              , beFactorRules     = rw
              , beFactorStates    = vw
              , beFactorInitials  = iw
              , beMergedRules     = rc
              , beMergedStates    = vc
              , beMergedInitials  = ic
              , beMergeSeed       = pair
              , beSaturationSteps = pred saturationIterations
              , beMergeSaturated  = m
              }
          where (l, (rw, vw, iw), sizes@(rc, vc, ic)) = likelihoodDelta' m
                (m, saturationIterations)
                  = saturateMerge'
                  $ ML.findWithDefault (Merge.fromLists [[v1, v2]]) pair cache
        minimalCands
          = minimaBy (comparing (Down . beEvaluation)) cands
        mrg = beMergeSaturated (head minimalCands)
        apply = Merge.apply mrg
        cache'
          = ML.map (Merge.applyMergeToMerge mrg)
          $ ML.mapKeysWith Merge.union (apply *** apply)
          $ ML.fromList
          $ map (\ BeamEntry{..} -> (beMergeSeed, beMergeSaturated))
                cands
        info' = Info
                  { infoRandomGen             = genNext
                  , infoIteration             = n
                  , infoMergePairs            = mergePairs
                  , infoBeamWidth             = length cands
                  , infoBeamIndex             = beIndex (head minimalCands)
                  , infoBeam                  = cands
                  , infoEquivalentBeamIndizes = map beIndex minimalCands
                  , infoMergeTreeMap
                      = M.map (\ case [x] -> x; xs -> Merge n xs)
                      $ mergeKeysWith (++) mrg
                      $ M.map (: []) infoMergeTreeMap
                  }
    in if null cands
       then []
       else cbsmGo cache' conf (mergeCRTG mrg g, info')
--   = g
--   : ( g `seq` case refineRanking $ enrichRanking $ mergeRanking g of
--         ((_, ((v1, _), (v2, _))), _) : _
--           -> let g' = flip mergeCRTG g
--                     $ saturateMerge (forwardStar (rules g)) (Merge.fromLists [[v1, v2]])
--             in cbsm g'
--         _ -> []
--     )

compileMergePairs
  :: (Ord v, RandomGen g)
  => (Count -> Count -> Double)
  -- ^ heuristic; /must be monotonically decreasing in both arguments/
  -> Map v Count
  -> Set v
  -> Bool
  -- ^ optionally randomize the order of states with the same count
  -> g
  -> ((Int, [(Double, ((v, Count), (v, Count)))]), g)
  -- ^ ((length of list,
  --     [(heuristic value, ((state 1, count 1), (state 2, count 2)))]),
  --    updated RandomGen state)
compileMergePairs heuristic cntM grpS doShuffle g
  = n `seq` ( ( n
              , coerce
                $ sortedCartesianProductWith'
                    ((Down .) . heuristic `on` snd)
                    vs
                    (tail vs) )
            , g' )
  where n     = let s = M.size cntM' in s * (s - 1) `div` 2
        cntM' = M.intersection cntM (M.fromSet (const ()) grpS)
        (vs, g')
          = ( if doShuffle
              then \ xs -> shuffleGroupsBy ((==) `on` snd) xs g
              else \ xs -> (xs, g)
            )
          $ sortBy (comparing snd)
          $ M.toList cntM'


shuffleGroupsBy :: RandomGen g => (a -> a -> Bool) -> [a] -> g -> ([a], g)
shuffleGroupsBy eq xs g
  = flip runState g
  . fmap concat
  . mapM (state . shuffle)
  $ groupBy eq xs


takeAtLeastOn :: Eq b => (a -> b) -> Int -> [a] -> [a]
takeAtLeastOn project = sanitize
  where
    sanitize n = if n < 1 then const [] else takeAtLeast n

    takeAtLeast _ [] = []
    takeAtLeast n (x : xs)
      = x : if n > 1
            then takeAtLeast (pred n) xs
            else takeOn (project x) xs

    takeOn _ [] = []
    takeOn px (y : ys)
      = if px == py
        then y : takeOn py ys
        else []
      where py = project y


-- | Negated sum, i.e., @negate (x + y)@ where @x@ and @y@ are the counts of
-- the two merged states (cf. 'beMergeSeed').
heuristicCountSum :: Count -> Count -> Double
heuristicCountSum x y = negate (x + y)


-- | A subterm of the actual likelihood delta induced by merging (cf.
-- 'likelihoodDelta'), namely @log₂ x^x * y^y / (x+y)^(x+y)@ where @x@ and @y@
-- are the counts of the two merged states (cf. 'beMergeSeed').
heuristicPartialLikelihoodDelta :: Count -> Count -> Double
heuristicPartialLikelihoodDelta x y = ln (p x * p y / p (x + y)) / log 2
  where
    -- | power with itself
    p :: Double -> Log Double
    p x = Exp (x * log x)  -- = Exp (log (x ** x))


normalizeLklhdByMrgdStates :: (Int, Int, Int) -> Log Double -> Log Double
normalizeLklhdByMrgdStates (_, mrgS, _) (Exp l)
  = Exp (l / fromIntegral mrgS)  -- = Exp l ** recip (fromIntegral mrgS)



cbsmStep2 :: (Ord v, Ord l) => CRTG v l -> CRTG v l
cbsmStep2 g
  = flip mergeCRTG g
  $ (\ ((_, ((v1, _), (v2, _))) : _) -> fst $ saturateMerge (forwardStar (rules g)) (Merge.fromLists [[v1, v2]]))
  $ fst $ mergeRanking g


cbsmStep1
  :: (Ord v, Ord l)
  => CRTG v l
  -> [((Log Double, ((v, Count), (v, Count))), ([[v]], (Log Double, (Log Double, Log Double, Log Double), (Int, Int, Int))))]
cbsmStep1 g
  = map (\ x@(_, ((v1, _), (v2, _))) ->
        (,) x
      $ let mrg = fst $ saturateMerge (forwardStar (rules g)) (Merge.fromLists [[v1, v2]]) in
        (map S.toList $ Merge.equivalenceClasses mrg, likelihoodDelta g mrg)
      )
  $ fst $ mergeRanking g


refineRanking
  :: Eq a
  => [((a, b), (c, (Log Double, (Int, Int, Int))))]
  -> [((a, b), (c, (Log Double, (Int, Int, Int))))]
refineRanking
  = concatMap (sortBy (comparing (Down . snd . snd)))
  . groupBy ((==) `on` fst . fst)


enrichRanking
  :: (Ord v, Ord l)
  => ([(a, ((v, b), (v, c)))], CRTG v l)
  -> [ ( (a, ((v, b), (v, c)))
       , ( Merge v
         , (Log Double, (Log Double, Log Double, Log Double) , (Int, Int, Int))
         )
       )
     ]
enrichRanking (xs, g)
  = map (\ x@(_, ((v1, _), (v2, _))) ->
          ( x
          , let mrg = satMrg $ Merge.fromLists [[v1, v2]] in
            (mrg, lklhdDelta mrg)
        ) )
  $ xs
  where lklhdDelta = likelihoodDelta g
        satMrg = fst . saturateMerge (forwardStar (rules g))


mergeRanking :: CRTG v l -> ([(Log Double, ((v, Count), (v, Count)))], CRTG v l)
mergeRanking g
  = (sortedCartesianProductWith' (add `on` snd) vs (tail vs), g)
    -- ToDo: instead of (+) maybe use states part of likelihood
  where
    add x y = Exp (x + y)
    vs = sortBy (comparing snd) (M.toList (cntState g))



-- | Determine the smallest merge that preserves bottom-up determminism and
-- subsumes the given merge.
saturateMerge
  :: forall s t
  .  (Ord s, Ord t)
  => ForwardStar s t
  -> Merge s  -- ^ merges (must be an equivalence relation)
  -> (Merge s, Int)
saturateMerge g mrgs
  = untilRight (saturateMergeStep g) (saturateMergeInit mrgs)


saturateMergeInit :: Merge a -> (Set a, Merge a)
saturateMergeInit mrgs = (Merge.elemS mrgs, mrgs)


saturateMergeStep
  :: (Ord s, Ord t)
  => ForwardStar s t
  -> (Set s, Merge s)
  -> Either (Set s, Merge s) (Merge s)
saturateMergeStep g (todo, mrgs)
  = case S.minView (S.map (Merge.apply mrgs) todo) of
      Nothing      -> Right mrgs
      Just (s, sS) -> Left
        $ foldl' step (sS, mrgs)
        $ concatMap
          ( filter ((2 <=) . S.size)
          . M.elems
          . M.fromListWith S.union
          . map (\ Rule{..} -> ( map (Merge.apply mrgs) from
                               , S.singleton (Merge.apply mrgs to)))
          )
        $ filter isMultiton
        $ M.elems
        $ M.unionsWith (flip (++))  -- bring together rules with same terminal
        $ M.elems
        $ M.intersection g
        $ M.fromSet (const ())
        $ fromMaybe (errorHere "saturateMergeStep" "")
        $ Merge.equivalenceClass s mrgs
  where
    step (s, m) mrg = let s' = S.insert (S.findMin mrg) s
                          m' = Merge.insert mrg m
                      in s' `seq` m' `seq` (s', m')


traceShow' :: Show a => [Char] -> a -> a
traceShow' cs x = trace (cs ++ ": " ++ show x) x


putFst :: Monad m => s1 -> StateT (s1, s2) m ()
putFst x = modify (\ (_, y) -> (x, y))
-- putSnd y = modify (\ (x, _) -> (x, y))


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
      ++ go1 ( M.alter (nothingIf M.null . flip M.difference srcM =<<) mini
             $ M.foldrWithKey' adjust m srcM )

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


likelihoodDelta
  :: (Ord l, Ord v)
  => CRTG v l
  -> Merge v
  -> (Log Double, (Log Double, Log Double, Log Double), (Int, Int, Int))
likelihoodDelta CRTG{..} = \ mrgs ->
  let (rw, rc) = productAndSum  -- rules
               $ map ( (\ (pr, su, si) -> (p su / pr, si))
                     . productPAndSumAndSize
                     )
               $ filter notSingle
               $ M.elems
               $ M.fromListWith (++)
               $ map ((mergeRule mrgs *** (: [])) . (ruleA A.!))
               $ (IS.toList . IS.fromList)
               $ concat
               $ M.elems
               $ M.intersection stateToRuleIxM (Merge.forward mrgs)
      (vw, vc) = productAndSum  -- states
               $ map ( (\ (pr, su, si) -> (pr / p su, si))
                     . productPAndSumAndSize
                     . map (getCnt cntState)
                     . S.toList
                     )
               $ Merge.equivalenceClasses mrgs

      (iw, ic) = productAndSum  -- initial states
               $ map ( (\ (pr, su, si) -> (p su / pr, si))
                     . productPAndSumAndSize
                     . M.elems
                     )
               $ filter ((1 <) . M.size)
               $ map (M.intersection cntInit . M.fromSet (const ()))
               $ Merge.equivalenceClasses mrgs
  in (rw * vw * iw, (rw, vw, iw), (rc, vc, ic))
  where
    ruleA = A.listArray (0, M.size cntRule - 1) (M.assocs cntRule)

    stateToRuleIxM = M.fromListWith (++) $ concatMap step $ A.assocs ruleA
      where step (i, (Rule v vs _, _)) = map (\ v' -> (v', [i]))
                                       $ (S.toList . S.fromList) (v : vs)

    notSingle [_] = False
    notSingle  _  = True

    -- | power with itself
    p :: Double -> Log Double
    p x = Exp (x * log x)  -- = Exp (log (x ** x))

    productAndSum :: [(Log Double, Int)] -> (Log Double, Int)
    productAndSum = foldl' step (1, 0)
      where step (a1, a2) (b1, b2) = strictPair (a1 * b1) (a2 + b2)

    productPAndSumAndSize :: [Count] -> (Log Double, Count, Int)
    productPAndSumAndSize = foldl' step (1, 0, -1)
      where step (a1, a2, a3) b = strictTriple (a1 * p b) (a2 + b) (succ a3)

    strictPair :: a -> b -> (a, b)
    strictPair x y = x `seq` y `seq` (x, y)

    strictTriple :: a -> b -> c -> (a, b, c)
    strictTriple x y z = x `seq` y `seq` z `seq` (x, y, z)

    getCnt m k = M.findWithDefault 0 k m


-- needed from test suite
ruleEquivalenceClasses
  :: (Ord l, Ord v) => BidiStar v l -> Merge v -> Map (Rule v l) [Rule v l]
ruleEquivalenceClasses g mrgs
  = M.filter notSingle
  $ M.fromListWith (++)
  $ map (\ r -> (mergeRule mrgs r, [r]))
  $ (S.toList . S.fromList)
  $ concat
  $ M.elems
  $ M.intersection g (Merge.forward mrgs)
  where
    notSingle [_] = False
    notSingle  _  = True


mergeRule :: Ord v => Merge v -> Rule v l -> Rule v l
mergeRule mrgs
  = \ Rule{..} -> Rule (mrg to) (map mrg from `using` evalList rseq) label
  where mrg = Merge.apply mrgs


mergeRuleMaybe :: Ord v => Merge v -> Rule v l -> Maybe (Rule v l)
mergeRuleMaybe mrgs Rule{..}
  = if any isJust (to' : from')
    then Just (Rule (fromMaybe to to') (zipWith fromMaybe from from') label)
    else Nothing
  where mrg = Merge.applyMaybe mrgs
        to' = mrg to
        from' = map mrg from


mergeCRTG :: (Ord l, Ord v) => Merge v -> CRTG v l -> CRTG v l
mergeCRTG mrgs CRTG{..}
  = CRTG
      (mapSomeKeysWith (+) (mergeRuleMaybe mrgs) cntRule)
      (mergeKeysWith (+) mrgs cntState)
      (mergeKeysWith (+) mrgs cntInit)


-- | Similar to 'M.mapKeysWith', but optimized for merging, because many keys
-- are left unchanged.
mergeKeysWith
  :: Ord k => (a -> a -> a) -> Merge k -> M.Map k a -> M.Map k a
mergeKeysWith (?) mrgs
  = uncurry (M.unionWith (?))
  . first (M.mapKeysWith (?) (Merge.apply mrgs))
  . M.partitionWithKey (\ k _ -> Merge.member k mrgs)


-- | Similar to 'M.mapKeysWith', but keys mapped to 'Nothing' are left
-- unchanged. If every key is mapped to 'Nothing', runtime is in /O(n)/.
mapSomeKeysWith
  :: Ord k => (a -> a -> a) -> (k -> Maybe k) -> Map k a -> Map k a
mapSomeKeysWith (?) f m
  = M.unionWith (?) unchanged
  $ M.mapKeysWith
      (?)
      (fromMaybe (errorHere "mapSomeKeysWith" "unexpected pattern") . f)
      todo
  where
    p k _ = isNothing (f k)
    (unchanged, todo) = M.partitionWithKey p m


whileM :: Monad m => m Bool -> m a -> m ()
whileM cond act = do
  b <- cond
  case b of
    True  -> act >> whileM cond act
    False -> return ()
