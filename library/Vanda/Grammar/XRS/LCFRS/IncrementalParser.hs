{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Grammar.XRS.LCFRS.IncrementalParser
  ( parse,
    parse',
    Container
  ) where

import Data.Hashable (Hashable(hashWithSalt))
import Data.Converging (Converging)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList,catMaybes, isNothing)
import Data.Range
import Data.Semiring
import Data.Tree (Tree)
import Data.Weight
import Vanda.Grammar.PMCFG

import qualified Data.HashMap.Lazy             as Map
import qualified Data.MultiHashMap             as MMap
import qualified Data.IntMap                   as IMap
import qualified Data.HashSet                  as Set
import qualified Vanda.Grammar.XRS.LCFRS.Chart as C

data Item nt t wt = Active (IMap.IntMap Range) (Rule nt t) wt Int Range [VarT t] [(Int, [VarT t])] (IMap.IntMap (IMap.IntMap Range)) (IMap.IntMap wt) deriving (Show) 

instance (Eq nt, Eq t) => Eq (Item nt t wt) where
  (Active cr r _ ri left right nc completions _) == (Active cr' r' _ ri' left' right' nc' completions' _) 
    =  cr          == cr'
    && r           == r' 
    && ri          == ri'
    && left        == left'
    && right       == right'
    && nc          == nc'
    && completions == completions'

instance (Hashable nt, Hashable t) => Hashable (Item nt t wt) where
  salt `hashWithSalt` (Active _ r _ _ left _ _ _ _) 
    = salt `hashWithSalt` r `hashWithSalt` left

type Container nt t wt = ( C.Chart nt t wt -- Passive Items
                         , Set.HashSet nt -- Not initialized NTs
                         , MMap.MultiMap (nt, Int) (Item nt t wt) -- Map of all Items that need the Component of the NT (Key) as the next Token for Combine (Search Map)
                         , MMap.MultiMap (nt, Int) (Item nt t wt) -- Map of all Items that have the Component of the NT (Key) already completed (Known Map)
                         )

parse :: forall nt t wt.(Show nt, Show t, Show wt, Hashable nt, Hashable t, Eq t, Ord wt, Weight wt, Ord nt, Converging wt) 
      => WPMCFG nt wt t -- Grammar
      -> Int -- Beam Width
      -> Int -- Max. Amount of Parse Trees
      -> [t] -- Word
      -> [Tree (Rule nt t)]
parse g bw tops w = parse' (prepare g w) bw tops w

parse' :: forall nt t wt.(Show nt, Show t, Show wt, Hashable nt, Hashable t, Eq t, Ord wt, Weight wt, Ord nt, Converging wt) 
       => (MMap.MultiMap nt (Rule nt t, wt), Map.HashMap nt (wt,wt), [nt]) -- prepare Result (RuleMap NT-Rules, IO-Weights NTs, Start NTs)
       -> Int -- Beam Width
       -> Int -- Max. Amount of Parse Trees
       -> [t] -- Word
       -> [Tree (Rule nt t)]
parse' (rmap, iow, s') bw tops w
  = C.parseTrees tops s'
    (singleton $ entire w) -- Goal Item 
  $ (\ (e, _, _, _) -> e) -- parse Trees just needs passive Items from Container
  $ C.chartify (C.empty, nset, MMap.empty, MMap.empty) update rules bw tops
    where
      nset = Set.fromList $ filter (not . (`elem` s')) $ Map.keys rmap
      rules = (initialPrediction w (s' >>= (`MMap.lookup` rmap)) iow)
            : predictionRule w rmap iow
            : [combineRule w iow]

-- Prediction rule for rules of initial nonterminals.
initialPrediction :: forall nt t wt. (Hashable nt, Eq nt, Semiring wt, Eq t, Show nt, Show t, Show wt) 
                  => [t]
                  -> [(Rule nt t, wt)]
                  -> Map.HashMap nt (wt, wt)
                  -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
initialPrediction word srules iow 
  = Left [ (Active cr' r w ri' left' right' fs' IMap.empty insides, heuristic)  
      | (r@(Rule ((_, as), fs)), w) <- srules
      , let fsindex = prepareComps fs
      , let (f0: fRest) = fsindex
      , ((firstri, firstright), firstfs) <- allCombinations [] f0 fRest
      , (cr', ri', left', right', fs') <- completeComponentsAndNextTerminals word IMap.empty firstri Epsilon firstright firstfs
      , let insides = IMap.fromList $ zip [0..] (map (fst . (iow Map.!)) as)
      , let heuristic = w <.> calcInsideWeight insides
      ]

calcInsideWeight :: forall wt. (Semiring wt) => IMap.IntMap wt -> wt
calcInsideWeight insides = foldl (<.>) one (map snd (IMap.toList insides ))

-- give every component of a function its index
prepareComps :: Function t -> [(Int, [VarT t])]
prepareComps = zip [0..]

-- Get all Components of a function with all remaining Components in the second Item
allCombinations :: [(Int, [VarT t])]  -> (Int, [VarT t]) -> [(Int, [VarT t])] -> [((Int, [VarT t]),  [(Int, [VarT t])])]
allCombinations xs x [] = [(x, xs)]
allCombinations xs x y'@(y:ys) = (x, xs ++ y') : (allCombinations (x:xs) y ys)

-- If the next Token is a Terminal, replace it by all possible Ranges for the given word (Scan). Complete Components and take all Combinations of the remaining Components. Stop if next Token in Variable
completeComponentsAndNextTerminals :: (Eq t, Show t)
                    => [t] -- Word
                    -> IMap.IntMap Range -- Already Completed
                    -> Int -- Curr Index
                    -> Range -- Curr Left
                    -> [VarT t] -- Curr Right
                    -> [(Int, [VarT t])] -- Next Components
                    -> [(IMap.IntMap Range, Int, Range, [VarT t], [(Int, [VarT t])])] -- Completed Components with Index, curr Left, curr Index, next Functions
completeComponentsAndNextTerminals _ cr ri left [] [] = [(cr, ri, left, [], [])]
-- Complete Rule Part
completeComponentsAndNextTerminals w cr ri left [] allfs@((fi, f):fs) = (cr, ri, left, [], allfs) : --Need this Item, because we cannot use it for combine otherwise
    ([(cr', ri', Epsilon, right', fs') 
        | ((ri', right'), fs') <- allCombinations [] (fi, f) fs
        ] >>= (\(cr'', ri'', left'', right'', fs'') -> completeComponentsAndNextTerminals w cr'' ri'' left'' right'' fs''))
    where cr' = IMap.insert ri left cr
-- Scan Rule Part
completeComponentsAndNextTerminals w cr ri left (T t:rights) fs
    = [ left'
        | left' <- mapMaybe (safeConc left) $ singletons t w
       ] >>= (\left'' -> completeComponentsAndNextTerminals w cr ri left'' rights fs)
-- Item Has to be Combined in Next Step -> Stop this function
completeComponentsAndNextTerminals _ cr ri left right@((Var _ _):_) fs = [(cr, ri, left, right, fs)]
--  Prediction rule for rules of not initial nonterminals.
predictionRule :: forall nt t wt. (Hashable nt, Eq nt, Semiring wt, Eq t, Show nt, Show t, Show wt) 
                  => [t]
                  -> MMap.MultiMap nt (Rule nt t, wt)
                  -> Map.HashMap nt (wt, wt)
                  -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
predictionRule word rs iow = Right app
    where
        app :: Item nt t wt
            -> Container nt t wt 
            -> [(Item nt t wt, wt)]
        app (Active _ (Rule ((_, as), _)) _ _ _ (Var i _:_) _ _ _) (_, inits, _, _)
          = [ (Active cr' r w ri' left' right' fs' IMap.empty insides, heuristic)
          | let a = as !! i
          , a `Set.member` inits
          , (r@(Rule ((a', as'), fs)), w) <- MMap.lookup a rs
          , let fsindex = prepareComps fs
          , let (f0: fRest) = fsindex
          , ((firstri, firstright), firstfs) <- allCombinations [] f0 fRest
          , (cr', ri', left', right', fs') <- completeComponentsAndNextTerminals word IMap.empty firstri Epsilon firstright firstfs
          , let insides = IMap.fromList $ zip [0..] (map (fst . (iow Map.!)) as')
                outside = snd $ iow Map.! a'
                heuristic = w <.> (calcInsideWeight insides) <.> outside
          ]
        app _ _ = []


combineRule :: forall nt t wt. (Show nt, Show t, Show wt, Hashable nt, Eq nt, Eq t, Weight wt)
        => [t]
        -> Map.HashMap nt (wt, wt) -- weights
        -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
combineRule word iow = Right app
    where
        app :: Item nt t wt -> Container nt t wt -> [(Item nt t wt, wt)]
        app trigger (_, _, s, k)
         =  [(Active cr' r wt ri' left' right' fs' completions insides, heu)
           | (Active _ (Rule ((_, as), _)) _ _ _ (Var i j:_) _ _ _) <- [trigger] -- Just so that I can pattern match different patterns in the two lists
           , chartItem <- MMap.lookup ((as !! i), j) k
           , ((Active cr r wt ri left right fs completions insides), heu) <- consequences trigger chartItem
           , (cr', ri', left', right', fs') <- completeComponentsAndNextTerminals word cr ri left right fs
         ]
    -- Finish all Items that need the last completed Component of trigger as Var
         ++ [(Active cr' r wt ri' left' right' fs' completions insides, heu)
           | (Active _ (Rule ((a, _), _)) _ currri _ [] _ _ _) <- [trigger] -- Just so that I can pattern match different patterns in the two lists
           , chartItem <- MMap.lookup (a , currri) s
           , ((Active cr r wt ri left right fs completions insides), heu) <- (consequences chartItem trigger)
           , (cr', ri', left', right', fs') <- completeComponentsAndNextTerminals word cr ri left right fs
         ]
    
        consequences :: Item nt t wt -- searching Item
                        -> Item nt t wt -- insert Item
                        -> [(Item nt t wt, wt)]
        consequences (Active cr rule@(Rule ((_, as), _)) wt ri left ((Var i j):rights) fs completeds insidess) (Active crf (Rule ((a, _), _)) wtf ri' left' [] _ _ insidesf)
            = [(Active cr rule wt ri left'' rights fs completed' insides', heuristic) 
            | j == ri' -- Is component number right?
            , a == (as!!i) -- Is NT right?
            , isCompatible (IMap.toList $ fromMaybe IMap.empty (completeds IMap.!? i)) -- Are all Ranges for the insert NT that are used by the search Item  part of the insert Item? If no Ranges are used by the search Item right now then use empty map instead of Nothing
            , left'' <- maybeToList $ safeConc left left'
            , let completed' = doubleInsert completeds i j left' 
                  insides' = IMap.insert i (wtf <.> calcInsideWeight insidesf) insidess 
                  outside =  snd $ iow Map.! a
                  heuristic = wt <.> (calcInsideWeight insides') <.> outside
                ] 
                where isCompatible :: [(Int, Range)] -> Bool
                      isCompatible used = 
                        foldr (checkRange) True used
                      checkRange :: (Int, Range) -> Bool -> Bool
                      checkRange (compi, usedRange) acc = case crf  IMap.!? compi of
                          Just foundRange -> ((usedRange == foundRange) && acc)
                          Nothing -> False

        consequences _ _ = []

doubleInsert :: IMap.IntMap (IMap.IntMap Range) -> Int -> Int -> Range -> IMap.IntMap (IMap.IntMap Range)
doubleInsert m i j r = IMap.insertWith IMap.union i (IMap.singleton j r) m
   
update :: (Show nt, Show t, Show wt, Eq nt, Eq t, Eq wt, Hashable nt, Semiring wt) => Container nt t wt -> Item nt t wt -> (Container nt t wt, Bool)
update (p, n, s, k) item@(Active cr r@(Rule ((nt, _), _)) wt ri left [] [] completed insides) = -- Active Item is completely gone through
    case getRangevector cr' of
        Just crv ->  case getBacktrace r wt completed of 
            Just bt -> case C.insert p nt crv bt (calcInsideWeight insides) of 
                (p', isnew) -> ((p', n, s, MMap.insert (nt, ri) item k), isnew || (not $ item `elem` (MMap.lookup (nt, ri) k))) -- look if new in chart or in Known Map
            Nothing -> ((p, n, s, k), False)
        Nothing -> ((p, n, s, k), False)
    where cr' = IMap.insert ri left cr
update (p, n, s, k) item@(Active _ (Rule ((nt, _), _)) _ ri _ [] _ _ _) -- New Finished Component, but still unfinished components
        = ((p, n, s, MMap.insert (nt, ri) item k), (not $ item `elem` (MMap.lookup (nt, ri) k)))
update (p, n, s, k) item@(Active _ (Rule ((_, as),_)) _ _ _ (Var i j:_) _ _ _)
        = ((p, (as !! i) `Set.delete` n, MMap.insert((as !! i), j) item s, k), (not $ item `elem` (MMap.lookup (as !!i, j) s))) -- Is Item new in Search Map
update (p, n, s, k) _ = ((p, n, s, k), False)

getRangevector :: IMap.IntMap Range -> Maybe Rangevector
getRangevector cr = fromList $ map snd $ IMap.toAscList cr 

getBacktrace :: 
    Rule nt t
    -> wt
    -> IMap.IntMap (IMap.IntMap Range) -- Completions
    -> Maybe (C.Backtrace nt t wt)
getBacktrace rule iw completions = 
    case containsANothing rvs of
        Just rvs' -> Just $ C.Backtrace rule iw rvs'  -- Rangevector of every NT
        Nothing -> Nothing
    where rvs = [rv
                | rangesOfOneNT <- (IMap.elems completions)
                , let rv = fromList $ IMap.elems rangesOfOneNT
                ]

--Could each RV of every NT be succesfully calculated
containsANothing :: [Maybe Rangevector] -> Maybe [Rangevector]
containsANothing xs = case null $ filter isNothing xs of 
    True -> Just $ catMaybes xs 
    False -> Nothing
