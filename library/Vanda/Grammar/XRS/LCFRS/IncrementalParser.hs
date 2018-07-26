-----------------------------------------------------------------------------
-- |
-- Module      :  IncrementalParser
-- Copyright   :  (c) Niklas Wünsche 2018
-- License     :  BSD-style
--
-- Maintainer  :  niklas.wuensche@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- In this module, you can find a function for parsing words using the incremental
-- parsing algorithm by Burden and Ljunglöf.
-- 'parse' uses a weighted 'PMCFG' ('WPMCFG') and returns a list of derivation trees ordered by minimal cost or maximum probability. Rule weights need to be instances of "Data.Semiring". parse' uses the output of 'Vanda.Grammar.PMCFG.prepare' rather than a 'WPMCFG'.
--
--
-- The parsing algorithm uses active items to represent
-- parsed parts of a word. They represent unfinished and finished derivations. 
-- To find all valid rule applications that generate a subword,
-- there are three different types of deductive rules applied until a
-- finished active item of the grammar rule is generated:
--
-- * initial prediction: An empty active item is generated for every component of all grammar rules that have a start symbol on their left sides.
-- * prediction: If an item needs to replace a component of a not yet seen nonterminal (NT) in the next step, generated an empty active Item for every component of all grammar rules that have this NT on their left side.
-- * combine: An unknown variable is replaced by the range of the
-- component of another active item if the variable and the current component of the other item match and the two items are compatible. For filling the chart later on, we store the range together with the variable's name. 
--
-- After every application of one of these deduction rules, the resulting items are processed (replace terminals with fitting ranges of the word, skip finished components). We stop when we find a variable and therefore the item has to be combined with another item in the next step, or we finished all components.
--
-- After a new processed item has been found, we look at its structure. We store the item in one "Data.MultiHashMap" if a component of the underlying rule has been completed and we store it in another "Data.MultiHashMap" if it needs a variable in the next step. This makes finding needed items easier when applying the deduction rules.  
--
-- If all components of the underlying rule have been completed, we store the 'Rangevector' of the components together with a 'Vanda.Grammar.XRS.LCFRS.Chart.Backtrace' consisting of the left hand side NT of the rule , the rule's weight and the 'Rangevector's of all NTs in a 'Vanda.Grammar.XRS.LCFRS.Chart'. This is used to generate the derivation trees afterwards by 'Vanda.Grammar.XRS.LCFRS.Chart.parseTrees'.
--
-----------------------------------------------------------------------------


{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Grammar.XRS.LCFRS.IncrementalParser
  ( parse,
    parse',
  ) where

import Data.Hashable (Hashable(hashWithSalt))
import Data.Converging (Converging)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList,catMaybes, isNothing)
import Data.Semiring
import Data.Tree (Tree)
import Data.Weight
import Prelude hiding (length, map)
import Vanda.Grammar.PMCFG

import qualified Data.HashMap.Lazy             as Map
import qualified Data.MultiHashMap             as MMap
import qualified Data.IntMap                   as IMap
import qualified Data.HashSet                  as Set
import qualified Vanda.Grammar.XRS.LCFRS.Chart as C

import Data.Vector hiding (update, filter, elem, head, mapMaybe, sequence)
import qualified Data.Vector.Unboxed as U

import qualified Data.BitMap as BM
import qualified Data.Range as R

data VecRule n t w = VecRule n (Vector [VarT t]) (U.Vector n) w deriving (Show)

instance (Eq n, Eq t, U.Unbox n) => Eq (VecRule n t w) where
  (VecRule n c as _) == (VecRule n' c' as' _) = {-# SCC "VecRuleEQ" #-} n == n' && as == as' && c == c'

instance (Hashable n, Hashable t, U.Unbox n) => Hashable (VecRule n t w) where
  salt `hashWithSalt` (VecRule a _ as _) = U.foldl' hashWithSalt (salt `hashWithSalt` a) as  

intoVecRule :: (U.Unbox n) => (Rule n t, w) -> VecRule n t w
intoVecRule (Rule ((a, as), c), w) = VecRule a (fromList c) (U.fromList as) w

fromVecRule :: (U.Unbox n) => (VecRule n t w) -> Rule n t
fromVecRule (VecRule a c as w) = Rule ((a, U.toList as), toList c)

data Item n t w = Active
                    R.RvMem
                    (VecRule n t w)
                    Int
                    R.Range
                    [VarT t]
                    BM.BitMap
                    (Vector (R.RvMem))
                    (U.Vector w) deriving (Show)

-- data Item nt t wt = Active 
--     (IMap.IntMap Range)                 -- Ranges of completed Component (Component Ranges)
--     (Rule nt t)                         -- Viewed Rule
--     wt                                  -- Weight of Viewed Rule
--     Int                                 -- Index of current Component (ri)
--     Range                               -- Found Range for first part of current Component (left)
--     [VarT t]                            -- part of the current Component that still has to be considered (right)
--     [(Int, [VarT t])]                   -- Unviewed Components of viewed Rule (Unfinished Components)
--     (IMap.IntMap (IMap.IntMap Range))   -- Map of Ranges I used Variable x y in some component, important for Backtrace in Chart
--     (IMap.IntMap wt)                    -- Current inside Weight/Heuristic of not completely used NTs
--     deriving (Show) 

instance (Eq nt, Eq t, U.Unbox nt) => Eq (Item nt t wt) where
  (Active cr r ri left right nc completions _) == (Active cr' r' ri' left' right' nc' completions' _) 
    =  {-# SCC "ItemEQ" #-} r           == r' 
    && ri          == ri'
    && left        == left'
    && right       == right'
    && cr          == cr'
    && nc          == nc'
    && completions == completions'

instance (Hashable nt, Hashable t, U.Unbox nt) => Hashable (Item nt t wt) where
  salt `hashWithSalt` (Active _ r ri left _ _ _ _) 
    = salt `hashWithSalt` r `hashWithSalt` ri `hashWithSalt` left

type Container nt t wt = ( C.Chart nt t wt
                         , Set.HashSet nt                           -- Not initialized NTs
                         , Set.HashSet (nt, R.RvMem, Int)           -- finished production for each nonterminal
                         , MMap.MultiMap (nt, Int) (Item nt t wt)   -- Map of all Items that need the Component of the NT (Key) as the next Token for Combine (Search Map)
                         , MMap.MultiMap (nt, Int) (Item nt t wt)   -- Map of all Items that have the Component of the NT (Key) already completed (Known Map)
                         )

-- | Main function in this module to parse a word using a 'WPMCFG'.
-- Returns a given number of derivations trees, ordered by their weight.
parse :: forall nt t wt.(Show nt, Show t, Show wt, Hashable nt, Hashable t, Eq t, Ord wt, Weight wt, Ord nt, Converging wt, U.Unbox nt, U.Unbox wt) 
      => WPMCFG nt wt t     -- Grammar
      -> Int                -- Beam Width
      -> Int                -- Max. Amount of Parse Trees
      -> [t]                -- Word
      -> [Tree (Rule nt t)]
parse g bw tops w = parse' (prepare g w) bw tops w

parse' :: forall nt t wt.(Show nt, Show t, Show wt, Hashable nt, Hashable t, Eq t, Ord wt, Weight wt, Ord nt, Converging wt, U.Unbox nt, U.Unbox wt) 
       => (MMap.MultiMap nt (Rule nt t, wt), Map.HashMap nt (wt,wt), [nt])  -- prepared Result (RuleMap NT-Rules, IO-Weights NTs, Start NTs)
       -> Int                                                               -- Beam Width
       -> Int                                                               -- Max. Amount of Parse Trees
       -> [t]                                                               -- Word
       -> [Tree (Rule nt t)]
parse' (rmap, iow, s') bw tops w
  = C.parseTrees tops s'
    (R.singleton $ R.entire w)                                                  -- Search for Item in Chart that has length of the word as its range
  $ (\ (e, _, _, _, _) -> e)                                                   -- parseTrees just needs the Chart of the Container
  $ C.chartify (C.empty, nset, Set.empty, MMap.empty, MMap.empty) update rules bw tops
    where
      nset = Set.fromList $ filter (not . (`elem` s')) $ Map.keys rmap
      rulemap = (fmap intoVecRule) <$> rmap
      fanouts = ((\ (VecRule _ c _ _) -> length c) . head) <$> rulemap
      rules = (initialPrediction w (s' >>= (`MMap.lookup` rulemap)) fanouts iow)
            : predictionRule w rulemap fanouts iow
            : [combineRule w iow]


parse'' :: forall nt t wt.(Show nt, Show t, Show wt, Hashable nt, Hashable t, Eq t, Ord wt, Weight wt, Ord nt, Converging wt, U.Unbox nt, U.Unbox wt) 
            => WPMCFG nt wt t     -- Grammar  -- prepared Result (RuleMap NT-Rules, IO-Weights NTs, Start NTs)
            -> Int                                                               -- Beam Width
            -> Int                                                               -- Max. Amount of Parse Trees
            -> [t]                                                               -- Word
            -> Container nt t wt
parse'' g bw tops w
       = C.chartify (C.empty, nset, Set.empty, MMap.empty, MMap.empty) update rules bw tops
         where
           (rmap, iow, s') = prepare g w
           nset = Set.fromList $ filter (not . (`elem` s')) $ Map.keys rmap
           rulemap = (fmap intoVecRule) <$> rmap
           fanouts = ((\ (VecRule _ c _ _) -> length c) . head) <$> rulemap
           rules = (initialPrediction w (s' >>= (`MMap.lookup` rulemap)) fanouts iow)
                 : predictionRule w rulemap fanouts iow
                 : [combineRule w iow]

-- Prediction rule for rules of start NTs.
initialPrediction :: forall nt t wt. (Hashable nt, Eq nt, Semiring wt, Eq t, Show nt, Show t, Show wt, U.Unbox nt, U.Unbox wt) 
                  => [t]
                  -> [VecRule nt t wt]
                  -> Map.HashMap nt Int
                  -> Map.HashMap nt (wt, wt)
                  -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
initialPrediction word srules fanouts iow 
  = Left [ (item, w <.> U.foldl' (<.>) one weights)  
         | r@(VecRule a c as w) <- srules
         , (index, bm') <- allCombinations $ BM.true (length c)
         , let completes = R.withCapacity (fanouts Map.! a)
               component = c ! index
               combines = (R.withCapacity . (fanouts Map.!)) `map` U.convert as
               weights = (fst . (iow Map.!)) `U.map` as
         , item <- implicits word (Active completes r index R.Epsilon component bm' combines weights)

        --  , let fsindex = prepareComps fs
        --  , let (f0: fRest) = fsindex
        --  , ((firstri, firstright), firstfs) <- allCombinations [] f0 fRest
        --  , (cr', ri', left', right', fs') <- completeComponentsAndNextTerminals word IMap.empty firstri Epsilon firstright firstfs 
        --        -- Don't have completed anything for this item yet, so give it empty component-ranges map and empty range for curr component. Try to Scan/Complete every component of the rule
        --  , let insides = IMap.fromList $ zip [0..] (map (fst . (iow Map.!)) as)
        --  , let heuristic = w <.> calcInsideWeight insides
         ]

-- calcInsideWeight :: forall wt. (Semiring wt) => IMap.IntMap wt -> wt
-- calcInsideWeight insides = foldl (<.>) one (map snd (IMap.toList insides ))

-- -- Store every component with its place in the Function
-- prepareComps :: Function t -> [(Int, [VarT t])]
-- prepareComps = zip [0..]

-- -- Get a Pair of all Components of a function with a List of all other Components
-- allCombinations :: [(Int, [VarT t])]  -> (Int, [VarT t]) -> [(Int, [VarT t])] -> [((Int, [VarT t]),  [(Int, [VarT t])])]
-- allCombinations xs x [] = [(x, xs)]
-- allCombinations xs x y'@(y:ys) = (x, xs ++ y') : (allCombinations (x:xs) y ys)

allCombinations :: BM.BitMap -> [(Int, BM.BitMap)]
allCombinations bm = [ (index, bm `BM.flip` index)
                     | index <- [0..(BM.length bm - 1)]
                     , bm BM.! index
                     ]

implicits :: (Eq t, Show t)
          => [t]
          -> Item n t w
          -> [Item n t w]
implicits w item@(Active completions r@(VecRule _ c _ _) index left [] bm combines weights)
  | BM.isFalse bm = [item]
  | otherwise
    = item : [ item'
             | (index', bm') <- allCombinations bm
             , let completions' = completions R.// (index, left)
             , item' <- implicits w (Active completions' r index' R.Epsilon (c ! index') bm' combines weights)
             ]
implicits w (Active completions r index left (T t : ts) bm combines weights)
  = [ item
    | left' <- mapMaybe (R.safeConc left) $ R.singletons t w
    , item <- implicits w (Active completions r index left' ts bm combines weights)
    ]
implicits _ item = [item]

-- -- If the next Token is a Terminal, replace it by all possible Ranges for the given word (Scan). Complete Components and take all Combinations of the remaining Components. Stop if next Token in Variable
-- completeComponentsAndNextTerminals :: (Eq t, Show t)
--                     => [t]                                                              -- Word
--                     -> IMap.IntMap Range                                                -- Component Ranges
--                     -> Int                                                              -- Curr Index
--                     -> Range                                                            -- Curr Left
--                     -> [VarT t]                                                         -- Curr Right
--                     -> [(Int, [VarT t])]                                                -- Next Components
--                     -> [(IMap.IntMap Range, Int, Range, [VarT t], [(Int, [VarT t])])]   -- updated Components Ranges, current Index and left and right, still unfinished Components
-- completeComponentsAndNextTerminals _ cr ri left [] [] = [(cr, ri, left, [], [])]
-- -- Complete Rule Part - If we finished the current Component, store it and call function again on all unfinished Components
-- completeComponentsAndNextTerminals w cr ri left [] allfs@((fi, f):fs) 
--     = (cr, ri, left, [], allfs) --need to store this Item because we need it to combine this component in other Items
--     : [ item
--       | ((ri', right'), fs') <- allCombinations [] (fi, f) fs
--       , item <- completeComponentsAndNextTerminals w cr' ri' Epsilon right' fs'
--       ]
--     where cr' = IMap.insert ri left cr
-- -- Scan Rule Part
-- completeComponentsAndNextTerminals w cr ri left (T t:rights) fs
--     = [ item
--       | left' <- mapMaybe (safeConc left) $ singletons t w
--       , item <- completeComponentsAndNextTerminals w cr ri left' rights fs
--       ]
-- -- Next Token is Variable -> Item has to be combined in next step -> Can't do more inside this function
-- completeComponentsAndNextTerminals _ cr ri left right@((Var _ _):_) fs = [(cr, ri, left, right, fs)]

--  Prediction rule for rules of not start NTs. Only predict them if another Item needs a Component of it in the next step and the Items of that NW weren't predicted before
-- predictionRule :: forall nt t wt. (Hashable nt, Eq nt, Semiring wt, Eq t, Show nt, Show t, Show wt) 
--                   => [t]
--                   -> MMap.MultiMap nt (Rule nt t, wt)
--                   -> Map.HashMap nt (wt, wt)
--                   -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
-- predictionRule word rs iow = Right app
--     where
--         app :: Item nt t wt
--             -> Container nt t wt 
--             -> [(Item nt t wt, wt)]
--         app (Active _ (Rule ((_, as), _)) _ _ _ (Var i _:_) _ _ _) (_, inits, _, _)
--           = [ (Active cr' r w ri' left' right' fs' IMap.empty insides, heuristic)
--           | let a = as !! i
--           , a `Set.member` inits
--           , (r@(Rule ((a', as'), fs)), w) <- MMap.lookup a rs
--           , let fsindex = prepareComps fs
--           , let (f0: fRest) = fsindex
--           , ((firstri, firstright), firstfs) <- allCombinations [] f0 fRest
--           , (cr', ri', left', right', fs') <- completeComponentsAndNextTerminals word IMap.empty firstri Epsilon firstright firstfs
--           , let insides = IMap.fromList $ zip [0..] (map (fst . (iow Map.!)) as')
--                 outside = snd $ iow Map.! a'
--                 heuristic = w <.> (calcInsideWeight insides) <.> outside
--           ]
--         app _ _ = []

predictionRule :: forall nt t wt. (Hashable nt, Eq nt, Semiring wt, Eq t, Show nt, Show t, Show wt, U.Unbox nt, U.Unbox wt) 
        => [t]
        -> MMap.MultiMap nt (VecRule nt t wt)
        -> Map.HashMap nt Int
        -> Map.HashMap nt (wt, wt)
        -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
predictionRule word rs fanouts iow = Right app
  where
    app :: Item nt t wt
        -> Container nt t wt 
        -> [(Item nt t wt, wt)]
    app (Active _ (VecRule _ _ nts w) _ _ (Var i _ : _) _ _ _) (_, inits, _, _, _)
      = [ (item, w <.> U.foldl' (<.>) one weights <.> snd (iow Map.! a))  
        | let a' = nts U.! i
        , a' `Set.member` inits
        , r@(VecRule a c as w) <- rs Map.! a'
        , (index, bm') <- allCombinations $ BM.true (length c)
        , let completes = R.withCapacity (fanouts Map.! a)
              component = c ! index
              combines = (R.withCapacity . (fanouts Map.!)) `map` U.convert as 
              weights = (fst . (iow Map.!)) `U.map` as
        , item <- implicits word (Active completes r index R.Epsilon component bm' combines weights)
        ]
    app _ _ = []

combineRule :: forall nt t wt. (Show nt, Show t, Show wt, Hashable nt, Eq nt, Eq t, Weight wt, U.Unbox nt, U.Unbox wt)
    => [t]
    -> Map.HashMap nt (wt, wt)
    -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
combineRule word iow = Right app
  where
    app :: Item nt t wt -> Container nt t wt -> [(Item nt t wt, wt)]
    app trigger@(Active _ (VecRule _ _ as _) _ _ (Var i j : _) _ _ _) (_, _, _, _, k)
     =  [ (item', weight)
        | chartItem <- MMap.lookup ((as U.! i), j) k
        , (item, weight) <- consequences trigger chartItem
        , item' <- implicits word item
        ]
    app trigger@(Active _ (VecRule a _ _ _) i _ [] _ _ _) (_, _, _, s, _)
        = [ (item', weight)
          | chartItem <- MMap.lookup (a , i) s
          , (item, weight) <- consequences chartItem trigger
          , item' <- implicits word item
          ]
    app _ _ = []

    consequences :: (U.Unbox nt, U.Unbox wt)
                 => Item nt t wt            -- searching Item
                 -> Item nt t wt         -- Item with finished component
                 -> [(Item nt t wt, wt)]
    consequences (Active cr rule@(VecRule a _ _ wt) ri rho (Var i j : right) bm combines weights) 
                 (Active crf (VecRule _ _ _ wt') _ rho' _ _ _ weights')
        = [ (item, wt <.> U.foldl' (<.>) one weights'' <.> (snd $ iow Map.! a))
          | (combines ! i) `R.subsetEq` crf
          , left <- maybeToList $ R.safeConc rho rho'
          , let combines' = accum (\ sa  () -> sa R.// (j, rho')) combines [(i, ())]
                weights'' = weights U.// [(i, wt' <.> U.foldl' (<.>) one weights')]
          , item <- implicits word (Active cr rule ri left right bm combines' weights'')
          ]
    consequences _ _ = []

-- combineRule :: forall nt t wt. (Show nt, Show t, Show wt, Hashable nt, Eq nt, Eq t, Weight wt)
--         => [t]
--         -> Map.HashMap nt (wt, wt) -- weights
--         -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
-- combineRule word iow = Right app
--     where
--         app :: Item nt t wt -> Container nt t wt -> [(Item nt t wt, wt)]
--     -- Combine trigger Item with all Items that already have completed the next variable of the trigger Item
--         app trigger@(Active _ (Rule ((_, as), _)) _ _ _ (Var i j:_) _ _ _) (_, _, _, k)
--          =  [(Active cr' r wt ri' left' right' fs' completions insides, heu)
--             | chartItem <- MMap.lookup ((as !! i), j) k
--             , ((Active cr r wt ri left right fs completions insides), heu) <- consequences trigger chartItem
--             , (cr', ri', left', right', fs') <- completeComponentsAndNextTerminals word cr ri left right fs
--             ]
--     -- Combine all Items that have the last completed component of the trigger Item as next Token
--         app trigger@(Active _ (Rule ((a, _), _)) _ currri _ [] _ _ _) (_, _, s, _)
--             = [(Active cr' r wt ri' left' right' fs' completions insides, heu)
--               | chartItem <- MMap.lookup (a , currri) s
--               , ((Active cr r wt ri left right fs completions insides), heu) <- (consequences chartItem trigger)
--               , (cr', ri', left', right', fs') <- completeComponentsAndNextTerminals word cr ri left right fs
--               ]
--         app _ _ = []

--         consequences :: Item nt t wt            -- searching Item
--                         -> Item nt t wt         -- Item with finished component
--                         -> [(Item nt t wt, wt)]
--         consequences (Active cr rule@(Rule ((nts, as), fsNT)) wt ri left ((Var i j):rights) fs completeds insidess) (Active crf (Rule ((a, _), _)) wtf ri' left' [] _ _ insidesf)
--             = [(Active cr rule wt ri left'' rights fs completed' insides', heuristic) 
--               | j == ri' -- Is component number right?
--               , a == (as!!i) -- Is NT right?
--               , all checkRange (IMap.toList $ fromMaybe IMap.empty (completeds IMap.!? i))
--                     -- Are all Ranges for the insert NT that are used by the search Item part of the completed Item? If no Ranges of the completed   Item are used by the search Item right now, use empty map instead of Nothing
--               , left'' <- maybeToList $ safeConc left left'
--               , let completed' = doubleInsert i j left' completeds
--                     insides' = IMap.insert i (wtf <.> calcInsideWeight insidesf) insidess -- Override old Insideweight for that NT with the newer one from the now used Item
--                     outside =  snd $ iow Map.! nts
--                     heuristic = wt <.> (calcInsideWeight insides') <.> outside
--               ] 
--                 where 
--                     checkRange :: (Int, Range) -> Bool
--                     checkRange (compi, usedRange) = case crf  IMap.!? compi of
--                                                          Just foundRange -> (usedRange == foundRange)
--                                                          Nothing -> False
--         consequences _ _ = []

-- -- Insert something inside a Map inside a Map
-- doubleInsert :: Int -> Int -> Range -> IMap.IntMap (IMap.IntMap Range) -> IMap.IntMap (IMap.IntMap Range)
-- doubleInsert i j r m = IMap.insertWith IMap.union i (IMap.singleton j r) m
   
-- Update Function for Container
{-# SCC update #-}
update :: (Show nt, Show t, Show wt, Eq nt, Eq t, Eq wt, Hashable nt, Semiring wt, U.Unbox nt, U.Unbox wt)
       => Container nt t wt
       -> Item nt t wt
       -> (Container nt t wt, Bool)
update (p, n, finished, s, k) item@(Active cr r@(VecRule a _ _ wt) index left [] bm combines weights)
  | BM.isFalse bm = let cr' = cr R.// (index, left)
                    in case R.intoRangevector cr' of
                      Just crv -> case sequence $ R.intoRangevector <$> toList combines of 
                                        Just rvs -> case C.insert p a crv (C.Backtrace (fromVecRule r) wt rvs) (wt <.> U.foldl' (<.>) one weights) of 
                                                        (p', isnew) -> let isnew' = isnew || not ((a, cr', index) `Set.member` finished)
                                                                           finished' = if isnew'
                                                                                          then (a, cr', index) `Set.insert` finished
                                                                                          else finished
                                                                       in ((p', n, finished', s, MMap.insert (a, index) item k), isnew')
                                        Nothing -> ((p, n, finished, s, k), False)
                      Nothing -> ((p, n, finished, s, k), False)
  | otherwise = let cr' = cr R.// (index, left)
                    new = (a, cr', index) `Set.member` finished
                    finished' = if new then (a, cr', index) `Set.insert` finished else finished
                in ((p, n, finished', s, MMap.insert (a, index) item k), new)
update (p, n, finished, s, k) item@(Active _ (VecRule _ _ as _) _ _ (Var i j:_) _ _ _)
        = let a = as U.! i
          in ((p, a `Set.delete` n, finished, MMap.insert (a, j) item s, k), True)
update (p, n, finished, s, k) _ = ((p, n, finished, s, k), False)
--
-- -- Try to create a Rangevector out of all found component Ranges
-- getRangevectors :: IMap.IntMap Range -> Maybe Rangevector
-- getRangevectors cr = fromList $ map snd $ IMap.toAscList cr 

-- Try to create a Backtrace out of the used Rule, together with its weight and the Ranges of all used Variables
-- getBacktrace :: Rule nt t
--              -> wt
--              -> IMap.IntMap (IMap.IntMap Range)                          -- Completions
--              -> Maybe (C.Backtrace nt t wt)
-- getBacktrace rule rw completions = 
--     case containsANothing rvs of
--         Just rvs' -> Just $ C.Backtrace rule rw rvs'            -- Rangevector of every NT
--         Nothing -> Nothing
--     where rvs = [rv 
--                 | rangesOfOneNT <- (IMap.elems completions)
--                 , let rv = fromList $ IMap.elems rangesOfOneNT
--                 ]

-- --Was each RV of every NT successfully created? If not, return Nothing.
-- containsANothing :: [Maybe Rangevector] -> Maybe [Rangevector]
-- containsANothing xs = case null $ filter isNothing xs of 
--                            True -> Just $ catMaybes xs 
--                            False -> Nothing
