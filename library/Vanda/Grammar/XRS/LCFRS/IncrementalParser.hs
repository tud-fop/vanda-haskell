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
                    (Vector (R.RvMem))
                    (U.Vector w) deriving (Show)


instance (Eq nt, Eq t, U.Unbox nt) => Eq (Item nt t wt) where
  (Active cr r ri left right completions _) == (Active cr' r' ri' left' right' completions' _) 
    =  {-# SCC "ItemEQ" #-} r           == r' 
    && ri          == ri'
    && left        == left'
    && right       == right'
    && cr          == cr'
    && completions == completions'

instance (Hashable nt, Hashable t, U.Unbox nt) => Hashable (Item nt t wt) where
  salt `hashWithSalt` (Active _ r ri left _ _ _) 
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
         , let completes = R.withCapacity (fanouts Map.! a)
               component = c ! 0
               combines = (R.withCapacity . (fanouts Map.!)) `map` U.convert as
               weights = (fst . (iow Map.!)) `U.map` as
         , item <- implicits word (Active completes r 0 R.Epsilon component combines weights)
         ]


implicits :: (Eq t, Show t)
          => [t]
          -> Item n t w
          -> [Item n t w]
implicits w item@(Active completions r@(VecRule _ c _ _) index left [] combines weights)
  | index == length c - 1 = [item]
  | otherwise
    = item : [ item'
             | let completions' = completions R.// (index, left)
                   index' = index + 1
             , item' <- implicits w (Active completions' r index' R.Epsilon (c ! index') combines weights)
             ]
implicits w (Active completions r index left (T t : ts) combines weights)
  = [ item
    | left' <- mapMaybe (R.safeConc left) $ R.singletons t w
    , item <- implicits w (Active completions r index left' ts combines weights)
    ]
implicits _ item = [item]


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
    app (Active _ (VecRule _ _ nts w) _ _ (Var i 0 : _) _ _) (_, inits, _, _, _)
      = [ (item, w <.> U.foldl' (<.>) one weights <.> snd (iow Map.! a))  
        | let a' = nts U.! i
        , a' `Set.member` inits
        , r@(VecRule a c as w) <- rs Map.! a'
        , let completes = R.withCapacity (fanouts Map.! a)
              component = c ! 0
              combines = (R.withCapacity . (fanouts Map.!)) `map` U.convert as 
              weights = (fst . (iow Map.!)) `U.map` as
        , item <- implicits word (Active completes r 0 R.Epsilon component combines weights)
        ]
    app _ _ = []

combineRule :: forall nt t wt. (Show nt, Show t, Show wt, Hashable nt, Eq nt, Eq t, Weight wt, U.Unbox nt, U.Unbox wt)
    => [t]
    -> Map.HashMap nt (wt, wt)
    -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
combineRule word iow = Right app
  where
    app :: Item nt t wt -> Container nt t wt -> [(Item nt t wt, wt)]
    app trigger@(Active _ (VecRule _ _ as _) _ _ (Var i j : _) _ _) (_, _, _, _, k)
     =  [ (item', weight)
        | chartItem <- MMap.lookup ((as U.! i), j) k
        , (item, weight) <- consequences trigger chartItem
        , item' <- implicits word item
        ]
    app trigger@(Active _ (VecRule a _ _ _) i _ [] _ _) (_, _, _, s, _)
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
    consequences (Active cr rule@(VecRule a _ _ wt) ri rho (Var i j : right) combines weights) 
                 (Active crf (VecRule _ _ _ wt') _ rho' _ _ weights')
        = [ (item, wt <.> U.foldl' (<.>) one weights'' <.> (snd $ iow Map.! a))
          | (combines ! i) `R.subsetEq` crf
          , left <- maybeToList $ R.safeConc rho rho'
          , let combines' = accum (\ sa  () -> sa R.// (j, rho')) combines [(i, ())]
                weights'' = weights U.// [(i, wt' <.> U.foldl' (<.>) one weights')]
          , item <- implicits word (Active cr rule ri left right combines' weights'')
          ]
    consequences _ _ = []

   
-- Update Function for Container
{-# SCC update #-}
update :: (Show nt, Show t, Show wt, Eq nt, Eq t, Eq wt, Hashable nt, Semiring wt, U.Unbox nt, U.Unbox wt)
       => Container nt t wt
       -> Item nt t wt
       -> (Container nt t wt, Bool)
update (p, n, intermediates, s, k) item@(Active cr r@(VecRule a c _ wt) index left [] combines weights)
  | index == length c - 1 = case R.intoRangevector (cr R.// (index, left)) of
                                 Just crv -> case sequence $ R.intoRangevector <$> toList combines of 
                                                  Just rvs -> case C.insert p a crv (C.Backtrace (fromVecRule r) wt rvs) (wt <.> U.foldl' (<.>) one weights) of 
                                                                   (p', isnew) -> ((p', n, intermediates, s, MMap.insert (a, index) item k), isnew)
                                                  Nothing -> ((p, n, intermediates, s, k), False)
                                 Nothing -> ((p, n, intermediates, s, k), False)
  | otherwise = let cr' = cr R.// (index, left)
                    new = not $ (a, cr', index) `Set.member` intermediates
                    intermediates' = if new then (a, cr', index) `Set.insert` intermediates else intermediates
                in ((p, n, intermediates', s, MMap.insert (a, index) item k), new)
update (p, n, intermediates, s, k) item@(Active _ (VecRule _ _ as _) _ _ (Var i j : _) _ _)
        = let a = as U.! i
              n' = if j == 0 then a `Set.delete` n else n
          in ((p, n', intermediates, MMap.insert (a, j) item s, k), True)
update t _ = (t, False)

