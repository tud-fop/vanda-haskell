{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Grammar.XRS.LCFRS.Chart
  ( -- * top-level functions to solve deduction systems
    chartify
  , parseTrees
    -- * types and constructors
  , Chart
  , ChartRule
  , Backtrace(..)
  , empty
    -- * chart operations
  , lookupWith
  , insert
  ) where

import Control.Monad (unless)
import Control.Monad.State (State, execState, get, put)
import Data.Either (rights, lefts)
import Data.Hashable (Hashable)
import Data.List (foldl', sortOn)
import Data.Maybe (maybeToList)
import Data.Range (Rangevector)
import Data.Semiring
import Data.Tree (Tree(Node))
import Vanda.Grammar.PMCFG (Rule(Rule))

import qualified Data.LimitedQueue as Q
import qualified Data.HashMap.Lazy as Map


-- | Backtrace storing the applied grammar rule and range vectors for producing a passive item.
data Backtrace nt t wt = Backtrace (Rule nt t) wt [Rangevector] deriving (Show)


instance (Eq nt, Eq t) => Eq (Backtrace nt t wt) where
  (Backtrace r _ rv) == (Backtrace r' _ rv') = r == r' && rv == rv'


-- | Chart for passive parsing items implemented by a two-step map: N -> RV -> Backtrace x W.
type Chart nt t wt = Map.HashMap nt (Map.HashMap Rangevector ([Backtrace nt t wt], wt))


-- | Initializes an empty chart for passive elements.
empty :: Chart nt t wt
empty = Map.empty


-- | Finds all passive items in chart by their nonterminal.
lookupWith :: (Eq nt, Hashable nt)
           => (nt -> Rangevector -> Backtrace nt t wt -> wt -> it) -- ^ item factory function
           -> Chart nt t wt                                        -- ^ chart of passive items
           -> nt                                                   -- ^ nonterminal
           -> [it]                                                 -- ^ list of found items
lookupWith con passives nta
  = [ con nta rv bt w
    | rvmap <- maybeToList $ Map.lookup nta passives
    , (rv, (bt:_, w)) <- Map.toList rvmap
    ]


-- | Inserts a passive items into the chart, returns updated chart and if it was the first item N x RV in the chart.
insert :: (Hashable nt, Eq nt) 
       => Chart nt t wt         -- ^ chart of passive items
       -> nt                    -- ^ passive item's nonterminal
       -> Rangevector           -- ^ passive item's range vector
       -> Backtrace nt t wt     -- ^ backtrace
       -> wt                    -- ^ weight
       -> (Chart nt t wt, Bool) -- ^ updated chart
insert passives a rho bt iw
  = case Map.lookup a passives
    of Nothing -> (Map.insert a (Map.singleton rho ([bt], iw)) passives, True)
       Just pa -> case Map.lookup rho pa
                  of Nothing -> (Map.adjust (Map.insert rho ([bt], iw)) a passives, True)
                     Just (bts, w) -> (Map.adjust (Map.insert rho (bt:bts, w)) a passives, False)


-- | Reads all derivation trees off a chart that produce a passive item (s, rv).
parseTrees :: forall nt t wt.(Eq nt, Hashable nt, Semiring wt, Ord wt)
           => Int                 -- ^ maximum amount of parse trees
           -> [nt]                -- ^ initial nonterminals
           -> Rangevector         -- ^ complete range vector of word to parse (0, |w|)
           -> Chart nt t wt       -- ^ the chart to read of
           -> [Tree (Rule nt t)]  -- ^ parse trees
parseTrees tops ss rv chart = fst <$> take tops (sortOn snd $ ss >>= parseTrees' rv)
  where
    -- order chart entries by weight (reverse) and prune list of back traces
    c' = ((\ (bts, wt) -> (take tops $ reverse bts, wt)) <$>) <$> chart

    parseTrees' :: Rangevector -> nt -> [(Tree (Rule nt t), wt)]
    parseTrees' rho a
      = take tops $ sortOn snd [ (Node r ds, weight)
                               | amap <- maybeToList $ a `Map.lookup` c'
                               , (bts, _) <- maybeToList $ rho `Map.lookup` amap
                               , (Backtrace r@(Rule ((_,as),_)) wr rhos) <- bts
                               , children <- sequence $ uncurry parseTrees' <$> zip rhos as
                               , let (ds, ws) = unzip children
                                     weight = wr <.> foldl' (<.>) one ws
                               ]


-- | Deduction rule consists of a function using a trigger item and
-- all explored items to return all deducable consequences.
type ChartRule it wt ct = Either [(it, wt)] (it -> ct -> [(it, wt)])


-- | Builds a chart using a list of rules. Executes modified Knuth algorithm
-- using the container for the set of explored items, an update function for it,
-- the set of deduction rules and approximation parameters.
chartify :: (Eq it, Ord wt, Semiring wt, Hashable it) 
      => ct                       -- ^ initial contianer
      -> (ct -> it -> (ct, Bool)) -- ^ update container
      -> [ChartRule it wt ct]     -- ^ deduction rules
      -> Int                      -- ^ beam width
      -> Int                      -- ^ maximum amount of chart entries per cell
      -> ct                       -- ^ final chart
chartify container update rules beam _
  = let initialitems = concat $ filter ((< zero) . snd) <$> lefts rules
    in snd $ execState (chartIteration (rights rules) update) 
                       (Q.fromList beam initialitems, container)


chartIteration :: (Eq it, Ord wt, Semiring wt, Hashable it) 
               => [it -> ct -> [(it, wt)]]
               -> (ct -> it -> (ct, Bool))
               -> State (Q.Queue it wt, ct) ()
chartIteration rules update
  = do (agenda, container) <- get
       unless (Q.null agenda)
        $ do let (agenda', item) = Q.deq agenda
                 (container', isnew) = update container item
             if isnew
                then do let agenda'' = Q.enqList agenda'
                                     $ filter ((< zero) . snd) 
                                     $ chartStep item container rules
                        put (agenda'', container')
                        chartIteration rules update
                else do put (agenda', container')
                        chartIteration rules update


chartStep :: it 
          -> ct
          -> [it -> ct -> [(it, wt)]]
          -> [(it, wt)]
chartStep trigger container rs = [ item
                                 | f <- rs
                                 , item <- f trigger container
                                 ]