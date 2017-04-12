module Vanda.Grammar.PMCFG.Chart
  ( -- * types and constructors
    Chart
  , ChartRule
  , Backtrace(..)
  , empty
    -- * chart operations
  , lookupWith
  , insert
  , chart
  , parseTrees
  ) where

import Control.Monad (unless)
import Control.Monad.State (State, execState, get, put)
import Vanda.Grammar.PMCFG (Rule(Rule))
import Vanda.Grammar.PMCFG.Range (Rangevector)
import Data.Either (rights, lefts)
import Data.Hashable (Hashable)
import Data.List (foldl', sortOn)
import Data.Maybe (maybeToList)
import Data.Semiring
import Data.Tree (Tree(Node))

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


-- | Finds passive items in chart by their nonterminal.
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
insert :: (Semiring wt, Hashable nt, Eq nt) 
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
                     Just (bts, w) -> (Map.adjust (Map.insert rho (bt:bts, w <+> iw)) a passives, False)


-- | Reads all derivation trees off a chart that produce a passive item (s, rv).
parseTrees :: (Eq nt, Hashable nt, Semiring wt, Ord wt)
           => Int
           -> [nt]
           -> Rangevector
           -> Chart nt t wt
           -> [Tree (Rule nt t)]
parseTrees tops ss rv c = map fst $ take tops $ sortOn snd $ ss >>= parseTrees' tops c' rv
  where
    c' = Map.map (\ m -> Map.map (\ (bts, wt) -> (take tops $ reverse bts, wt)) m) c
    parseTrees' :: (Eq nt, Hashable nt, Semiring wt, Ord wt) 
                =>Int -> Chart nt t wt -> Rangevector -> nt -> [(Tree (Rule nt t), wt)]
    parseTrees' tops chart rho a
      = take tops $ sortOn snd [ (Node r ds, weight)
                               | amap <- maybeToList $ a `Map.lookup` chart
                               , (bts, _) <- maybeToList $ rho `Map.lookup` amap
                               , (Backtrace r@(Rule ((_,as),_)) wr rhos) <- bts
                               , children <- sequence $ uncurry (parseTrees' tops chart) <$> zip rhos as
                               , let (ds, ws) = unzip children
                                     weight = wr <.> foldl' (<.>) one ws
                               ]


-- | Deduction rule consists of a function
type ChartRule it wt ct = Either [(it, wt)] (it -> ct -> [(it, wt)])


-- | Builds a chart using a list of rules
chart :: (Eq it, Ord wt, Semiring wt, Hashable it) 
      => ct                       -- ^ initial contianer
      -> (ct -> it -> (ct, Bool)) -- ^ update container
      -> [ChartRule it wt ct]     -- ^ deduction rules
      -> Int                      -- ^ beam width
      -> Int                      -- ^ maximum amount of chart entries per cell
      -> ct                       -- ^ final chart
chart container update rules beam tops
  = snd $ execState (chartIteration (rights rules) update) 
                    (Q.fromList beam (concat $ lefts rules), container)


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
                                     $ filter ((/= zero) . snd) 
                                     $ chartStep item container' rules
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