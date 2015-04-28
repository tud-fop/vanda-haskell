-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.PBSM.Types
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

module Vanda.PBSM.Types
( RTG ()
, Rule (..)
, rtg
, ruleM, rules, rulesRanked, ruleS, ruleSRanked
, terminalS, terminalSRanked
, nonterminalS, nonterminals
, mapTerminals
, mapNonterminals, mapNonterminals'
, initialS, initials
, intifyNonterminals
, toHypergraph, toHypergraphRanked
, toHypergraphStartSeparated, toHypergraphStartSeparatedRanked
, language, languages
) where


import Vanda.Hypergraph
import qualified Data.Queue as Q

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Control.DeepSeq (NFData (), rnf)
import Control.Monad.State
import Control.Seq
import qualified Data.Binary as B
import Data.Function (on)
import Data.List
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Tree


data RTG n t = RTG
  { initialS :: S.Set n
  , ruleM    :: M.Map (t, Int) (M.Map n (S.Set [n]))
  } deriving Show


instance (B.Binary n, B.Binary t) => B.Binary (RTG n t) where
  put (RTG inis rM) = B.put inis >> B.put rM
  get = RTG <$> B.get <*> B.get


instance (NFData n, NFData t) => NFData (RTG n t) where
  rnf (RTG nS rM) = rnf nS `seq` rnf rM

seqRTG
  :: Strategy (S.Set n)
  -> Strategy (M.Map (t, Int) (M.Map n (S.Set [n])))
  -> Strategy (RTG n t)
seqRTG strat1 strat2 (RTG iniS rM) = strat1 iniS `seq` strat2 rM


rtg :: (Ord n, Ord t) => [n] -> [Rule n t] -> RTG n t
rtg inis
  = RTG (S.fromList inis)
  . M.fromListWith (M.unionWith S.union)
  . map (\ (Rule n t ns) -> ((t, length ns), M.singleton n (S.singleton ns)))


initials :: RTG n t -> [n]
initials = S.toList . initialS


rules :: RTG n t -> [Rule n t]
rules = rulesHelper fst

rulesRanked :: RTG n t -> [Rule n (t, Int)]
rulesRanked = rulesHelper id

rulesHelper :: ((t, Int) -> t') -> RTG n t -> [Rule n t']
rulesHelper f g
  = [ Rule n (f t) ns
    | (t, m) <- M.toList (ruleM g)
    , (n, nsS) <- M.toList m
    , ns <- S.toList nsS
    ]


ruleS :: (Ord n, Ord t) => RTG n t -> S.Set (Rule n t)
ruleS = ruleSHelper fst

ruleSRanked :: (Ord n, Ord t) => RTG n t -> S.Set (Rule n (t, Int))
ruleSRanked = ruleSHelper id

ruleSHelper
  :: (Ord n, Ord t') => ((t, Int) -> t') -> RTG n t -> S.Set (Rule n t')
ruleSHelper f g
  = S.unions
      [ S.mapMonotonic (Rule n (f t)) nsS
      | (t, m) <- M.toList (ruleM g)
      , (n, nsS) <- M.toList m
      ]


data Rule n t
  = Rule { lhs :: n, lab :: t, succs :: [n] }
  deriving (Eq, Ord, Show)


instance (NFData n, NFData t) => NFData (Rule n t) where
  rnf (Rule n t ns) = rnf n `seq` rnf t `seq` rnf ns


instance Ord a => Ord (Tree a) where
  compare (Node x1 ts1) (Node x2 ts2)
    = case compare x1 x2 of
        EQ -> compare ts1 ts2
        o -> o


terminalSRanked :: RTG n t -> S.Set (t, Int)
terminalSRanked = M.keysSet . ruleM

terminalS :: Eq t => RTG n t -> S.Set t
terminalS = S.fromAscList . map fst . S.toAscList . terminalSRanked


nonterminals :: Ord n => RTG n t -> [n]
nonterminals = S.toList . nonterminalS


nonterminalS :: Ord n => RTG n t -> S.Set n
nonterminalS g
  = S.fromList $ do
      M.elems (ruleM g)
      >>= M.toList
      >>= (\ (n, nsS) -> n : concat (S.toList nsS))


mapTerminals :: (Ord n, Ord t') => (t -> t') -> RTG n t -> RTG n t'
mapTerminals f (RTG inS rM)
  = RTG inS
  $ M.mapKeysWith (M.unionWith S.union) (first f) rM


mapNonterminals :: (Ord m, Ord n, Ord t) => (m -> n) -> RTG m t -> RTG n t
mapNonterminals f (RTG inS rM)
  = RTG (S.map f inS)
  $ M.map (M.mapKeysWith S.union f . M.map (S.map (map f))) rM


mapNonterminals' :: (Ord m, Ord n, Ord t) => (m -> n) -> RTG m t -> RTG n t
mapNonterminals' f g
  = mapNonterminals f g
  `using`
    seqRTG
      (seqFoldable rseq)
      (seqMap r0 (seqMap rseq (seqFoldable (seqList rseq))))


intifyNonterminals :: (Ord n, Ord t) => RTG n t -> (RTG Int t, M.Map n Int)
intifyNonterminals g
  = (mapNonterminals intify g, mapping)
  where
    intify n = ML.findWithDefault
      (errorModule "intifyNonterminals: This must not happen.")
      n
      mapping
    mapping = M.fromList $ flip zip [1 ..] $ S.toList $ nonterminalS g


toHypergraph :: (Enum i, Num i, Ord n, Hypergraph h) => RTG n t -> h n t i
toHypergraph = toHypergraphHelper fst

toHypergraphRanked
  :: (Enum i, Num i, Ord n, Hypergraph h) => RTG n t -> h n (t, Int) i
toHypergraphRanked = toHypergraphHelper id

toHypergraphHelper
  :: (Enum i, Num i, Ord n, Hypergraph h)
  => ((t, Int) -> l) -> RTG n t -> h n l i
toHypergraphHelper f g
  = mkHypergraph $ zipWith toHyperedge [0 ..] $ rulesHelper f g


toHypergraphStartSeparated
  :: (Enum i, Num i, Ord n, Hypergraph h) => n -> t -> RTG n t -> h n t i
toHypergraphStartSeparated = toHypergraphStartSeparatedHelper fst

toHypergraphStartSeparatedRanked
  :: (Enum i, Num i, Ord n, Hypergraph h)
  => n -> t -> RTG n t -> h n (t, Int) i
toHypergraphStartSeparatedRanked
  = toHypergraphStartSeparatedHelper id

toHypergraphStartSeparatedHelper
  :: (Enum i, Num i, Ord n, Hypergraph h)
  => ((t, Int) -> l) -> n -> t -> RTG n t -> h n l i
toHypergraphStartSeparatedHelper f n t g
  = if n `S.member` nonterminalS g
    then errorModule "toHypergraphStartSeparated: nonterminal already exists"
    else mkHypergraph
       $ zipWith toHyperedge [0 ..]
       $ [Rule n (f (t, 1)) [ini] | ini <- initials g] ++ rulesHelper f g


toHyperedge :: i -> Rule v l -> Hyperedge v l i
toHyperedge i (Rule v l vs) = mkHyperedge v vs l i


language :: Ord n => RTG n t -> [Tree t]
language g@(RTG nS _)
  = concat
  $ transpose
  $ map (\ n -> M.findWithDefault [] n (languages g)) (S.toList nS)


languages :: Ord n => RTG n t -> M.Map n [Tree t]
languages g = langM
  where
    langM = M.map (concat . transpose . map apply) ruleM'
    apply (Rule _ l ns)
      = map (Node l)
      $ combinations
      $ map (\ n -> M.findWithDefault [] n langM) ns
    ruleM'
      = M.map (sortBy (compare `on` length . succs))
      $ M.fromListWith (++) [(lhs r, [r]) | r <- rules g]


combinations :: [[a]] -> [[a]]
combinations yss
  = if any null yss then [] else evalState go $ Q.singleton (id, yss)
  where
    go = untilState Q.null $ do
            (prefix, xss) <- state Q.deq
            fillQueue prefix xss
            return $ prefix $ map head xss

    fillQueue :: ([a] -> [a]) -> [[a]] -> State (Q.Queue ([a] -> [a], [[a]])) ()
    fillQueue _ [] = return ()
    fillQueue prefix ((x : xs) : xss) = do
      unless (null xs) $ modify $ Q.enq (prefix, xs : xss)
      fillQueue (prefix . (x :)) xss
    fillQueue _ _ = errorModule "combinations: This must not happen."

    untilState predicate action = do
      x <- get
      if predicate x
        then return []
        else do
          y  <- action
          ys <- untilState predicate action
          return (y : ys)


errorModule :: String -> a
errorModule = error . ("Vanda.PBSM.Types." ++)
