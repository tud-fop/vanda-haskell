module PBSM.Types where


import Data.Hypergraph
import qualified Data.Queue as Q

import Control.Monad.State
import Data.Function (on)
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tree


type SForest a = S.Set (Tree a)
type NT = SForest String
type T = String

data RTG n t
  = RTG { initialS :: S.Set n, ruleS :: S.Set (Rule n t) }
--   = RTG { initialS :: S.Set n, rules :: [Rule n t] }
  deriving (Show)


initials :: RTG n t -> [n]
initials = S.toList . initialS


rules :: RTG n t -> [Rule n t]
rules = S.toList . ruleS
-- ruleS :: (Ord n, Ord t) => RTG n t -> S.Set (Rule n t)
-- ruleS = S.fromList . rules


data Rule n t
  = Rule { lhs :: n, lab :: t, succs :: [n] }
  deriving (Eq, Ord, Show)


instance Ord a => Ord (Tree a) where
  compare (Node x1 ts1) (Node x2 ts2)
    = case compare x1 x2 of
        EQ -> compare ts1 ts2
        o -> o


nonterminals :: Ord n => RTG n t -> S.Set n
nonterminals
  = S.fromList
  . concatMap (\ r -> lhs r : succs r)
  . rules


toHypergraph :: Ord v => RTG v l -> Hypergraph v l () Int
toHypergraph g
  = hypergraph $ map toHyperedge $ zip [0 ..] $ rules g
  where
    toHyperedge (i, Rule v l vs) = hyperedge v vs l () i


language :: Ord n => RTG n t -> [Tree t]
language g@(RTG nS _)
  = concat
  $ transpose
  $ map (\ n -> M.findWithDefault [] n (languages g)) (S.toList nS)


languages :: Ord n => RTG n t -> M.Map n [Tree t]
languages g = langM
  where
    langM = M.map (concat . transpose . map apply) ruleM
    apply (Rule _ l ns)
      = map (Node l)
      $ combinations
      $ map (\ n -> M.findWithDefault [] n langM) ns
    ruleM
      = M.map (sortBy (compare `on` length . succs))
      $ M.fromListWith (++) [(lhs r, [r]) | r <- rules g]


combinations :: [[a]] -> [[a]]
combinations yss
  = if any null yss then [] else evalState go $ Q.singleton $ (id, yss)
  where
    go = untilState Q.null $ do
            (prefix, xss) <- state Q.deq
            fillQueue prefix xss
            return $ prefix $ map head xss

    fillQueue _ [] = return ()
    fillQueue prefix ((x : xs) : xss) = do
      unless (null xs) $ modify $ Q.enq (prefix, xs : xss)
      fillQueue (prefix . (x :)) xss
    fillQueue _ _ = error "PBSM.Types.combinations: This must not happen."

    untilState predicate action = do
      x <- get
      if predicate x
        then return []
        else do
          y  <- action
          ys <- untilState predicate action
          return (y : ys)


yield :: Tree a -> [a]
yield (Node x []) = [x]
yield (Node _ xs) = concatMap yield xs
