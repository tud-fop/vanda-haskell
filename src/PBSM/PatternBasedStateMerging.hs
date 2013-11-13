module PBSM.PatternBasedStateMerging where


import PBSM.Types

import Data.Either (lefts, rights)
import Data.Function (on)
import Data.List (maximumBy, partition)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tree


forestToGrammar :: Ord t => Forest t -> RTG (SForest t) t
forestToGrammar corpus
  = rtg (map S.singleton corpus)
  $ concatMap go corpus
  where
    go :: Tree t -> [Rule (S.Set (Tree t)) t]
    go t@(Node x ts)
      = Rule (S.singleton t) x (map S.singleton ts)
      : concatMap go ts


unknownTerminals :: Ord t => RTG n t -> Tree t -> S.Set (t, Int)
unknownTerminals g tree
  = terminalsTrees S.\\ terminalsG
  where
    terminalsG
      = S.fromList $ map (\ (_, t, i) -> (t, i)) $ M.keys $ ruleM g
    terminalsTrees
      = S.fromList $ flattenWithRank tree
    flattenWithRank (Node x ts)
      = (x, length ts) : concatMap flattenWithRank ts


generalize :: (Ord n, Ord t) => ([n] -> n) -> RTG n t -> [Tree t] -> RTG n t
generalize merger = foldl (generalize' 1 merger)


generalize' :: (Ord n, Ord t) => Int -> ([n] -> n) -> RTG n t -> Tree t -> RTG n t
generalize' i merger g t
  | any ((`S.member` initialS g) . rootLabel) ds = g
  | null ds   = generalize' (i + 1) merger (descent merger g t (initials g)) t
  | otherwise = g{initialS = S.insert (rootLabel $ head ds) (initialS g)}
  where
    ds = deriveTreeComplete' g t $ initials g


deriveTree'
  :: (Ord n, Ord t)
  => RTG n t -> Tree t -> [n] -> [Tree (n, Either (Tree t) t)]
deriveTree' g t = concatMap (deriveTree g t)


deriveTree
  :: (Ord n, Ord t)
  => RTG n t -> Tree t -> n -> [Tree (n, Either (Tree t) t)]
deriveTree g t@(Node x ts) nt
  = if null parses then return (Node (nt, Left t) []) else parses
  where
    parses
      = S.toList (M.findWithDefault S.empty (nt, x, length ts) (ruleM g))
      >>= sequence . zipWith (deriveTree g) ts
      >>= return . Node (nt, Right x)


deriveTreeComplete' :: (Ord n, Ord t) => RTG n t -> Tree t -> [n] -> [Tree n]
deriveTreeComplete' g t = concatMap (deriveTreeComplete g t)


deriveTreeComplete :: (Ord n, Ord t) => RTG n t -> Tree t -> n -> [Tree n]
deriveTreeComplete g (Node x ts) nt
  = S.toList (M.findWithDefault S.empty (nt, x, length ts) (ruleM g))
  >>= sequence . zipWith (deriveTreeComplete g) ts
  >>= return . Node nt


completeDerivation :: Tree (n, Either a t) -> Bool
completeDerivation = all (isRight . snd) . flatten


incompleteDerivation :: Tree (n, Either a t) -> Bool
incompleteDerivation = any (isLeft . snd) . flatten


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False


isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True


maxSententialForm :: [Tree (a, Either b c)] -> Tree (a, Either b c)
maxSententialForm
  = maximumBy $ \ t1 t2 ->
      case (compare `on` length . rights . map snd . flatten) t1 t2 of
        EQ -> case (compare `on` length . lefts . map snd . flatten) t1 t2 of
          LT -> GT
          GT -> LT
          EQ -> EQ  -- TODO: refine further
        o -> o


descent
  :: (Ord n, Ord t)
  => ([n] -> n) -> RTG n t -> Tree t -> [n] -> RTG n t
descent merger g t nts
  = if null underivableTrees
    then merge merger g merges
    else descent merger g (head underivableTrees) nts'
  where
    nts' = S.toList (nonterminalS g)
    holeDerivs
      = [ ( nt
          , t'
          , deriveTreeComplete' g t' nts'
          )
        | let d = maxSententialForm $ deriveTree' g t nts
        , isRight (snd (rootLabel d))
          || error "PBSM.PatternBasedStateMerging.descent: \
                   \Unknown terminal/rank combination."
        , (nt, Left t') <- flatten d
        ]
    underivableTrees = [t' | (_, t', ds) <- holeDerivs, null ds]
    merges
      = [ nt : map rootLabel ds
        | (nt, _, ds) <- holeDerivs
        ]


merge :: (Ord n, Ord t) => ([n] -> n) -> RTG n t -> [[n]] -> RTG n t
merge merger g ntss
  = mapNonterminals mapState g
  where
    mapState q = M.findWithDefault q q mapping
    mapping
      = M.fromList
          [ (x, merger xs)
          | xs <- map S.toList $ unionOverlaps $ map S.fromList ntss
          , x <- xs
          ]


unionOverlaps :: Ord a => [S.Set a] -> [S.Set a]
unionOverlaps [] = []
unionOverlaps (x : xs)
  = case partition (S.null . S.intersection x) xs of
      (ys, []) -> x : unionOverlaps ys
      (ys, zs) -> unionOverlaps (S.unions (x : zs) : ys)


-- combinations :: [[a]] -> [[a]]
-- combinations = foldr (\ xs yss -> [x : ys | ys <- yss, x <- xs]) [[]]
-- oder:
-- combinations = sequence
