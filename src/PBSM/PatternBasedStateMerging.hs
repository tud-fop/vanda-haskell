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
  = RTG (S.fromList (map S.singleton corpus))
  $ S.fromList
  $ concatMap go corpus
  where
    go :: Tree t -> [Rule (S.Set (Tree t)) t]
    go t@(Node x ts)
      = Rule (S.singleton t) x (map S.singleton ts)
      : concatMap go ts


unknownTerminals :: Ord t => RTG n t -> Tree t -> S.Set t
unknownTerminals g tree
  = terminalsTrees S.\\ terminalsG
  where
    terminalsG     = S.fromList $ map lab $ rules g
    terminalsTrees = S.fromList $ flatten tree


generalize :: (Ord a, Ord t) => RTG (S.Set a) t -> [Tree t] -> RTG (S.Set a) t
generalize g = foldl generalize' g


generalize' :: (Ord a, Ord t) => RTG (S.Set a) t -> Tree t -> RTG (S.Set a) t
generalize' g t
  = if any ((`S.member` initialS g) . fst . rootLabel) ds
    then g
    else if null ds
      then generalize' (descent g t (initials g)) t
      else g{initialS = S.insert (fst $ rootLabel $ head ds) (initialS g)}
  where
    ds = filter completeDerivation $ concatMap (deriveTree g t) (initials g)


deriveTree
  :: (Eq n, Eq t)
  => RTG n t -> Tree t -> n -> [Tree (n, Either (Tree t) t)]
deriveTree g t@(Node x ts) nt
  = if null parses then return (Node (nt, Left t) []) else parses
  where
    parses
      = filterRules nt
      >>= sequence . zipWith (deriveTree g) ts . succs
      >>= return . Node (nt, Right x)
    filterRules q
      = filter ((q ==) . lhs)
      . filter ((length ts ==) . length . succs)
      . filter ((x ==) . lab)
      $ rules g


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
  :: (Ord a, Ord t)
  => RTG (S.Set a) t -> Tree t -> [S.Set a] -> RTG (S.Set a) t
descent g t nts
  = if null underivableTrees
    then merge g merges
    else descent g (head underivableTrees) nts'
  where
    nts' = S.toList (nonterminals g)
    holeDerivs
      = [ ( nt
          , t'
          , filter completeDerivation
          $ concatMap (deriveTree g t') nts'
          )
        | (nt, Left t')
            <- flatten $ maxSententialForm $ concatMap (deriveTree g t) nts
        ]
    underivableTrees = [t' | (_, t', ds) <- holeDerivs, null ds]
    merges
      = [ nt : map (fst . rootLabel) ds
        | (nt, _, ds) <- holeDerivs
        ]


merge :: (Ord a, Ord t) => RTG (S.Set a) t -> [[S.Set a]] -> RTG (S.Set a) t
merge g ntss
  = RTG (S.map mapState $ initialS g)
        (S.map mapRule  $ ruleS    g)
  where
    mapRule (Rule n t ns) = Rule (mapState n) t (map mapState ns)
    mapState q = M.findWithDefault q q mapping
    mapping
      = M.fromList
          [ (x, S.unions xs)
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
