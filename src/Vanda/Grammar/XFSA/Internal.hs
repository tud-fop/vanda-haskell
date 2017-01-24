{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module XFSA.Internal (
  XFSA(..),
  empty, epsilon, singleton,
  fromList, expand, construct,
  mkXFSA, stateCount, allStates
) where

import qualified Data.Vector as V
import Data.Vector ((!))
import Data.Monoid

import Control.Monad
import Control.Monad.Trans.State

-- | A data type for XFSA. @l@ is the type of the labels.
data XFSA l = XFSA {
  initialStates :: !(V.Vector Int),
  finalStates :: !(V.Vector Int),
  transitions :: !(V.Vector (V.Vector (Int, l)))
  }
             deriving (Functor, Foldable, Traversable, Show)

instance Applicative XFSA where
  pure = return
  (<*>) = ap

instance Monad XFSA where
  return = singleton

  m >>= f = construct (fmap f m)

mkXFSA :: [Int] -> [Int] -> [[(Int, l)]] -> XFSA l
mkXFSA i f t = XFSA (V.fromList i) (V.fromList f) (V.fromList $ map V.fromList t)

-- | L = ∅
empty :: XFSA l
empty = mkXFSA [] [] []

-- | L = {ε}, epsilon = fromList []
epsilon :: XFSA l
epsilon = mkXFSA [0] [0] [[]]

-- | L = {w}, singleton l = fromList [l]
singleton :: l -> XFSA l
singleton w = mkXFSA [0] [1] [[(1,w)],[]]

-- | L = {w}
fromList :: [l] -> XFSA l
fromList l = mkXFSA [0] [n] trans where
  n = length l
  trans = map (:[]) (zip [1..] l) ++ [[]]

-- | Replaces each label with a bunch of other labels
expand :: (l1 -> [l2]) -> XFSA l1 -> XFSA l2
expand f m = m >>= fromList . f

construct :: XFSA (XFSA l) -> XFSA l
construct fsa@(XFSA ini fin trans) = XFSA ini' fin' trans' where
  fsaDelta = flip evalState 0 $ traverse f fsa where
    f x = do
      d <- get
      put (d + stateCount x)
      return (d, x)

  ini' = do
    i <- ini
    (_, (d, f)) <- transitions fsaDelta ! i
    (+d) <$> initialStates f

  fin' = do
    (i, (d, f)) <- allLabels fsaDelta
    guard (flookup ! i)
    (+d) <$> finalStates f

  trans' = V.update transSub transUpd

  transSub = do
    (d, f) <- allLabels' fsaDelta
    t <- transitions f
    return $ do
      (i, l) <- t
      return (i+d, l)

  transUpd = do
    (i, (d, f)) <- allLabels fsaDelta
    sfin <- finalStates f
    let sfin' = sfin + d
    (_, (dnext, fsaNext)) <- transitions fsaDelta ! i
    let newTrans = do
          si <- initialStates fsaNext
          (snext, l) <- transitions fsaNext ! si
          return (snext + dnext, l)
    return (sfin', (transSub ! sfin') <> newTrans)

  flookup = V.update (V.replicate (V.length trans) False)
            (V.map (\x -> (x,True)) fin)


stateCount :: XFSA l -> Int
stateCount (XFSA _ _ t) = V.length t

allLabels :: XFSA l -> V.Vector (Int, l)
allLabels (XFSA _ _ trans) = V.concatMap id trans

allLabels' :: XFSA l -> V.Vector l
allLabels' = V.map snd . allLabels

allStates :: XFSA l -> V.Vector Int
allStates f = V.enumFromN 0 (stateCount f)
