module XFSA.Closure (
  union, concatenate, kleene, intersect,
  construct
) where

import XFSA.Internal

import Data.Vector ((!))
import qualified Control.Applicative as A

-- | L = L₁ ∪ L₂
union :: XFSA l -> XFSA l -> XFSA l
union f1 f2 = construct $ mkXFSA [0] [1] [[(1,f1),(1,f2)],[]]

-- | L = L₁ ⋅ L₂
concatenate :: XFSA l -> XFSA l -> XFSA l
concatenate f1 f2 = construct $ mkXFSA [0] [2] [[(1,f1)],[(2,f2)],[]]

-- L = L₁*
kleene :: XFSA l -> XFSA l
kleene f = construct $ mkXFSA [0] [0] [[(0,f)]]

-- | L = L₁ ∩ L₂
-- only works if all labels generate words of a fixed length
intersect :: (l1 -> l2 -> Maybe l) -> XFSA l1 -> XFSA l2 -> XFSA l
intersect f f1@(XFSA i1 fin1 trans1) f2@(XFSA i2 fin2 trans2) = XFSA i fin trans
  where
    n = stateCount f2
    mk a b = a*n + b

    i = mk <$> i1 <*> i2

    fin = mk <$> fin1 <*> fin2

    trans = do
      a <- allStates f1
      b <- allStates f2
      return $ do
        (a', l1) <- trans1 ! a
        (b', l2) <- trans2 ! b
        case f l1 l2 of
         Nothing -> A.empty
         Just l -> return (mk a' b', l)
