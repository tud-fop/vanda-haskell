-- Copyright (c) 2010, Toni Dietze

-- | Implementation of state-split grammars based on
--
-- * Slav Petrov, Leon Barrett, Romain Thibaux, Dan Klein.
--   /Learning Accurate, Compact, and Interpretable Tree Annotation./
--   Proc. COLING/ACL 2006 (Main Conference)
--   <http://www.petrovi.de/data/acl06.pdf>
--
-- * Slav Petrov, Dan Klein.
--   /Learning and Inference for Hierarchically Split PCFGs./
--   AAAI 2007 (Nectar Track)
--   <http://www.petrovi.de/data/aaai07.pdf>

module StateSplit where

-- import EM -- TODO
import Tools.Miscellaneous(mapFst, mapSnd, sumWith)

import Data.Hypergraph

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


-- | Make a single vertex splitable.
initializeVertex :: (Num n) => v -> (v, n)
initializeVertex v = (v, 0)


-- | Makes the vertices of a 'Hypergraph' splitable without changing the
-- semantics of the 'Hypergraph'.
initialize
  :: (Num n, Ord n, Ord v)
  => Hypergraph  v     l w i
  -> Hypergraph (v, n) l w i
initialize g = mapVerticesMonotonic initializeVertex g


-- | Do a state-splitting step.
split
  :: (Fractional w, Num n, Ord v, Ord n)
  => n
  -> ((v, n) -> Bool)
  -> Hypergraph (v, n) l w i
  -> Hypergraph (v, n) l w i
split offset dontSplit g
  = hypergraph
      [ hyperedge hd tl (eLabel e) w (eId e)
      | e <- edges g
      , let tls = combinations . map split $ eTail e
      , let w   = eWeight e / fromIntegral (length tls)
      , hd <- split (eHead e)
      , tl <- tls
      ]
  where
    split v@(x, n) | dontSplit v = [v]
                   | otherwise   = [v, (x, n + offset)]


-- | Do a state-merging step.
merge
  :: (Fractional w, Ord v, Ord k, Ord l, Ord i)
  => (v -> k)
  -> Data.Hypergraph.Hypergraph v l w i
  -> Data.Hypergraph.Hypergraph k l w i
merge mergeState
  = hypergraph
  . map
      (\ ((hd, tl, l, i), es) ->
        let w = sumWith eWeight es
                / (fromIntegral . S.size . S.fromList . map eHead $ es)
        in hyperedge hd tl l w i
      )
  . M.toList
  . partition
      (\ e ->
        ( mergeState (eHead e)
        , map mergeState (eTail e)
        , eLabel e
        , eId e
        )
      )
  . edges


maxSplit :: (Ord n) => Hypergraph (v, n) l w i -> n
maxSplit = maximum . map snd . vertices

-- ---------------------------------------------------------------------------

-- | Partition a list with respect to a function; e.g.
--
-- > partition (flip mod 3) [0 .. 9] 
-- > == fromList [(0,[0,3,6,9]), (1,[1,4,7]), (2,[2,5,8])]
partition :: (Ord k) => (a -> k) -> [a] -> M.Map k [a]
partition f = foldr step M.empty
    where step x = M.insertWith (++) (f x) [x]

-- ---------------------------------------------------------------------------

-- | Create a list of lists of all possible combinations of the given elements
-- of the given length respectively.
--
-- > variation 2 [0 .. 1] == [[0,0],[1,0],[0,1],[1,1]]
variation
  :: (Num n)
  => n      -- ^ Length of the combinations.
  -> [a]    -- ^ Given elements.
  -> [[a]]
variation 0 _  = [[]]
variation k xs = concatMap (\ys -> map (:ys) xs) (variation (k-1) xs)


-- | Build a list of all possible combinations of sublist elements by taking
-- an element of each sublist, respectively; e.g.
-- @combinations [[-1, -2], [1, 2]] == [[-1, 1], [-2, 1], [-1, 2], [-2, 2]]@.
combinations :: [[a]] -> [[a]]
combinations []
  = [[]]
combinations (x : xs)
  = [ y : ys
    | ys <- combinations xs
    , y  <- x
    ]

-- ---------------------------------------------------------------------------

-- | Checks, if a given 'Hypergraph' is equivalent to the 'Hypergraph' after
-- splitting and a merging everything back.
--
-- The function takes numerical imprecisions into account.
prop_splitMerge
  :: (Ord v, Integral n, Ord l, Ord i, Fractional w, Ord w)
  => Hypergraph (v, n) l w i -> Bool
prop_splitMerge g
  = let offset = 1 + maxSplit g
        mergeState (q, n) = (q, n `mod` offset)
        g' = merge mergeState . split offset (const False) $ g
    in eqListWith eqEdge (edges g) (edges g')
    where
      x ~~ y = abs (x - y) < 0.0000001
      eqEdge e1 e2
        =  eHead  e1 == eHead  e2
        && eTail  e1 == eTail  e2
        && eLabel e1 == eLabel e2
        && eId    e1 == eId    e2
        && eWeight e1 ~~ eWeight e2
      eqListWith f xs ys
        =  and (map (\ x -> any (f x) ys) xs)
        && and (map (\ y -> any (f y) xs) ys)
