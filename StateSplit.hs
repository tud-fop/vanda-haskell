-- Copyright (c) 2010, Toni Dietze

-- | Implementation of state-split grammars based on
--
-- * Min Zhang, Hongfei Jiang, Aiti Aw, Haizhou Li, Chew Lim Tan, Sheng Li.
--   /A Tree Sequence Alignment-based Tree-to-Tree Translation Model./
--   Proc. of ACL-HLT 2008
--   <http://www.aclweb.org/anthology/P/P08/P08-1064.pdf>
--
-- * Slav Petrov, Leon Barrett, Romain Thibaux, Dan Klein.
--   /Learning Accurate, Compact, and Interpretable Tree Annotation./
--   Proc. COLING/ACL 2006 (Main Conference)
--   <http://www.petrovi.de/data/acl06.pdf>

module StateSplit where

-- import EM -- TODO
import Tools.Miscellaneous(mapFst, mapSnd, sumWith)

import qualified Data.WTA as WTA

import qualified Data.List as L
import qualified Data.Map as M


-- | Makes the states of a 'WTA.WTA' splitable without changing the semantics
-- of the 'WTA.WTA'.
initialize
  :: (Num n, Ord q, Ord n)
  => WTA.WTA  q     t w
  -> WTA.WTA (q, n) t w
initialize wta = WTA.mapStates (flip (,) 0) wta


-- | Do a state-splitting step.
split
  :: (Fractional w, Ord q, Ord n, Num n)
  => n  -- ^ Must be larger than 'maxSplit' of the input 'WTA.WTA'.
  -> WTA.WTA (q, n) t w
  -> WTA.WTA (q, n) t w
split offset wta
  = WTA.create
      (splitTranss (WTA.transitions wta))
      (splitFinals (WTA.finalWeights wta))
    where
      splitters = [id, mapSnd ((+) offset)]
      -- splitters = [(*) 2, (+) 1 . (*) 2]
      factor    = 1 / (fromIntegral (length splitters))
      splitTranss ts
        = [ t{WTA.transState = q', WTA.transStates = qs', WTA.transWeight = w'}
          | t   <- ts
          , let q    = WTA.transState  t
          , let qs   = WTA.transStates t
          , let l    = length qs
          , let qs's = map (flip (zipWith id) qs) (variation l splitters)
          , let q's  = map ($ q) splitters
          , let w'   = (factor ^ l) * WTA.transWeight t
          , q'  <- q's
          , qs' <- qs's
          ]
      splitFinals fs
        = [ (q', w')
          | (q, w) <- fs
          , let w'  = factor * w
          , q'     <- map ($ q) splitters
          ]


-- | Do a state-merging step.
merge
  :: (Fractional w, Eq q, Ord t, Ord k)
  => (q -> k)       -- ^ Maps a state to its representation after merging.
  -> WTA.WTA q t w
  -> WTA.WTA k t w
merge mergeState wta
  = WTA.create
      (   map (\((t, q, qs), ts') ->
              WTA.Transition t q qs
            $ sumWith WTA.transWeight ts'
              / (fromIntegral $ length $ L.nub $ map WTA.transState ts')
          )
        . M.toList
        . partition
            (\t ->
              ( WTA.transTerminal t
              , mergeState (WTA.transState t)
              , map mergeState (WTA.transStates t)
            ) )
        $ WTA.transitions  wta
      )
      (   map (mapSnd $ sumWith snd)
        . M.toList
        . partition (mergeState . fst)
        $ WTA.finalWeights wta
      )


maxSplit :: (Ord n) => WTA.WTA (q, n) t w -> n
maxSplit = maximum . map snd . WTA.states

-- -----------------------------------------------------------------------------

{-
merge offset qmerge wta
  = foldr f (WTA.transitions wta)
  
  tmerge t =  ( WTA.transTerminal t
              , qmerge $ WTA.transState t
              , map qmerge $ WTA.transStates t
              )
-}

--mapState


-- | Partition a list with respect to a function; e.g.
--
-- > partition (flip mod 3) [0 .. 9] 
-- > == fromList [(0,[0,3,6,9]), (1,[1,4,7]), (2,[2,5,8])]
partition :: (Ord k) => (a -> k) -> [a] -> M.Map k [a]
partition f = foldr step M.empty
    where step x = M.insertWith (++) (f x) [x]

{-
mergeList :: (Fractional b) => (a -> b) -> (a -> b) -> [a] -> b
mergeList toWeight _       [x] = toWeight x
mergeList _        _       []  = 0
mergeList toWeight toCount xs
  = let counts = map toCount xs
    in sum (zipWith (*) counts (map toWeight xs)) / sum counts
-}

-- -----------------------------------------------------------------------------

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

-- -----------------------------------------------------------------------------

testWTA =
  WTA.create
    [ WTA.Transition 'a' 'b' "b"   0.1
    , WTA.Transition 'a' 'b' "bb"  0.2
    , WTA.Transition 'a' 'b' "bc"  0.3
    , WTA.Transition 'a' 'b' "bd"  0.4
    , WTA.Transition 'd' 'e' "fg"  1
    , WTA.Transition 'h' 'i' "jkl" 1
    ]
    [ ('b', 0.25)
    , ('e', 0.75)
    ]


-- | Checks, if a given 'WTA.WTA' is equivalent to the 'WTA.WTA' after
-- splitting and a merging everything back.
--
-- The function takes numerical imprecisions into account.
prop_splitMerge
  :: (Fractional w, Ord q, Integral n, Ord t, Ord w)
  => WTA.WTA (q, n) t w
  -> Bool
prop_splitMerge wta
  = let offset = 1 + maxSplit wta
        mergeState (q, n) = (q, n `mod` offset)
        wta' = merge mergeState . split offset $ wta
    in eqListWith eqTrans (WTA.transitions wta) (WTA.transitions  wta')
    && eqListWith eqFin   (WTA.finalWeights wta) (WTA.finalWeights wta')
    where
      x ~~ y = abs (x - y) < 0.0000001
      eqTrans t1 t2
        =  t1{WTA.transWeight = 0} == t2{WTA.transWeight = 0}
        && WTA.transWeight t1 ~~ WTA.transWeight t2
      eqFin f1 f2
        =  fst f1 == fst f2
        && snd f1 ~~ snd f2
      eqListWith f (x:xs) (y:ys) = f x y && eqListWith f xs ys
      eqListWith _ []     []     = True
      eqListWith _ _      _      = False