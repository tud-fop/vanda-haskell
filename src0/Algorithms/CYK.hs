-- (c) 2011 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

module Algorithms.CYK (cyk) where


import Data.WCFG

import qualified Data.Set as S


-- | Intersect the language represented by a epsilon-free 'WCFG' in Chomsky
-- Normal Form with the language containing a single word resulting in a new
-- epsilon-free 'WCFG' in Chomsky Normal Form.
-- /It is not checked if the input grammar has the required form./
cyk :: (Ord v, Eq t) => [t] -> WCFG v t w i -> WCFG (Int, v, Int) t w i
cyk ts g
  = let n = length ts
        ps = productions g
        ps' = [ production (i, pLeft p, i + 1) [Right t'] (pWeight p) (pId p)
              | p <- ps
              , [Right t'] <- [pRight p]
              , (i, t) <- zip [0 ..] ts
              , t' == t
              ]
    in wcfg (0, initial g, n)
  $ (++) ps'
  $ iter ps [(k, i, j) | k <- [2 .. n], i <- [0 .. n - k], j <- [1 .. k - 1]]
  $ S.fromList
  $ map pLeft ps'


iter
  :: (Ord v)
  => [Production v t w i]
  -> [(Int, Int, Int)]
  -> S.Set (Int, v, Int)
  -> [Production (Int, v, Int) t w i]
iter _ [] _ = []
iter ps ((k, i, j):xs) vs'
  = let ps' = [ production l' r' (pWeight p) (pId p)
              | p <- ps
              , [Left r1, Left r2] <- [pRight p]
              , let r1' = (i    , r1, i + j)
              , let r2' = (i + j, r2, i + k)
              , S.member r1' vs'
              , S.member r2' vs'
              , let l' = (i, pLeft p, i + k)
              , let r' = [Left r1', Left r2']
              ]
    in (++) ps' $ iter ps xs $ S.union vs' $ S.fromList $ map pLeft ps'
