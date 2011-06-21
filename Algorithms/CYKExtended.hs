-- (c) 2011 Toni Dietze
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

module Algorithms.CYKExtended (cyk) where


import Data.WCFG
import Tools.Miscellaneous (mapFst, mapSnd)

import Data.List (tails)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S


data Item v t w i = Item
  (Production v t w i)     -- the source production
  Int                      -- start position in input string
  Int                      -- end position in input string
  [Either (Int, v,Int) t]  -- new rhs
  [Either v t]             -- rest of old rhs


-- | Bottom up parsing for epsilon-free 'Data.WCFG.WCFG's.
-- /It is not checked if the input grammar is epsilon-free./
cyk :: (Eq t, Ord v) => [t] -> WCFG v t w i -> WCFG (Int, v, Int) t w i
cyk w g
  = let n = length w
        (items, prods)
          = partitionItems
          $ catMaybes
              [ scan' w' $ Item p i i [] (pRight p)
              | p <- productions g
              , (i, w') <- zip [0 ..] $ tails w
              ]
        ls = S.fromList $ map pLeft prods
     in wcfg (0, initial g, n)
     $ prods ++ iter [0 .. n] w ls ls items


iter
  :: (Eq t, Ord v)
  => [Int]                -- ^ unprocessed spans
  -> [t]                  -- ^ input string
  -> S.Set (Int, v, Int)  -- ^ unprocessed new non-terminals for current span
  -> S.Set (Int, v, Int)  -- ^ currently all new non-terminals
  -> [Item v t w i]       -- ^ currently all items
  -> [Production (Int, v, Int) t w i]
iter [] _ _ _ _ = []
iter ks@(k : ks') w ls' ls items
  | S.null ls'
  = iter ks' w ls ls items
  | otherwise
  = let (items', prods)
          = partitionItems
          $ mapMaybe (scan w)
              [ Item p i j' (Left v' : r) rest
              | Item p i j r (Left v : rest) <- items
              , let j' = i + k
              , let v' = (j, v, j')
              , S.member v' ls'
              ]
        ls'' = S.fromList (map pLeft prods) S.\\ ls
    in prods
    ++ iter
          ks
          w
          ls''
          (S.union ls ls'')
          (items' ++ items)


partitionItems
  ::  [Item v t w i]
  -> ([Item v t w i], [Production (Int, v, Int) t w i])
partitionItems
  = foldr f ([], [])
  where
    f (Item p i j r [])
      = mapSnd (production (i, pLeft p, j) (reverse r) (pWeight p) (pId p) :)
    f item
      = mapFst (item :)


scan :: (Eq t) => [t] -> Item v t w i -> Maybe (Item v t w i)
scan w item@(Item _ _ j _ _) = scan' (drop j w) item


scan' :: (Eq t) => [t] -> Item v t w i -> Maybe (Item v t w i)
scan' (t : w) (Item p i j r (Right t' : rest))
  | t == t'   = scan' w (Item p i (j + 1) (Right t' : r) rest)
  | otherwise = Nothing
scan' _ item@(Item _ _ _ _ (Left _ : _)) = Just item
scan' _ item@(Item _ _ _ _ []          ) = Just item
scan' _ _                                = Nothing
