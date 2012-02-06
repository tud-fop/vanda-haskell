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


import qualified Data.Queue as Q
import Data.WCFG

import Data.List (tails)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)


data Item v t w i = Item
  (Production v t w i)      -- the source production
  Int                       -- start position in input string
  Int                       -- end position in input string
  [Either (Int, v, Int) t]  -- new rhs (reversed)
  [Either v t]              -- rest of old rhs
  [t]                       -- unscanned part of input word
  deriving (Show)


-- | Bottom up parsing for 'Data.WCFG.WCFG's.
cyk :: (Eq t, Ord v) => [t] -> WCFG v t w i -> WCFG (Int, v, Int) t w i
cyk w g
  = wcfg (0, initial g, length w)
  $ iter M.empty
  $ Q.fromList
  $ mapMaybe scan
    [ Item p i i [] (pRight p) w'
    | p <- productions g
    , (i, w') <- zip [0 ..] $ tails w
    ]


iter
  :: (Eq t, Ord v)
  => M.Map (Int, v) (M.Map Int [t], [Item v t w i])
  -> Q.Queue (Item v t w i)
  -> [Production (Int, v, Int) t w i]
iter m q
  | Q.null q  = []
  | otherwise = m `seq` let (it, q') = Q.deq q in
    case it of
      Item p i j r (Left v : rest) _
       -> let (wM, its) = M.findWithDefault (M.empty, []) (j, v) m
          in iter (M.insert (j, v) (wM, it : its) m)
            $ flip Q.enqList q'
            $ flip mapMaybe (M.toList wM)
            $ \ (k, w) -> scan $ Item p i k (Left (j, v, k) : r) rest w
      Item p j k r [] w
       -> let v = pLeft p
              v' = (j, v, k)
              (wM, its) = M.findWithDefault (M.empty, []) (j, v) m
       in production v' (reverse r) (pWeight p) (pId p)
        : if M.member k wM
          then iter m q'
          else iter (M.insert (j, v) (M.insert k w wM, its) m)
            $ flip Q.enqList q'
            $ flip mapMaybe its
            $ \ (Item p' i _{-j-} r' (_{-Left v-} : rest) _)
              -> scan $ Item p' i k (Left v' : r') rest w
      _ -> error "Algorithms.CYKExtended.iter: Invariant violated"


scan :: (Eq t) => Item v t w i -> Maybe (Item v t w i)
scan (Item p i j r (Right t : rest) (t' : w))
  | t == t'   = scan (Item p i (j + 1) (Right t : r) rest w)
  | otherwise = Nothing
scan item@(Item _ _ _ _ (Left _ : _) _) = Just item
scan item@(Item _ _ _ _ []           _) = Just item
scan _                                  = Nothing


{-
g1 :: WCFG Char Char Int Int
g1 = wcfg 'S'
  [ production 'S' [Left 'A', Left 'B', Left 'D'] 0 0
  , production 'A' [Right 'a', Left 'A'] 1 1
  , production 'A' [] 2 2
  , production 'B' [Left 'C', Left 'B', Left 'C', Right 'b', Left 'C'] 3 3
  , production 'B' [Right 'b'] 4 4
  , production 'C' [] 5 5
  , production 'D' [Left 'C'] 6 6
  ]


g2 :: WCFG Char Char Int Int
g2 = wcfg 'S'
  [ production 'S' [Left 'S', Left 'S'] 0 0
  , production 'S' [Right 's'] 1 1
  ]
-}
