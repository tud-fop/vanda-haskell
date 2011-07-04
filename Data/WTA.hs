-- (c) 2010-2011 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

module Data.WTA
( WTA(..)
, wtaCreate
, states
, mapStates
, drawWTA
, weightTree
, generate
, generate'
) where

import Data.Hypergraph

import Control.DeepSeq
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Tree as T


data WTA q t w i = WTA
    { finalWeights :: M.Map q w
    , toHypergraph :: Hypergraph q t w i
    } deriving Show


wtaCreate :: (Ord q, Num w) => [(q, w)] -> [Hyperedge q t w i] -> WTA q t w i
wtaCreate fs ts = WTA (M.fromListWith (+) fs) (hypergraph ts)


states :: (Ord q) => WTA q t w i -> [q]
states a
  = S.toList
  $ L.foldl' (flip S.insert) (verticesS $ toHypergraph a)
  $ M.keys
  $ finalWeights a


mapStates :: (Ord q, Num w) => (p -> q) -> WTA p t w i -> WTA q t w i
mapStates f (WTA fs g) = WTA (M.mapKeysWith (+) f fs) (mapVertices f g)


drawWTA :: (Show q, Show t, Show w, Show i) => WTA q t w i -> String
drawWTA (WTA fs g)
  =   "Transitions:\n"
  ++  drawHypergraph g
  ++  "\nFinal Weights:\n"
  ++  (unlines . map show $ M.toList fs)


weightTree :: (Eq q, Eq t, Num w, Ord q) => WTA q t w i -> T.Tree t -> w
weightTree (WTA fs g) tree
  = sum
  . map (\(q, w) -> weightTree' g q tree * w)
  $ M.toList fs


weightTree'
  :: (Eq q, Eq t, Num w, Ord q) => Hypergraph q t w i -> q -> T.Tree t -> w
weightTree' g q tree
  = sum
      [ product (zipWith (weightTree' g) qs trees) * eWeight t
      | let root = T.rootLabel tree
      , let trees = T.subForest tree
      , let lTrees = length trees
      , t <- M.findWithDefault [] q (edgesM g)
      , eLabel t == root
      , let qs = eTail t
      , lTrees == length qs
      ]


generate :: (Ord q) => Hypergraph q t w i -> [T.Tree t]
generate = fmap (fmap (\(_, t, _) -> t)) . generate'


generate' :: (Ord q) => Hypergraph q t w i -> [T.Tree (q, t, w)]
generate' g = map fst $ generateHeight g 0 M.empty


generateHeight
  :: (Ord q)
  => Hypergraph q t w i
  -> Int
  -> M.Map q [(T.Tree (q, t, w), Int)]
  -> [(T.Tree (q, t, w), Int)]
generateHeight g h m
  = let trees
          = [ ( T.Node (eHead t, eLabel t, eWeight t) trees'
              , h + 1 )
            | t <- edges g
            , (trees', h') <- generateSubs (eTail t) m
            , h' == h
            ]
    in if null trees
    then []
    else trees
      ++ generateHeight
          g
          (h + 1)
          (foldr
            (\x@(t, _) ->
              M.insertWith
                (++)
                (let (q, _, _) = T.rootLabel t in q)
                [x]
            )
            m
            trees
          )


generateSubs :: (Num h, Ord h, Ord q) => [q] -> M.Map q [(t, h)] -> [([t], h)]
generateSubs (q:qs) m
  = let tss = generateSubs qs m
    in maybe
        []
        (\ts' -> [(t':ts, max h' h) | (ts, h) <- tss, (t', h') <- ts'])
        (M.lookup q m)
generateSubs [] _ = [([], 0)]


{-
generate wta q
  = [ T.Node (transTerminal t, q, transWeight t) subs
    | t <- transitions wta
    , transState t == q
    , subs <- combinations (map (generate wta) (transStates t))
    ]


generateH wta 1 q
  = [ T.Node (transTerminal t, q, transWeight t) []
    | t <- transitions wta
    , transState t == q
    , transStates t == []
    ]
generateH wta n q
  = [ T.Node (transTerminal t, q, transWeight t) subs
    | t <- transitions wta
    , transState t == q
    , subs <- combinations (map (generateH wta (n-1)) (transStates t))
    ]


combinations (xs:xss) = [ x:ys | ys <- combinations xss, x <- xs ]
combinations [] = [[]]


split [] = []
split (x:xs) = it [] x xs
  where
    it fxs y zs = (fxs, y, zs):
      case zs of
        (z:zs') -> it (fxs ++ [y]) z zs'
        _ -> []
-}

-- ---------------------------------------------------------------------------

instance (NFData q, NFData t, NFData w, NFData i) => NFData (WTA q t w i) where
  rnf (WTA fs g) = rnf fs `seq` rnf g
