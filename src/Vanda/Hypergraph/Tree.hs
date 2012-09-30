{-# LANGUAGE RecordWildCards #-}
module Vanda.Hypergraph.Tree where

data Tree l
  = Nullary { rootLabel :: l }
  | Unary { rootLabel :: l, sub1 :: Tree l }
  | Binary { rootLabel :: l, sub1 :: Tree l, sub2 :: Tree l }
  | Node { rootLabel :: l, _subForest :: [Tree l] }
  deriving (Eq, Ord, Show)

node :: l -> [Tree l] -> Tree l
node l cs = case cs of
              [] -> Nullary{ rootLabel = l }
              [c1] -> Unary{ rootLabel = l, sub1 = c1 }
              [c1, c2] -> Binary{ rootLabel = l, sub1 = c1, sub2 = c2 }
              _ -> Node{ rootLabel = l, _subForest = cs }

subForest :: Tree l -> [Tree l]
subForest t = case t of
                Nullary{} -> []
                Unary{ .. } -> [sub1]
                Binary{ .. } -> [sub1, sub2]
                Node{ .. } -> _subForest

instance Functor Tree where
  fmap f t = case t of
               Nullary l -> Nullary (f l)
               Unary l s1 -> Unary (f l) (fmap f s1)
               Binary l s1 s2 -> Binary (f l) (fmap f s1) (fmap f s2)
               Node l sF -> Node (f l) (map (fmap f) sF)

mapChildren :: (Tree l -> Tree l) -> Tree l -> Tree l
mapChildren f t = case t of
                    Nullary{} -> t
                    Unary{ .. } -> t{ sub1 = f sub1 }
                    Binary{ .. } -> t{ sub1 = f sub1, sub2 = f sub2 }
                    Node{ .. } -> t{ _subForest = map f _subForest }

front :: Tree l -> [l]
front Nullary{ .. } = [rootLabel]
front t = concatMap front $ subForest t
