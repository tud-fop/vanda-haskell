{-# LANGUAGE RecordWildCards #-}
module Vanda.Hypergraph.Tree where

import qualified Data.Binary as B
import Data.Hashable ( Hashable (..) )
import Data.Traversable
import Data.Foldable ( Foldable, foldMap )
import Data.Monoid ( mappend )
import Control.Applicative
import Data.List ( intercalate )

data Tree l
  = Nullary { rootLabel :: l }
  | Unary { rootLabel :: l, sub1 :: Tree l }
  | Binary { rootLabel :: l, sub1 :: Tree l, sub2 :: Tree l }
  | Node { rootLabel :: l, _subForest :: [Tree l] }
  deriving (Eq, Ord)


instance B.Binary l => B.Binary (Tree l) where
  get = do
          x1 <- B.get
          x2 <- B.get
          return $! node x1 x2
  put t = B.put (rootLabel t) >> B.put (subForest t)

instance Hashable l => Hashable (Tree l) where
  hashWithSalt salt Nullary{ .. } = hashWithSalt salt rootLabel
  hashWithSalt salt Unary{ .. }
    = salt `hashWithSalt` rootLabel `hashWithSalt` sub1
  hashWithSalt salt Binary{ .. }
    = salt `hashWithSalt` rootLabel `hashWithSalt` sub1 `hashWithSalt` sub2
  hashWithSalt salt Node{ .. }
    = salt `hashWithSalt` rootLabel `hashWithSalt` _subForest

instance Traversable Tree where
  traverse f (Nullary l) = Nullary <$> f l
  traverse f (Unary l t) = Unary <$> f l <*> traverse f t
  traverse f (Binary l t1 t2) = Binary <$> f l <*> traverse f t1 <*> traverse f t2
  traverse f (Node l ts) = Node <$> f l <*> traverse (traverse f) ts

instance Functor Tree where
  fmap f (Nullary l) = Nullary $ f l
  fmap f (Unary l t) = Unary (f l) (fmap f t)
  fmap f (Binary l t1 t2) = Binary (f l) (fmap f t1) (fmap f t2)
  fmap f (Node l ts) = Node (f l) (map (fmap f) ts)

instance Foldable Tree where
  foldMap f (Nullary l)
    = f l
  foldMap f (Unary l t)
    = f l `mappend` foldMap f t
  foldMap f (Binary l t1 t2)
    = f l `mappend` foldMap f t1 `mappend` foldMap f t2
  foldMap f (Node l ts)
    = f l `mappend` foldMap (foldMap f) ts

arity :: Tree l -> Int
arity t = case t of
            Nullary{} -> 0
            Unary{} -> 1
            Binary{} -> 2
            Node{ .. } -> length _subForest

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

mapChildren :: (Tree l -> Tree l) -> Tree l -> Tree l
mapChildren f t = case t of
                    Nullary{} -> t
                    Unary{ .. } -> t{ sub1 = f sub1 }
                    Binary{ .. } -> t{ sub1 = f sub1, sub2 = f sub2 }
                    Node{ .. } -> t{ _subForest = map f _subForest }

front :: Tree l -> [l]
front Nullary{ .. } = [rootLabel]
front t = concatMap front $ subForest t

instance Show i => Show (Tree i) where
  show t = case subForest t of
             [] -> show (rootLabel t)
             xs -> concat [ show $ rootLabel t
                          , "(", intercalate "," $ map show xs, ")"
                          ]

