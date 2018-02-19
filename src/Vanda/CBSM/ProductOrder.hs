{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CBSM.ProductOrder
-- Copyright   :  (c) Technische Universität Dresden 2014
-- License     :  BSD-style
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.CBSM.ProductOrder where


import Data.List (foldl')


data Product a b = a :&: b deriving (Eq, Show)


class PartialOrd a where
  comparePO :: a -> a -> Maybe Ordering

  (<=?)  :: a -> a -> Maybe Bool
  (<=?) = lift $ \ case { GT -> False; _ -> True }

  (>=?)  :: a -> a -> Maybe Bool
  (>=?) = lift $ \ case { LT -> False; _ -> True }

  (<?)   :: a -> a -> Maybe Bool
  (<?)  = lift $ \ case { LT -> True; _ -> False }

  (>?)   :: a -> a -> Maybe Bool
  (>?)  = lift $ \ case { GT -> True; _ -> False }

  (==?)  :: a -> a -> Maybe Bool
  (==?) = lift $ \ case { EQ -> True; _ -> False }

  (/=?) :: a -> a -> Maybe Bool
  (/=?) = lift $ \ case { EQ -> False; _ -> True }

  lift :: (Ordering -> b) -> a -> a -> Maybe b
  lift f x y = fmap f (comparePO x y)

instance PartialOrd Int where
  comparePO = (Just .) . compare

instance (PartialOrd a, PartialOrd b) => PartialOrd (Product a b) where
  comparePO (x1 :&: x2) (y1 :&: y2) = do
    x <- comparePO x1 y1
    y <- comparePO x2 y2
    case (x, y) of
      (EQ, EQ) -> Just EQ

      (LT, GT) -> Nothing
      (LT, _ ) -> Just LT
      (_ , GT) -> Just GT

      (GT, LT) -> Nothing
      (_ , LT) -> Just LT
      (GT, _ ) -> Just GT


class PartialOrd a => Enums a where
  succs :: a -> [a]
  preds :: a -> [a]

instance Enums Int where
  succs x = [succ x]
  preds x = [pred x]

instance (Enums a, Enums b) => Enums (Product a b) where
  succs (x :&: y) = fmap (x :&:) (succs y) ++ fmap (:&: y) (succs x)
  preds (x :&: y) = fmap (x :&:) (preds y) ++ fmap (:&: y) (preds x)


newtype LubSet a = LubSet [a] deriving Show

instance Eq a => Eq (LubSet a) where
  LubSet xs == LubSet ys = eqListsets xs ys

addLub :: PartialOrd a => a -> LubSet a -> LubSet a
addLub l (LubSet ls)
  | fmap or (mapM (l <=?) ls) == Just True = LubSet ls
  | otherwise = LubSet $ l : filter ((Just True /=) . (l >?)) ls


data LEqSet a = LEqSet
  { singletons :: [a]
  , lubs       :: LubSet a
  } deriving Show


instance Eq a => Eq (LEqSet a) where
  LEqSet ss1 ls1 == LEqSet ss2 ls2 = eqListsets ss1 ss2 && ls1 == ls2


eqListsets :: Eq a => [a] -> [a] -> Bool
eqListsets xs ys
  =  all (`elem` ys) xs
  && all (`elem` xs) ys


empty :: LEqSet a
empty = LEqSet [] (LubSet [])


insertSingleton :: PartialOrd a => a -> LEqSet a -> LEqSet a
insertSingleton = undefined


insertLub :: PartialOrd a => a -> LEqSet a -> LEqSet a
insertLub l (LEqSet ss ls)
  = LEqSet ss (addLub l ls)
  -- TODO

insertLubs :: PartialOrd a => [a] -> LEqSet a -> LEqSet a
insertLubs = flip $ foldl' (flip insertLub)
