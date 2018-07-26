module Data.BitMap where

import Data.Vector.Unboxed as U

newtype BitMap = BitMap (Vector Bool) deriving (Eq, Show)

(!) :: BitMap -> Int -> Bool
(BitMap a) ! n = a U.! n

false :: Int -> BitMap
false n = BitMap $ U.replicate n False

true :: Int -> BitMap
true n = BitMap $ U.replicate n True

length :: BitMap -> Int
length (BitMap a) = U.length a

flip :: BitMap -> Int -> BitMap
(BitMap a) `flip` n = BitMap $ U.accum (\ old _ -> not old) a [(n, ())]

isTrue :: BitMap -> Bool
isTrue (BitMap a) = U.and a

isFalse :: BitMap -> Bool
isFalse (BitMap a) = not $ U.or a
