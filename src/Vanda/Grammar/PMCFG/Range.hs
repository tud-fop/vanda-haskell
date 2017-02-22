module Vanda.Grammar.PMCFG.Range 
  ( Range(Epsilon)
  , Rangevector
  -- * ranges
  , singletons
  , entire
  , safeConc
  -- * range vectos
  , singleton
  , fromList
  , (!)
  , vectorLength
  ) where

import qualified Data.Vector.Unboxed as V
import Data.Hashable (Hashable(hashWithSalt))
import Data.List (elemIndices)
import Data.Maybe (fromMaybe)


-- | A range (i, j) in a word w.
-- Consider i \< j for i \>= 0 and j \<= |w|
-- and 'Epsilon' substitutes all (i, i) for 0 <= i <= |w|.
data Range = Range Int Int
           | Epsilon -- ^ empty range of ε in w
            deriving (Show, Eq, Ord)

instance Hashable Range where
  salt `hashWithSalt` Epsilon = salt `hashWithSalt` (0 :: Int) `hashWithSalt` (0 :: Int)
  salt `hashWithSalt` (Range x y) = salt `hashWithSalt` x `hashWithSalt` y

-- | A range vector is a non-overlapping sequence of ranges.
newtype Rangevector = Rangevector (V.Vector (Int, Int)) deriving(Eq, Ord, Show)

instance Hashable Rangevector where
  salt `hashWithSalt` (Rangevector v) = V.foldl' (hashWithSalt) salt v 


-- | Index access for Range vectors. 
at, (!) :: Rangevector -> Int -> Range
at (Rangevector rv) i = let (x, y) = rv V.! i
                        in if x == y
                           then Epsilon
                           else Range x y
(!) = at


-- | Singleton constructor for range vectors.
singleton :: Range -> Rangevector
singleton r = fromMaybe undefined (fromList [r])


-- | Save constructor for Rangevectors.
-- Cheks of Ranges in vector are non-overlapping and stores them in an unboxed container.
fromList :: [Range] -> Maybe Rangevector
fromList rs
  | isNonOverlapping rs = Just $ Rangevector $ V.fromList $ map unbox rs
  | otherwise           = Nothing
  where
    unbox (Range x y) = (x, y)
    unbox Epsilon     = (0, 0)


-- | Wrapper for Data.Vector.length.
vectorLength :: Rangevector -> Int
vectorLength (Rangevector v) = V.length v


-- | A singleton range is a range of a single character in a word.
singletons :: (Eq t) => t -> [t] -> [Range]
singletons c w = map singleton' $ c `elemIndices` w
  where singleton' i = Range i $ i+1


-- | Full range of a word.
entire :: [t] -> Range
entire [] = Epsilon
entire xs = Range 0 $ length xs


-- | Concatenates two ranges. Fails if neighboring ranges do not fit.
safeConc :: Range -> Range -> Maybe Range
safeConc Epsilon r = Just r
safeConc r Epsilon = Just r
safeConc (Range i j) (Range k l)
  | j == k = Just $ Range i l
  | otherwise = Nothing


-- | Checks for overlapping components in a range vector.
isNonOverlapping :: [Range] -> Bool
isNonOverlapping = isNonOverlapping' []
  where
    isNonOverlapping' :: [(Int, Int)] -> [Range] -> Bool
    isNonOverlapping' _ []                          = True
    isNonOverlapping' cache (Range i j : rs)
      | any (\ (k, l) -> (j > k && j < l) 
                         || (i > k && i < l) 
                         || i <= k && j >= l) cache = False
      | otherwise                                   = isNonOverlapping' ((i, j):cache) rs
    isNonOverlapping' cache (Epsilon : rs)          = isNonOverlapping' cache rs

