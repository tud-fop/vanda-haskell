module XFSA.Language (
  assocLanguage, language, contains,
  wordElem, sortedDerivs
) where

import XFSA.Internal

import Data.Foldable
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.Monoid

import Control.Applicative
import qualified Control.Monad.WeightedSearch as W

-- | All words in the language of the associated automaton, sorted by length
assocLanguage :: XFSA l -> [[l]]
assocLanguage = language (\l -> [[l]])

-- | All words in the language of the XFSA
language :: Monoid t => (l -> [t]) -> XFSA l -> [t]
language f = sortedDerivs (map (\w -> (1::Int, w)) . f)

-- | Tests whether a word is element of the language of the XFSA.
contains :: (l -> [c] -> [[c]]) -> [c] -> XFSA l -> Bool
contains f w (XFSA ini fin trans) = V.any (go w) ini where
  go [] q = V.elem q fin
  go cs q = V.or $ do
    (q', l) <- trans ! q
    cs' <- V.fromList $ f l cs
    return $ go cs' q'

-- | A restricted version of @contains@
wordElem :: (l -> c -> Bool) -> [c] -> XFSA l -> Bool
wordElem f = contains $ \l cs -> case cs of
  [] -> []
  (c:cs') -> [cs' | f l c]

-- | All words of the language of the XFSA, sorted by the weights of their generating labels
sortedDerivs :: (Monoid t, W.Weight w) => (l -> [(w, t)]) -> XFSA l -> [t]
sortedDerivs f (XFSA ini fin trans) = toList . asum $ V.map go ini where
  go i | flookup ! i = pure mempty <|> goTrans i
       | otherwise = goTrans i

  goTrans i = do
    (j, l) <- asum . map pure $ trans' ! i
    w <- asum . map (\(w, t) -> W.weight w $ return t) $ f l
    rest <- go j
    return $ w <> rest

  trans' = V.map V.toList trans

  flookup = V.update (V.replicate (V.length trans) False)
            (V.map (\x -> (x,True)) fin)
