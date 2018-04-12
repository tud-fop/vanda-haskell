-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Corpus.Penn.Filter
-- Description :  remove some annotations from penn treebank trees
-- Copyright   :  (c) Technische Universität Dresden 2015
-- License     :  BSD-style
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Functions to remove some annotations from penn treebank trees which might
-- not be useful in for training.
--
-- You may find some insight about the annotation syntax in
-- “<http://dx.doi.org/10.3115/1075812.1075835 The Penn Treebank: annotating predicate argument structure>”.
-----------------------------------------------------------------------------

module Vanda.Corpus.Penn.Filter
( stripAll
, stripTraces
) where


import           Control.Monad ((>=>))
import           Data.Char
import           Data.List
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Tree



-- | Remove all knwon annotations including traces (cf. 'stripTraces').
stripAll :: Tree String -> Maybe (Tree String)
stripAll
  = mapMaybeTree
      (stripOtherInner >=> stripTracesInner >=> stripFunctionalTags)
      (stripOtherLeaf >=> stripTraceLeaf)


-- | Remove annotations of null elements, discontinuous contituents,
-- conjunction and gapping.
stripTraces :: Tree String -> Maybe (Tree String)
stripTraces = mapMaybeTree stripTracesInner stripTraceLeaf


mapMaybeTree
  :: (a -> Maybe b)  -- ^ inner node mapper
  -> (a -> Maybe b)  -- ^ leaf mapper
  -> Tree a
  -> Maybe (Tree b)
mapMaybeTree f g = go
  where
    go (Node cs []) = node (g cs) []
    go (Node cs ts) = if null ts' then Nothing else node (f cs) ts'
      where ts' = mapMaybe go ts
    node Nothing  _  = Nothing
    node (Just x) ts = Just (Node x ts)


-- | Repeatedly remove trailing digits prefixed by @-@ or @=@.
stripTracesInner :: String -> Maybe String
stripTracesInner = Just . go . reverse
  where
    go cs = case span isDigit cs of
              (_ : _, '-' : xs) -> go xs
              (_ : _, '=' : xs) -> go xs
              _                 -> reverse cs


stripTraceLeaf :: String -> Maybe String
stripTraceLeaf cs
  = case break ('-' ==) cs of
      (l, '-' : r) | S.member l traceLeafPrefixes && all isDigit r -> Nothing
      _ -> Just cs

traceLeafPrefixes :: Set String
traceLeafPrefixes
  = S.fromList ["*", "*T*", "*ICH*", "*PPA*", "*RNR*", "*EXP*"]


stripFunctionalTags :: String -> Maybe String
stripFunctionalTags
  = Just
  . intercalate "-"
  . (\ (x : xs) -> x : filter (`S.notMember` functionalTags) xs)
  . unintercalate ('-' ==)


functionalTags :: Set String
functionalTags
  = S.fromList [ "HLN", "LST", "TTL", "CLF", "NOM", "ADV", "LGS", "PRD", "SBJ"
               , "TPC", "CLR", "VOC", "DIR", "LOC", "MNR", "PRP", "TMP"]


stripOtherInner :: String -> Maybe String
stripOtherInner cs = justIf (cs /= "-NONE-") cs


stripOtherLeaf :: String -> Maybe String
stripOtherLeaf cs = justIf (S.notMember cs otherLeafs) cs

otherLeafs :: Set String
otherLeafs = S.fromList ["*", "*?*", "*NOT*", "*U*"]


justIf :: Bool -> a -> Maybe a
justIf b x = if b then Just x else Nothing


unintercalate :: (a -> Bool) -> [a] -> [[a]]
unintercalate p xs
  = case break p xs of
      (ys, []    ) -> [ys]
      (ys, _ : zs) -> ys : unintercalate p zs
