-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Corpus.Negra
-- Copyright   :  (c) Technische Universität Dresden 2012
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Vanda.Corpus.Negra
  ( Negra (..)
  , WordTag (..)
  , Sentence (..)
  , SentenceData (..)
  , Edge (..)
  , negraToForest
  , negraTreeToTree
  ) where

import Control.Arrow ( first, second )
import qualified Data.IntMap as IM
import qualified Data.List as L
import Data.Ord ( comparing )
import qualified Data.Tree as T

data Negra
  = Negra
    { wordtags  :: [WordTag]
    , sentences :: [Sentence]
    } deriving Show

data WordTag
  = WordTag
    { wtId :: Int
    , wtTag :: String
    , wtBound :: Bool
    , wtDescr :: String
    } deriving Show

data Sentence
  = Sentence
    { sId :: Int
    , sEditorId :: Int
    , sDate :: String
    , sOriginId :: Int
    , sComment :: Maybe String
    , sData :: [SentenceData]
    }
    deriving Show

data SentenceData
  = SentenceWord
    { sdWord :: String
    , sdPostag :: String
    , sdMorphtag :: String
    , sdEdge :: Edge
    , sdSecEdges :: [Edge]
    , sdComment :: Maybe String
    }
  | SentenceNode
    { sdNum :: Int
    , sdPostag :: String
    , sdMorphtag :: String
    , sdEdge :: Edge
    , sdSecEdges :: [Edge]
    , sdComment :: Maybe String
    }
    deriving Show

data Edge
  = Edge
    { eLabel :: String
    , eParent :: Int
    }
    deriving Show


-- | Converts a list of 'SentenceData' to a 'T.Forest' of
-- not-crossing trees.
negraToForest
  :: [SentenceData]
  -> T.Forest ((Maybe SentenceData, Span), Span)
negraToForest
  = splitCrossedTree
  . updateInnerSpans
  . pointerTreeToCrossedTree
  . negraToPointerTree


-- | Represents a tree by using 'Pointer's.
--
-- The map maps a 'Pointer' to a node.
-- A node contains a list of labels and a list of children.
-- A child is either a 'Pointer' to a child node or a leaf.
-- A leaf contains a label and the position of the child in the yield of the
-- tree.
type PointerTree a = IM.IntMap ([a], [Either Pointer (a, Position)])
type Pointer = IM.Key
type Position = Int
type Span = (Position, Position)


-- | Insert a leaf in a 'PointerTree'.
insertLeaf
  :: (label, Position)  -- ^ leaf to insert
  -> Pointer            -- ^ 'Pointer' to parent node
  -> PointerTree label
  -> PointerTree label
insertLeaf leaf parent
  = IM.insertWith (second . (:) . head . snd) parent ([], [Right leaf])


-- | Insert an inner node in a 'PointerTree'.
insertNode
  :: label    -- ^ node to insert
  -> Pointer  -- ^ 'Pointer' to new node
  -> Pointer  -- ^ 'Pointer' to parent node
  -> PointerTree label
  -> PointerTree label
insertNode node ptr parent
  = IM.insertWith (second . (:) . head . snd) parent ([], [Left ptr])
  . IM.insertWith (first . (:) . head . fst) ptr ([node], [])


-- | Extract the 'PointerTree' from a list of 'SentenceData'.
negraToPointerTree :: [SentenceData] -> PointerTree SentenceData
negraToPointerTree = ins 0
  where
    ins _ [] = IM.empty
    -- ins i (x@SentenceWord{sdEdge = Edge{eParent = 0}}:xs) = ins i xs
    ins i (x@SentenceWord{}:xs) = insertLeaf (x, i) (eParent $ sdEdge x) (ins (i+1) xs)
    ins i (x@SentenceNode{sdNum = n}:xs) = insertNode x n (eParent $ sdEdge x) (ins i xs)



-- | Convert a 'PointerTree' to a 'T.Tree'.
--
-- 'Span' lists are used to represent crossing edges.
-- Every node of the resulting tree contains a 'Span' list.
-- Only leaf nodes will contain correct 'Span' lists!
-- A 'Span' list is a list of 'Span's.
-- A 'Span' is a pair of yield positions of the whole tree.
--
-- E.g. a node has the 'Span' list [(2, 4), (7, 9)]. Then the yield of the
-- sub tree located at the node contains the leafs 2, 3, 4, 7, 8 and 9 of the
-- whole tree.
pointerTreeToCrossedTree
  :: (Show a)
  => PointerTree a
  -> T.Tree (Maybe a, [Span])
pointerTreeToCrossedTree = f 0
  where
    f num pt
      = case IM.lookup num pt of
            Just ([lab], children) -> T.Node (Just lab, []) (map (g pt) children)
            Just ([], children) -> T.Node (Nothing, []) (map (g pt) children)
            Nothing -> error ("PointerTree malformed: pointer " ++ show num ++ " does not exists:\n" ++ show pt)
            _ -> error "Parser.Negra.pointerTreeToCrossedTree"
    g pt (Left num) = f num pt
    g _ (Right (leaf, pos)) = T.Node (Just leaf, [(pos, pos)]) []


-- | Calculate the correct 'Span' lists of inner nodes by propagating the
-- 'Span' lists of leaf nodes.
updateInnerSpans
  :: T.Tree (a, [Span])
  -> T.Tree (a, [Span])
updateInnerSpans (T.Node (lab, _) forest@(_:_))
  = let forest' = map updateInnerSpans forest
        spans = mergeSpans $ concatMap (snd . T.rootLabel) forest' in
    T.Node (lab, spans) forest'
updateInnerSpans node = node


-- | Merge adjacent or overlapping 'Span's in a 'Span' list.
-- The resulting 'Span' list is sorted.
mergeSpans :: [Span] -> [Span]
mergeSpans = m . L.sort
  where
    m (a:ys@(b:xs))
      = if snd a + 1 >= fst b
        then m ((fst a, max (snd a) (snd b)):xs)
        else a : m ys
    m xs = xs

{-- FIXME use QuickCheck
test_mergeSpans :: Bool
test_mergeSpans
  = mergeSpans [(1,1), (1,2), (3,4), (6,10), (7,9), (11,12)]
    == [(1,4), (6,12)]
--}

-- | Takes a tree containing correct 'Span' lists and splits nodes to remove
-- crossing edges.
--
-- The resulting tree contains \"local 'Span's\" over the former (before the
-- split) direct children and \"global 'Span's\", i.e. 'Span's over the yield.
-- There are no 'Span' lists anymore.
-- Child nodes are sorted by the (global) 'Span's.
splitCrossedTree
  :: T.Tree   (a, [Span])
  -> T.Forest ((a, Span), Span)
splitCrossedTree (T.Node (lab, spans) forest)
  = relabel 1
  $ foldr f
          (map ((,) []) spans)
          (concatMap splitCrossedTree forest)
    where
      f t@(T.Node (_, sChild) _) (current@(children, sParent):forest')
        = if fst sChild >= fst sParent && snd sChild <= snd sParent
          then (L.insertBy (comparing (snd . T.rootLabel)) t children, sParent):forest'
          else current : f t forest'
      f _ [] = error "spans malformed"
      relabel i ((g, s):xs)
        = let i' = i + length g in
          T.Node ((lab, (i, i' - 1)), s) g : relabel i' xs
      relabel _ _ = []


negraTreeToTree :: T.Tree ((Maybe SentenceData, Span), Span) -> T.Tree String
negraTreeToTree (T.Node ((Nothing, _), _) f)
  = T.Node "ROOT" (fmap negraTreeToTree f)
negraTreeToTree (T.Node ((Just dat@(SentenceNode{}), _), _) f)
  = T.Node (sdPostag dat) (fmap negraTreeToTree f)
negraTreeToTree (T.Node ((Just dat@(SentenceWord{}), _), _) [])
  = T.Node (sdPostag dat) [T.Node (hack . sdWord $ dat) []]
  where
    hack = filter (`notElem` "() ")
negraTreeToTree _
  = error "malformed negra tree: a SentenceWord has children"

  
{-- FIXME neither exported nor used
isSentenceNode :: SentenceData -> Bool
isSentenceNode (SentenceNode {}) = True
isSentenceNode _ = False


showSentenceData :: SentenceData -> String
showSentenceData SentenceWord{sdWord = w} = w
showSentenceData SentenceNode{sdPostag = t} = t
--}
