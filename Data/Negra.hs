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


module Data.Negra where


import Tools.Miscellaneous(mapFst, mapSnd)

import Control.Applicative
import Control.DeepSeq
import qualified Data.Binary as B
import qualified Data.IntMap as IntMap
import qualified Data.List as L
import Data.Ord (comparing)
import qualified Data.Tree as T
import Data.Word (Word8)


data Sentence = Sentence
    { sId :: Int
    , sEditorId :: Int
    , sDate :: String
    , sOriginId :: Int
    , sComment :: Maybe String
    , sData :: [SentenceData]
    }
    deriving Show

data SentenceData =
    SentenceWord
    { sdWord :: String
    , sdPostag :: String
    , sdMorphtag :: String
    , sdEdge :: Edge
    , sdSecEdges :: [Edge]
    , sdComment :: Maybe String
    } |
    SentenceNode
    { sdNum :: Int
    , sdPostag :: String
    , sdMorphtag :: String
    , sdEdge :: Edge
    , sdSecEdges :: [Edge]
    , sdComment :: Maybe String
    }
    deriving Show

data Edge = Edge
    { eLabel :: String
    , eParent :: Int
    }
    deriving Show


-- | Remove 'SentenceWord's which represent punctuation from a list of
-- 'SentenceData'.
filterPunctuation :: [SentenceData] -> [SentenceData]
filterPunctuation
  = filter fltr
  where
    fltr SentenceWord{sdWord = w}
      = notElem w ["\"","''","(",")",",","-",".","/",":",";","?","``"]
    fltr SentenceNode{} = True

-- ---------------------------------------------------------------------------

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
type PointerTree a = IntMap.IntMap ([a], [Either Pointer (a, Position)])
type Pointer = IntMap.Key
type Position = Int
type Span = (Position, Position)


-- | Insert a leaf in a 'PointerTree'.
insertLeaf
  :: (label, Position)  -- ^ leaf to insert
  -> Pointer            -- ^ 'Pointer' to parent node
  -> PointerTree label
  -> PointerTree label
insertLeaf leaf parent
  = IntMap.insertWith (mapSnd . (:) . head . snd) parent ([], [Right leaf])


-- | Insert an inner node in a 'PointerTree'.
insertNode
  :: label    -- ^ node to insert
  -> Pointer  -- ^ 'Pointer' to new node
  -> Pointer  -- ^ 'Pointer' to parent node
  -> PointerTree label
  -> PointerTree label
insertNode node ptr parent
  = IntMap.insertWith (mapSnd . (:) . head . snd) parent ([], [Left ptr])
  . IntMap.insertWith (mapFst . (:) . head . fst) ptr ([node], [])


-- | Extract the 'PointerTree' from a list of 'SentenceData'.
negraToPointerTree :: [SentenceData] -> PointerTree SentenceData
negraToPointerTree = ins 0
  where
    ins _ [] = IntMap.empty
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
pointerTreeToCrossedTree ptrTree = f 0 ptrTree
  where
    f num pt
      = case IntMap.lookup num pt of
            Just ([lab], children) -> T.Node (Just lab, []) (map (g pt) children)
            Just ([], children) -> T.Node (Nothing, []) (map (g pt) children)
            Nothing -> error ("PointerTree malformed: pointer " ++ show pt ++ " does not exists:\n" ++ show ptrTree)
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


test_mergeSpans :: Bool
test_mergeSpans
  = mergeSpans [(1,1), (1,2), (3,4), (6,10), (7,9), (11,12)]
    == [(1,4), (6,12)]


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


{-negraTreeToTree
  :: T.Tree ((Maybe SentenceData, Span), Span)
  -> T.Tree (String             , Span)-}
-- negraTreeToTree = fmap (liftFst (liftFst (maybe "" showSentenceData)))
{-
negraTreeToTree (T.Node ((Nothing, sl), sg) f)
  = T.Node ("", "", sl, sg) (fmap negraTreeToTree f)
negraTreeToTree (T.Node ((Just dat@(SentenceNode{}), sl), sg) f)
  = T.Node (sdPostag dat, eLabel $ sdEdge dat, sl, sg) (fmap negraTreeToTree f)
negraTreeToTree (T.Node ((Just dat@(SentenceWord{}), sl), sg) [])
  = T.Node (sdPostag dat, eLabel $ sdEdge dat, sl, sg) [
      T.Node (sdMorphtag dat, "", sl, sg) [
        T.Node (sdWord dat, "", sl, sg) []
      ]
    ]
negraTreeToTree _
  = error "malformed negra tree: a SentenceWord has children"
-}

negraTreeToTree :: T.Tree ((Maybe SentenceData, Span), Span) -> T.Tree String
negraTreeToTree (T.Node ((Nothing, _), _) f)
  = T.Node "ROOT" (fmap negraTreeToTree f)
negraTreeToTree (T.Node ((Just dat@(SentenceNode{}), _), _) f)
  = T.Node (sdPostag dat) (fmap negraTreeToTree f)
negraTreeToTree (T.Node ((Just dat@(SentenceWord{}), _), _) [])
  = T.Node (sdPostag dat) [T.Node (sdWord dat) []]
negraTreeToTree _
  = error "malformed negra tree: a SentenceWord has children"


isSentenceNode :: SentenceData -> Bool
isSentenceNode (SentenceNode {}) = True
isSentenceNode _ = False


showSentenceData :: SentenceData -> String
showSentenceData SentenceWord{sdWord = w} = w
showSentenceData SentenceNode{sdPostag = t} = t


-- printSDTree = putStrLn . T.drawTree . (fmap show . fmap (fmap showSentenceData)) . toTree . sData
-- printSDTree = putStrLn . T.drawTree . fmap show . fmap (liftFst (fmap showSentenceData)) . head . negraToForest . sData

-- test
--   = parseFromFile p_negra "Parser/corpus-sample.export"
--     >>= \(Right x)
--     ->  putStrLn $ T.drawForest $ fmap (fmap show) $ fmap negraTreeToTree $ negraToForest $ sData ( x !! 10)


corpusSmall :: String
corpusSmall = "/home/gdp/dietze/Documents/vanda/Parser/tiger_release_aug07_part.export"
corpusBig :: String
corpusBig = "/var/local/share/gdp/nlp/resources/tigercorpus2.1/corpus/tiger_release_aug07.export"

-- ---------------------------------------------------------------------------

instance B.Binary Sentence where
  put s = do
    B.put $ sId s
    B.put $ sEditorId s
    B.put $ sDate s
    B.put $ sOriginId s
    B.put $ sComment s
    B.put $ sData s
  get = Sentence <$> B.get <*> B.get <*> B.get <*> B.get <*> B.get <*> B.get

instance B.Binary SentenceData where
  put sd@SentenceWord{} = do
    B.put (0 :: Word8)
    B.put $ sdWord sd
    B.put $ sdPostag sd
    B.put $ sdMorphtag sd
    B.put $ sdEdge sd
    B.put $ sdSecEdges sd
    B.put $ sdComment sd
  put sd@SentenceNode{} = do
    B.put (1 :: Word8)
    B.put $ sdNum sd
    B.put $ sdPostag sd
    B.put $ sdMorphtag sd
    B.put $ sdEdge sd
    B.put $ sdSecEdges sd
    B.put $ sdComment sd
  get = B.get >>= \t -> case (t :: Word8) of
    0 -> SentenceWord
            <$> B.get <*> B.get <*> B.get <*> B.get <*> B.get <*> B.get
    1 -> SentenceNode
            <$> B.get <*> B.get <*> B.get <*> B.get <*> B.get <*> B.get
    _ -> error "Parser.Negra2.get"

instance B.Binary Edge where
  put e = do
    B.put $ eLabel e
    B.put $ eParent e
  get = Edge <$> B.get <*> B.get

-- ---------------------------------------------------------------------------

instance NFData Sentence where
  rnf s = rnf (sId s)
    `seq` rnf (sEditorId s)
    `seq` rnf (sDate s)
    `seq` rnf (sOriginId s)
    `seq` rnf (sComment s)
    `seq` rnf (sData s)

instance NFData SentenceData where
  rnf sd@SentenceWord{} =
          rnf (sdWord sd)
    `seq` rnf (sdPostag sd)
    `seq` rnf (sdMorphtag sd)
    `seq` rnf (sdEdge sd)
    `seq` rnf (sdSecEdges sd)
    `seq` rnf (sdComment sd)
  rnf sd@SentenceNode{} =
          rnf (sdNum sd)
    `seq` rnf (sdPostag sd)
    `seq` rnf (sdMorphtag sd)
    `seq` rnf (sdEdge sd)
    `seq` rnf (sdSecEdges sd)
    `seq` rnf (sdComment sd)

instance NFData Edge where
  rnf e = rnf (eLabel e)
    `seq` rnf (eParent e)
