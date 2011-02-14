-- Copyright (c) 2010, Toni Dietze

module Parser.Negra where

import Parser.ApplicativeParsec
import Tools.Miscellaneous(mapFst, mapSnd)

import           Control.DeepSeq
import qualified Data.Binary   as B
import           Data.ByteString.Lazy (ByteString)
import           Data.Char     (isDigit, ord)
import qualified Data.IntMap   as IntMap
import qualified Data.List     as L
import           Data.Ord      (comparing)
import qualified Data.Tree     as T
import           Data.Word     (Word8)

import Debug.Trace

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


p_negra = {-fmap (safeEncode . LazyBinaryList) $-}
        p_ignoreLines
     *> (p_format >>= \format ->
              many p_table
           *> many (p_Sentence (format == 4))
        )
    <*  eof


p_format =  -- TODO stub
        try (string "#FORMAT")
     *> negraSpaces1
     *> p_Int
    <*  negraSpaces
    <*  newline
    <*  p_ignoreLines


p_table =  -- TODO stub
        try (string "#BOT")
     *> manyTill anyChar (try (string "#EOT"))
    <*  manyTill anyChar newline
    <*  p_ignoreLines


p_Sentence lemma =
        {-fmap B.encode
     $-}  try (string "#BOS")
     *> negraSpaces1
     *> (p_Int >>= \num -> {-traceShow num $-}
            Sentence num
        <$> p_Int
        <*> p_date
        <*> p_Int
        <*> optionMaybe p_comment
        <*  negraNewline
        <*> manyTill (p_SentenceData lemma) (try (string "#EOS"))
        <*  negraSpaces1
        <*  string (show num)
        <*  negraNewline
        )
    <*  p_ignoreLines


p_SentenceData lemma =
    (       SentenceNode <$> (char '#' *> p_Int)
        <|> SentenceWord <$> p_word
    )
    <*  (if lemma then p_word else return "")  -- TODO: #FORMAT 4 seems to contain the lemma here
    <*> p_word
    <*> p_word
    <*> p_Edge
    <*> manyTill p_Edge (lookAhead (negraNewline <|> string "%%" *> return '%'))
    <*> optionMaybe p_comment
    <*  negraNewline
    <*  p_ignoreLines


p_Edge = Edge <$> p_word <*> p_Int


p_comment = string "%%" *> negraSpaces *> many (noneOf "\n")

p_emptyLine = negraSpaces *> negraNewline

p_ignoreLines = many p_ignoreLine

p_ignoreLine = try p_emptyLine *> return ()
           <|> try p_comment *> negraNewline *> return ()

p_word = many1 negraNonSpace <* tokenCleanup

p_Int :: GenParser Char st Int
p_Int = fmap read (many1 digit) <* tokenCleanup


p_date = many1 (oneOf "/0123456789") <* tokenCleanup

tokenCleanup = negraSpaces1 *> return () <|> lookAhead newline *> return ()

negraSpace   = oneOf " \t"
negraSpaces  = skipMany negraSpace
negraSpaces1 = skipMany1 negraSpace

negraNonSpace = satisfy $ (<) 32 . ord

-- negraNonSpace = satisfy $
--     \c -> let o = ord c in o >= 33 && o <= 127 || o >= 160 && o <= 255

negraNewline = char '\n'

-- loadHGraph file = parseFromFile p_WTA file


--------------------------------------------------------------------------------

-- | Converts a list of 'SentenceData' to a 'Data.Tree.Forest' of
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



-- | Convert a 'PointerTree' to a 'Data.Tree.Tree'.
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
            Just ([label], children) -> T.Node (Just label, []) (map (g pt) children)
            Just ([], children) -> T.Node (Nothing, []) (map (g pt) children)
            Nothing -> error ("PointerTree malformed: pointer " ++ show pt ++ " does not exists:\n" ++ show ptrTree)
    g pt (Left num) = f num pt
    g _ (Right (leaf, pos)) = T.Node (Just leaf, [(pos, pos)]) []


-- | Calculate the correct 'Span' lists of inner nodes by propagating the
-- 'Span' lists of leaf nodes.
updateInnerSpans
  :: T.Tree (a, [Span])
  -> T.Tree (a, [Span])
updateInnerSpans (T.Node (label, _) forest@(_:_))
  = let forest' = map updateInnerSpans forest
        spans = mergeSpans $ concatMap (snd . T.rootLabel) forest' in
    T.Node (label, spans) forest'
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
splitCrossedTree (T.Node (label, spans) forest)
  = relabel 1
  $ foldr f
          (map ((,) []) spans)
          (concatMap splitCrossedTree forest)
    where
      f t@(T.Node (_, sChild) _) (current@(children, sParent):forest)
        = if fst sChild >= fst sParent && snd sChild <= snd sParent
          then (L.insertBy (comparing (snd . T.rootLabel)) t children, sParent):forest
          else current : f t forest
      f _ [] = error "spans malformed"
      relabel i ((f, s):xs)
        = let i' = i + length f in
          T.Node ((label, (i, i' - 1)), s) f : relabel i' xs
      relabel _ _ = []


{-negraTreeToTree
  :: T.Tree ((Maybe SentenceData, Span), Span)
  -> T.Tree (String             , Span)-}
-- negraTreeToTree = fmap (liftFst (liftFst (maybe "" showSentenceData)))
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


isSentenceNode (SentenceNode {}) = True
isSentenceNode _ = False

showSentenceData SentenceWord{sdWord = w} = w
showSentenceData SentenceNode{sdPostag = t} = t

-- printSDTree = putStrLn . T.drawTree . (fmap show . fmap (fmap showSentenceData)) . toTree . sData
-- printSDTree = putStrLn . T.drawTree . fmap show . fmap (liftFst (fmap showSentenceData)) . head . negraToForest . sData

-- test
--   = parseFromFile p_negra "Parser/corpus-sample.export"
--     >>= \(Right x)
--     ->  putStrLn $ T.drawForest $ fmap (fmap show) $ fmap negraTreeToTree $ negraToForest $ sData ( x !! 10)


corpusSmall = "/home/gdp/dietze/Documents/vanda/Parser/tiger_release_aug07_part.export"
corpusBig = "/var/local/share/gdp/nlp/resources/tigercorpus2.1/corpus/tiger_release_aug07.export"


data BinaryContainer a = BinaryContainer a ByteString

instance Show (BinaryContainer a) where
  show (BinaryContainer _ x) = show x

safeEncode :: (B.Binary a) => a -> BinaryContainer a
safeEncode x = BinaryContainer undefined (B.encode x)

safeDecode :: (B.Binary a) => BinaryContainer a -> a
safeDecode (BinaryContainer _ x) = B.decode x

newtype LazyBinaryList a = LazyBinaryList {unLazyBinaryList :: [a]} deriving Show

instance (B.Binary a) => B.Binary (LazyBinaryList a) where
  put (LazyBinaryList xs) = go xs
    where
      go xs
        = let maxL = 1
              (ys, zs) = splitAt maxL xs
              l        = fromIntegral (if null zs then length ys else maxL) :: Word8
              putChunk = B.put l >> mapM_ B.put ys
          in if l /= 0
          then putChunk >> go zs
          else putChunk
  get = fmap LazyBinaryList (go 0)
    where
      go 0 = B.get >>= \ l -> if (l :: Word8) /= 0 then go l else return []
      go l = (:) <$> B.get  <*> go (l - 1)

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
    0 -> SentenceWord <$> B.get <*> B.get <*> B.get <*> B.get <*> B.get <*> B.get
    1 -> SentenceNode <$> B.get <*> B.get <*> B.get <*> B.get <*> B.get <*> B.get

instance B.Binary Edge where
  put e = do
    B.put $ eLabel e
    B.put $ eParent e
  get = Edge <$> B.get <*> B.get


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

-- !!! This violates DeepSeq properties  !!!
instance NFData ParseError where
  rnf _ = ()

instance NFData ByteString where
  rnf _ = ()