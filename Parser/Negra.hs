-- Copyright (c) 2010, Toni Dietze

module Parser.Negra where

import Parser.ApplicativeParsec
import Data.Char(isDigit, ord)
import qualified Data.IntMap as IntMap
import Data.Function(on)
import qualified Data.List as L
import qualified Data.Tree as T

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


p_negra =
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
        try (string "#BOS")
     *> negraSpaces1
     *> (p_Int >>= \num ->
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
p_Int = (many1 digit >>= return . read) <* tokenCleanup


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

-- converts a list of SentenceData to a forest of not-crossing trees
negraToForest
  :: [SentenceData]
  -> T.Forest ((Maybe SentenceData, Span), Span)
negraToForest
  = splitCrossedTree
  . updateInnerSpans
  . pointerTreeToCrossedTree
  . negraToPointerTree


-- represents a tree by using "pointers"
-- A pointer is an Int.
-- The map maps a pointer to a node.
-- A node contains a list of labels and a list of children.
-- A child is either a pointer to a child node or a leaf.
-- A leaf contains a label and the position of the child in the yield of the tree.
type PointerTree a = IntMap.IntMap ([a], [Either Pointer (a, Position)])
type Pointer = IntMap.Key
type Position = Int
type Span = (Position, Position)


-- Insert a leaf in a PointerTree.
insertLeaf
  :: (label, Position)
  -> Pointer
  -> PointerTree label
  -> PointerTree label
insertLeaf leaf parent
  = IntMap.insertWith (liftSnd . (:) . head . snd) parent ([], [Right leaf])


-- Insert an inner node in a PointerTree.
insertNode
  :: label
  -> Pointer
  -> Pointer
  -> PointerTree label
  -> PointerTree label
insertNode node ptr parent
  = IntMap.insertWith (liftSnd . (:) . head . snd) parent ([], [Left ptr])
  . IntMap.insertWith (liftFst . (:) . head . fst) ptr ([node], [])


liftFst :: (a -> c) -> (a, b) -> (c, b)
liftFst f (x, y) = (f x, y)


liftSnd :: (b -> c) -> (a, b) -> (a, c)
liftSnd f (x, y) = (x, f y)


-- Extract the PointerTree from a list of SentenceData.
negraToPointerTree :: [SentenceData] -> PointerTree SentenceData
negraToPointerTree = ins 0
  where
    ins _ [] = IntMap.empty
    ins i (x@SentenceWord{}:xs) = insertLeaf (x, i) (eParent $ sdEdge x) (ins (i+1) xs)
    ins i (x@SentenceNode{sdNum = n}:xs) = insertNode x n (eParent $ sdEdge x) (ins i xs)



-- Convert a PointerTree to a Data.Tree.Tree.
-- Span lists are used to represent crossing edges.
-- Every node of the resulting tree contains a span list.
-- Only leaf nodes will contain correct span lists!
-- A span list is a list of spans.
-- A span is a pair of yield positions of the whole tree.
-- E.g. a node has the span [(2, 4), (7, 9)]. Then the yield of the sub tree
-- located at the node contains the leafs 2, 3, 4, 7, 8 and 9 of the whole tree.
pointerTreeToCrossedTree
  :: PointerTree SentenceData
  -> T.Tree (Maybe SentenceData, [Span])
pointerTreeToCrossedTree = f 0
  where
    f num pt
      = case IntMap.lookup num pt of
            Just ([label], children) -> T.Node (Just label, []) (map (g pt) children)
            Just ([], children) -> T.Node (Nothing, []) (map (g pt) children)
            Nothing -> error "PointerTree malformed"
    g pt (Left num) = f num pt
    g _ (Right (leaf, pos)) = T.Node (Just leaf, [(pos, pos)]) []


-- Calculate the correct span lists of inner nodes by propagating the span lists of leaf nodes.
updateInnerSpans
  :: T.Tree (a, [Span])
  -> T.Tree (a, [Span])
updateInnerSpans (T.Node (label, _) forest@(_:_))
  = let forest' = map updateInnerSpans forest
        spans = mergeSpans $ concatMap (snd . T.rootLabel) forest' in
    T.Node (label, spans) forest'
updateInnerSpans node = node


-- Merge adjacent or overlapping spans in a span list.
-- The resulting span list is sorted.
mergeSpans :: [Span] -> [Span]
mergeSpans = m . L.sort
  where
    m (a:ys@(b:xs))
      = if snd a + 1 >= fst b
        then m ((fst a, max (snd a) (snd b)):xs)
        else a:(m ys)
    m xs = xs

test_mergeSpans
  = mergeSpans [(1,1), (1,2), (3,4), (6,10), (7,9), (11,12)]
    == [(1,4), (6,12)]


-- Takes a tree containing correct span lists and splits nodes to remove
-- crossing edges.
-- The resulting tree contains spans (no span lists anymore).
-- Child nodes are sorted by the spans.
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
          then (L.insertBy (compare `on` (snd . T.rootLabel)) t children, sParent):forest
          else current:(f t forest)
      f _ [] = error "spans malformed"
      relabel i ((f, s):xs)
        = let i' = i + length f in
          (T.Node ((label, (i, i' - 1)), s) f):(relabel i' xs)
      relabel _ _ = []


negraTreeToTree
  :: T.Tree ((Maybe SentenceData, Span), Span)
  -> T.Tree (String             , Span)
negraTreeToTree = fmap (liftFst (maybe "" showSentenceData) . fst)


isSentenceNode (SentenceNode {}) = True
isSentenceNode _ = False

showSentenceData SentenceWord{sdWord = w} = w
showSentenceData SentenceNode{sdPostag = t} = t

-- printSDTree = putStrLn . T.drawTree . (fmap show . fmap (fmap showSentenceData)) . toTree . sData
-- printSDTree = putStrLn . T.drawTree . fmap show . fmap (liftFst (fmap showSentenceData)) . head . negraToForest . sData

-- test = parseFromFile p_negra "Parser/corpus-sample.export" >>= \(Right x) -> printSDTree ( x !! 10)
