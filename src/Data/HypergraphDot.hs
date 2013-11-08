-- (c) 2013 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

-- | Export 'Hypergraph's to <http://www.graphviz.org/ Graphviz> DOT language.
module Data.HypergraphDot
( hypergraph2dot
, fullHypergraph2dot
, render  -- reexport from Text.PrettyPrint
) where


import Data.Hypergraph
import qualified Data.Map as M
import Text.PrettyPrint


-- | Generate a 'Doc' which represents the 'Hypergraph' as a DOT digraph.
-- You can use 'render' to generate a 'String'.
hypergraph2dot
  :: (Ord v, Show l, Show v)
  => Hypergraph v l w i
  -> Doc
hypergraph2dot = fullHypergraph2dot show (show . eLabel) ""


-- | Extended version of 'hypergraph2dot'.
fullHypergraph2dot
  :: Ord v
  => (v -> String)                  -- ^ vertex label renderer
  -> (Hyperedge v l w i -> String)  -- ^ hyperedge label renderer
  -> String                         -- ^ graph label
  -> Hypergraph v l w i             -- ^ the transformed 'Hypergraph'
  -> Doc                            -- ^ resulting DOT digraph
fullHypergraph2dot drawVertex drawEdge name hg
  = text "digraph"
  <+> escape name
  $+$ (braces $ sep $ punctuate semi $ map (sep . punctuate semi)
        [ map (\ (v, i) -> int i <+> vertexOpts v) $ M.toList mapping
        , map (\ (i, e) -> int i <+> edgeOpts e) $ zip [-1, -2 ..] $ edges hg
        , map (edge2dot vertex) $ zip [-1, -2 ..] $ edges hg
        ]
      )
  where
    vertexOpts v = opts
      [(text "label", escape $ drawVertex v)]
    edgeOpts e = opts 
      [ (text "shape", text "rectangle")
      , (text "label", escape $ drawEdge e)]
    vertex x = int $ M.findWithDefault 0 x mapping
    mapping = M.fromList $ flip zip [1 ..] $ vertices hg


-- | Convert a single 'Hyperedge' to DOT.
edge2dot :: (v -> Doc) -> (Int, Hyperedge v l w i) -> Doc
edge2dot vertex (i, e)
  = hsep
  $ punctuate semi
  $ map (\ (j, doc) -> doc <+> opts [(text "label", int j)])
  $ zip [0 ..]
  $ (int i <+> text "->" <+> (vertex $ eHead e))
    : map (\ x -> vertex x <+> text "->" <+> int i) (eTail e)


-- | Compose a list of DOT options.
opts :: [(Doc, Doc)] -> Doc
opts = brackets . sep . punctuate comma . map (\ (k, v) -> k <> equals <> v)


-- | Escape a 'String' for DOT.
escape :: String -> Doc
escape = doubleQuotes . text . go
  where
    go ('"' : cs) = '\\' : '"' : go cs
    go (c   : cs) = c          : go cs
    go []         = []
