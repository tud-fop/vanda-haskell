-- Copyright (c) 2010, Toni Dietze

module TestData.TestHypergraph where


import Data.Hypergraph

import Data.Tree


testHypergraphs :: (Fractional w) => [Hypergraph Char Char w ()]
testHypergraphs
  = [ --      in     out A  out B
      -- A    2/3    1      1.6
      -- B    0.5    1/3    1
      -- C    2/3    0      0
      -- D    0      1      1.6
      -- E    0      0      0
      hypergraph
        [ hyperedge 'A' "AB" 's' 0.2 ()
        , hyperedge 'A' "BA" 's' 0.3 ()
        , hyperedge 'A' ""   'a' 0.5 ()
        , hyperedge 'B' "AA" 's' 0.9 ()
        , hyperedge 'B' ""   'b' 0.1 ()
        , hyperedge 'C' "A"  'b' 1   () -- C not reachable
        , hyperedge 'A' "D"  'b' 1   () -- D not terminating
        , hyperedge 'E' "E"  'b' 1   () -- E not reachable and not terminating
        ]
    , hypergraph
        [ hyperedge 'S' "SDS" ' ' 0.6   ()
        , hyperedge 'S' "SA"  ' ' 0.3   ()
        , hyperedge 'S' "BA"  ' ' 0.1   ()
        , hyperedge 'A' "ABC" ' ' 0.1   ()
        , hyperedge 'A' "CBA" ' ' 0.599 ()
        , hyperedge 'A' "S"   ' ' 0.3   ()
        , hyperedge 'A' ""    ' ' 0.001 ()
        , hyperedge 'B' "CBC" ' ' 0.99  ()
        , hyperedge 'B' "CBC" ' ' 0.001 ()
        , hyperedge 'B' "SAB" ' ' 0.002 ()
        , hyperedge 'B' ""    ' ' 0.007 ()
        , hyperedge 'C' "CCC" ' ' 0.6   ()
        , hyperedge 'C' "ASA" ' ' 0.2   ()
        , hyperedge 'C' "BAS" ' ' 0.199 ()
        , hyperedge 'C' ""    ' ' 0.001 ()
        , hyperedge 'D' "DA"  ' ' 0.999 ()
        , hyperedge 'D' ""    ' ' 0.001 ()
        ]
    , hypergraph
        [ hyperedge 't' "r"  't' 1.0 ()
        , hyperedge 'q' ""   'a' 0.2 ()
        , hyperedge 'r' ""   'b' 0.3 ()
        , hyperedge 's' ""   'a' 0.5 ()
        , hyperedge 'r' "qr" 's' 0.9 ()
        , hyperedge 'r' "sr" 's' 0.1 ()
        ]
    , hypergraph
        [ hyperedge 't' "q"  't' 1.0 ()
        , hyperedge 'q' ""   'b' 0.1 ()
        , hyperedge 'q' "rq" 's' 0.9 ()
        , hyperedge 'r' ""   'a' 1.0 ()
        ]
    ]


testTreess :: [[Tree Char]]
testTreess
  = [ []
    , []
    , let f = Node 's' [Node 'a' [], Node 'b' []]
            : map (\ t -> Node 's' [Node 'a' [], Node 's' [Node 'a' [], t]]) f
      in (take 4 $ map (Node 't' . (: [])) f) {-++ [Node 't' [Node 's' [Node 'a' [], Node 's' [Node 'a' [], Node 'b' []]]]]-}
    ]
