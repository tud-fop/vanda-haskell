-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Toni Dietze 2011
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module TestData.TestWTA where

import Data.Hypergraph
import Data.WTA


testWTAs :: (Num w) => [WTA Char Char w ()]
testWTAs =
    [ wtaCreate  -- 0
        []
        []
    , wtaCreate  -- 1
        [('f', 1)]
        [ hyperedge 'q' ""   'a' 2 ()
        , hyperedge 'q' ""   'b' 3 ()
        , hyperedge 'f' "qf" 'a' 1 ()
        , hyperedge 'f' "qq" 'a' 1 ()
        ]
    , wtaCreate  -- 2
        [('q', 1), ('r', 1)]
        [ hyperedge 'q' ""   'a' 2 ()
        , hyperedge 'r' ""   'b' 3 ()
        , hyperedge 'r' "q"  'd' 1 ()
        , hyperedge 'q' "r"  'g' 1 ()
        , hyperedge 'q' "q"  'h' 1 ()
        , hyperedge 'q' "qq" 's' 1 ()
        , hyperedge 'r' "rr" 's' 1 ()
        ]
    , wtaCreate  -- 3
        [('q', 1), ('r', 1)]
        [ hyperedge 'q' ""   'a' 2 ()
        , hyperedge 'q' ""   'b' 3 ()
        , hyperedge 'q' "q"  'd' 1 ()
        , hyperedge 'r' "r"  'g' 1 ()
        , hyperedge 'r' "qq" 's' 1 ()
        , hyperedge 'r' "qr" 's' 1 ()
        ]
    , wtaCreate  -- 4
        [('s', 1), ('r', 1)]
        [ hyperedge 'r' "ssr" 'g' 2 ()
        , hyperedge 'r' "sr"  's' 3 ()
        , hyperedge 'r' "r"   'a' 1 ()
        , hyperedge 'r' "s"   'a' 1 ()
        , hyperedge 's' ""    'a' 1 ()
        , hyperedge 's' ""    'b' 1 ()
        ]
    , wtaCreate  -- 5
        [('f', 1)]
        [ hyperedge 'a' ""   'a' 2 ()
        , hyperedge 'b' ""   'b' 3 ()
        , hyperedge 'a' "aa" 'c' 1 ()
        , hyperedge 'b' "bb" 'c' 1 ()
        , hyperedge 'f' "ba" 'c' 1 ()
        , hyperedge 'f' "bf" 'c' 1 ()
        , hyperedge 'f' "fa" 'c' 1 ()
        ]
    , wtaCreate  -- 6
        [('q', 1)]
        [ hyperedge 'q' ""    'a' 2 ()
        , hyperedge 'q' ""    'b' 3 ()
        , hyperedge 'q' "qq"  '2' 1 ()
        , hyperedge 'q' "qqq" '3' 1 ()
        ]
    , wtaCreate  -- 7
        [('l', 1), ('r', 1)]
        [ hyperedge 'q' ""   'a' 2 ()
        , hyperedge 'l' ""   'a' 2 ()
        , hyperedge 'r' ""   'a' 2 ()
        , hyperedge 'l' "lr" 'l' 1 ()
        , hyperedge 'r' "lr" 'r' 1 ()
        ]
    , wtaCreate  -- 8
        [('f', 1)]
        [ hyperedge 'q' ""   'a' 2 ()
        , hyperedge 'y' ""   'a' 2 ()
        , hyperedge 'q' ""   'b' 3 ()
        , hyperedge 'f' "qf" 'a' 1 ()
        , hyperedge 'f' "fq" 'a' 1 ()
        , hyperedge 'f' "qq" 'a' 1 ()
        , hyperedge 'f' "qw" 'a' 1 ()
        ]
    , wtaCreate  -- 9
        [('f', 1)]
        [ hyperedge '0' ""   'a' 2 ()
        , hyperedge '1' "0"  'b' 2 ()
        , hyperedge '2' "1"  'c' 3 ()
        , hyperedge '3' "21" 'd' 1 ()
        , hyperedge '3' "12" 'd' 1 ()
        ]
    , wtaCreate  -- 10
        [('f', 1)]
        [ hyperedge 'f' ""   'l' 2 ()
        , hyperedge 'q' ""   'l' 3 ()
        , hyperedge 'f' "qf" 'n' 1 ()
        , hyperedge 'f' "qq" 'n' 1 ()
        ]
    ]
