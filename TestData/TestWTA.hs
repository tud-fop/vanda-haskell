-- Copyright (c) 2010, Toni Dietze

module TestData.TestWTA where

import Data.WTA


testWTAs =
    [ create  -- 0
        []
        []
    , create  -- 1
        [ Transition 'a' 'q' ""   2
        , Transition 'b' 'q' ""   3
        , Transition 'a' 'f' "qf" 1
        , Transition 'a' 'f' "qq" 1
        ]
        [('f', 1)]
    , create  -- 2
        [ Transition 'a' 'q' ""   2
        , Transition 'b' 'r' ""   3
        , Transition 'd' 'r' "q"  1
        , Transition 'g' 'q' "r"  1
        , Transition 'h' 'q' "q"  1
        , Transition 's' 'q' "qq" 1
        , Transition 's' 'r' "rr" 1
        ]
        [('q', 1), ('r', 1)]
    , create  -- 3
        [ Transition 'a' 'q' ""   2
        , Transition 'b' 'q' ""   3
        , Transition 'd' 'q' "q"  1
        , Transition 'g' 'r' "r"  1
        , Transition 's' 'r' "qq" 1
        , Transition 's' 'r' "qr" 1
        ]
        [('q', 1), ('r', 1)]
    , create  -- 4
        [ Transition 'g' 'r' "ssr" 2
        , Transition 's' 'r' "sr"  3
        , Transition 'a' 'r' "r"   1
        , Transition 'a' 'r' "s"   1
        , Transition 'a' 's' ""    1
        , Transition 'b' 's' ""    1
        ]
        [('s', 1), ('r', 1)]
    , create  -- 5
        [ Transition 'a' 'a' ""   2
        , Transition 'b' 'b' ""   3
        , Transition 'c' 'a' "aa" 1
        , Transition 'c' 'b' "bb" 1
        , Transition 'c' 'f' "ba" 1
        , Transition 'c' 'f' "bf" 1
        , Transition 'c' 'f' "fa" 1
        ]
        [('f', 1)]
    , create  -- 6
        [ Transition 'a' 'q' ""    2
        , Transition 'b' 'q' ""    3
        , Transition '2' 'q' "qq"  1
        , Transition '3' 'q' "qqq" 1
        ]
        [('q', 1)]
    , create  -- 7
        [ Transition 'a' 'q' ""   2
        , Transition 'a' 'l' ""   2
        , Transition 'a' 'r' ""   2
        , Transition 'l' 'l' "lr" 1
        , Transition 'r' 'r' "lr" 1
        ]
        [('l', 1), ('r', 1)]
    , create  -- 8
        [ Transition 'a' 'q' ""   2
        , Transition 'a' 'y' ""   2
        , Transition 'b' 'q' ""   3
        , Transition 'a' 'f' "qf" 1
        , Transition 'a' 'f' "fq" 1
        , Transition 'a' 'f' "qq" 1
        , Transition 'a' 'f' "qw" 1
        ]
        [('f', 1)]
    , create  -- 9
        [ Transition 'a' '0' ""   2
        , Transition 'b' '1' "0"  2
        , Transition 'c' '2' "1"  3
        , Transition 'd' '3' "21" 1
        , Transition 'd' '3' "12" 1
        ]
        [('f', 1)]
    ]
