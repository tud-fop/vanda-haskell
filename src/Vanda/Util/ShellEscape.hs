-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Util.ShellEscape
-- Description :  escape strings to be used as command line arguments
-- Copyright   :  (c) Technische Universität Dresden 2017
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Escape 'String's such that they can be used as command line arguments in
-- a shell.
-----------------------------------------------------------------------------

module Vanda.Util.ShellEscape where


import Data.Char


data Quoting = None | Single | Double deriving (Eq, Read, Show)


-- | Escape a 'String' as needed.
shellescape :: String -> String
shellescape cs
  = case analyze cs of
      None   -> cs
      Single -> shellescapeSingle cs
      Double -> shellescapeDouble cs


-- | Analyze a 'String' to find a suitable quoting.
analyze :: String -> Quoting
analyze = goNone
  where
    goNone cs@(c : cs') | elem c allowedUnquoted = goNone cs'
                        | otherwise              = goSingle cs
    goNone [] = None

    goSingle (c : cs) | c == '\'' = Double
                      | otherwise = goSingle cs
    goSingle [] = Single


-- | 'Char'acters that are allowed without quoting.
--
-- To find characters that do not need to be quoted in bash run:
--
-- > for i in {0..255}; do printf "%q\n" "$(echo -e "\x$(printf '%X' "$i")")"; done | grep "^[^\\$']"
allowedUnquoted :: [Char]
allowedUnquoted
   = "%+-./0123456789:=@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz"


-- | Escape a 'String' in single quotes.
shellescapeSingle :: String -> String
shellescapeSingle cs = '\'' : concatMap f cs ++ "'"
  where
    f '\0' = ""
    f '\'' = "'\"'\"'"
    f c    = [c]


-- | Escape a 'String' in double quotes.
shellescapeDouble :: String -> String
shellescapeDouble cs = '"' : concatMap f cs ++ "\""
  where
    f '\0' = ""
    f '!'  = "\"'!'\""
    f c | elem c "$`\"\\" = ['\\', c]
        | otherwise       = [c]
