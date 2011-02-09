-- Copyright (c) 2010, Toni Dietze

module Tools.PrettyPrint(prettyIndentBracktes) where

indentation i = replicate (2 * i) ' '

prettyIndentBracktes
    :: String
    -> String
prettyIndentBracktes s
    = pp s 0 False
      where
        pp [] _ _
            = []
        pp (c:cs) i l
            | c `elem` "([{" = '\n' : indentation  i    ++ c : pp cs (i+1) True
            | c `elem` ")]}" = '\n' : indentation (i-1) ++ c : pp cs (i-1) True
            | c `elem` " "   = c : pp cs i l
            | otherwise      = (if l then '\n' : indentation i else []) ++ c : pp cs i False

