-- Copyright (c) 2010, Toni Dietze

module Tools.PrettyPrint
( prettyIndentBracktes
, putStrColumns
) where


import Data.List (transpose)


indentation :: Int -> String
indentation i = replicate (2 * i) ' '


prettyIndentBracktes :: String -> String
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


putStrColumns :: [String] -> [[String]] -> IO ()
putStrColumns seps cols
  = putStr
  . unlines
  . map concat
  . transpose
  $ columnalign seps cols


columnalign :: [String] -> [[String]] -> [[String]]
columnalign seps cols
  = map' (\ (sep, col, l) -> map (\ x -> trim l x ++ sep) col)
         (\ (_  , col, _) -> col)
  $ zip3 (cycle seps) cols
  $ map (maximum . map length) cols
  where
    map' _ g [x]      = [g x]
    map' f g (x : xs) = f x : map' f g xs
    map' _ _ []       = []


trim :: Int -> String -> String
trim 0 _        = []
trim n (c : cs) = c : trim (n - 1) cs
trim n []       = replicate n ' '
