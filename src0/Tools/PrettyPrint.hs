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

module Tools.PrettyPrint
( lazyAlign
, prettyIndentBracktes
, putStrColumns
) where


import Data.List (findIndex, intercalate, transpose)


-- | Align many columns at a special character. The alignement in the columns
-- is based on the widest entry seen so far, respectively. For example:
--
-- >>> putStr $ lazyAlign '.' [["1.1", "0."], ["1.11", "0."], ["1.1", "0."]]
-- 1.1 0.
-- 1.11 0.
-- 1.1  0.
lazyAlign :: Char -> [[String]] -> String
lazyAlign c = unlines . go []
  where
    go _ [] = []
    go als (ss : ls)
      = let (als', ss') = unzip $ map align $ zip (als ++ repeat (0, 0)) ss
        in intercalate " " ss' : go als' ls
    align ((l, r), cs)
      = let len = length cs in
        case findIndex (c ==) cs of
          Nothing -> let l' = max l len in
            ( (l', r)
            , replicate (l' - len) ' ' ++ cs ++ replicate (r + 1) ' '
            )
          Just i -> let {l' = max l i; r' = max r (len - i)} in
            ( (l', r')
            , replicate (l' - i) ' ' ++ cs ++ replicate (r' - len + i) ' '
            )


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
