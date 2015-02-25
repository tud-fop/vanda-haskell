module Vanda.CFTG.Foobar where


anyRule
  = map ((0 :) . (++ [0]) .  concatMap (\ (x, y) -> [x, y]))
  $ concat
  $ iterate (concatMap step) [[(0, 0)]]
  where
    step :: [(Int, Int)] -> [[(Int, Int)]]
    step [(x, y)]
      = [[(succ x, 0), (0, succ y)]]
    step (z@(x, y) : zs)
      = ((succ x, 0) : (0, succ y) : zs) : map (z :) (step zs)


alwaysSameRule
  = map ((0 :) . (++ [0]) .  concatMap (\ (x, y) -> [x, y]))
  $ iterate step [(0, 0)]
  where
    step = concatMap $ \ (x, y) -> [(succ x, 0), (0, succ y)]


printLanguage n = putStr . unlines . take n . map (unwords . map show)

{-
printLanguage 6 alwaysSameRule

0 0 0 0
0 1 0 0 1 0
0 2 0 0 1 1 0 0 2 0
0 3 0 0 1 1 0 0 2 2 0 0 1 1 0 0 3 0
0 4 0 0 1 1 0 0 2 2 0 0 1 1 0 0 3 3 0 0 1 1 0 0 2 2 0 0 1 1 0 0 4 0
0 5 0 0 1 1 0 0 2 2 0 0 1 1 0 0 3 3 0 0 1 1 0 0 2 2 0 0 1 1 0 0 4 4 0 0 1 1 0 0 2 2 0 0 1 1 0 0 3 3 0 0 1 1 0 0 2 2 0 0 1 1 0 0 5 0
-}
