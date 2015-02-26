module Vanda.CFTG.Foobar where


import qualified Control.Error

import           Control.Arrow (first)


errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.CFTG.Foobar"


-- language partitioned by length of words
anyRule :: [[[Int]]]
anyRule
  = map (map finalize)
  $ iterate (concatMap (uncurry step)) [([(0, 0)], [])]
  where
    step :: [(Int, Int)] -> [(Int, Int)] -> [([(Int, Int)], [(Int, Int)])]
    step (z : []) done = [(expand z, done)]
    step (z : zs) done = (expand z, zs ++ done)
                       : map (first (z :)) (step zs done)
    step _        _    = errorHere "anyRule.step" "unexpected pattern"

    expand :: (Int, Int) -> [(Int, Int)]
    expand (x, y) = [(succ x, 0), (0, succ y)]

    finalize :: ([(Int, Int)], [(Int, Int)]) -> [Int]
    finalize = (0 :) . (++ [0])
             . concatMap (\ (x, y) -> [x, y])
             . uncurry (++)


alwaysSameRule :: [[Int]]
alwaysSameRule
  = map ((0 :) . (++ [0]) .  concatMap (\ (x, y) -> [x, y]))
  $ iterate step [(0, 0)]
  where
    step = concatMap $ \ (x, y) -> [(succ x, 0), (0, succ y)]


printLanguage :: Show a => [[a]] -> IO ()
printLanguage = mapM_ printWord


printWord :: Show a => [a] -> IO ()
printWord = putStrLn . concat . shoveIn (cycle [" ", "  "]) . map show


printLanguage' :: Show a => [[a]] -> IO ()
printLanguage' = mapM_ printWord'


printWord' :: Show a => [a] -> IO ()
printWord' = putStrLn . spacey . map show


spacey :: [String] -> String
spacey = concat . shoveIn (map (flip replicate ' ') seps)


shoveIn :: [a] -> [a] -> [a]
shoveIn _        (ys@[_]) = ys
shoveIn (x : xs) (y : ys) = y : x : shoveIn xs ys
shoveIn _        []       = []
shoveIn _        _        = errorHere "shoveIn" "first list too short"


seps :: [Int]
seps = go [] 1
  where
    go xs i = i : xs ++ go (xs ++ i : xs) (succ i)


pairUp :: [a] -> [(a, a)]
pairUp (x : y : zs) = (x, y) : pairUp zs
pairUp []           = []
pairUp _            = errorHere "pairUp" "uneven list length"
