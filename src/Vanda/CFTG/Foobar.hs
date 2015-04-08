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



-- try: mapM_ (putStrLn . blockdiagramASCII) $ take 6 alwaysSameRule
blockdiagramASCII :: [Int] -> String
blockdiagramASCII xs
  = unlines
  $ (concat (map show xs) :)
  $ map line [1 .. maximum xs]
  where line n = map (\ x -> if x >= n then '█' else ' ') xs


-- try: writeFile "test.svg" $ blockdiagramSVG $ alwaysSameRule !! 13
blockdiagramSVG :: [Int] -> String
blockdiagramSVG xs
  = unlines
      [ svgHead (show (2 * length xs)) (show (yScale * maximum xs))
      , unlines $ map line $ zip [0 ..] xs
      , svgTail
      ]
  where
    yScale = 100
    line :: (Int, Int) -> String
    line (x, y)
      = "<line x1=\"" ++ show (2 * x + 1)
      ++ "\" y1=\"" ++ "0"
      ++ "\" x2=\"" ++ show (2 * x + 1)
      ++ "\" y2=\"" ++ show (yScale * y)
      ++ "\" stroke=\"black\" stroke-width=\"2\"/>"


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


-- https://de.wikipedia.org/wiki/Scalable_Vector_Graphics#Dokument
svgHead :: String -> String -> String
svgHead w h = unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
  , ""
  , "<svg xmlns=\"http://www.w3.org/2000/svg\""
  , "     xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:ev=\"http://www.w3.org/2001/xml-events\""
  , "     version=\"1.1\" baseProfile=\"full\""
  , "     width=\"" ++ w  ++ "\" height=\"" ++ h ++ "\">"
  ]

svgTail :: String
svgTail = "</svg>"
