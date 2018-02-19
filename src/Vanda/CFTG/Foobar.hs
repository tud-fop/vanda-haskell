-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Technische Universität Dresden 2018
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.CFTG.Foobar where


import qualified Control.Error

import           Control.Arrow (first)
import           Control.Monad (guard)
import           Data.List     (inits, (\\))


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


-- Test whether language `anyRule' is characterized by properties
allModelling :: Int -> [[Int]]
allModelling len =
 if len `mod` 2 /= 0
 then []
 else do
   w' <- sequence $ map (const [1..(len `div` 2)]) [1..(len `div` 2)]
   w  <- fmap ((0:) . (++[0])) . map concat . mapM (\x -> [[0,-x], [x,0]]) $ w'
   guard $ all ($w)
     [ dyck'
     , lengthsOK
     , gammaVsLength
     ]
   return $ map abs w


dyck' :: (Ord a, Num a) => [a] -> Bool
dyck' w = let counts = map sum $ inits $ ((tail $ init $ w) \\ [0])
          in all (>0) (init . tail $ counts) && last counts == 0
--  all (>0) (init . tail $ counts) && last counts == 0

lengthsOK :: [Int] -> Bool
lengthsOK w = all (uncurry okay) $ zip w $ concatMap (\x -> [x,x]) [1..]
  where
    okay 0 _ = True
    okay n j = if n > 0
               then n < k - j
               else -n < j - 1
    k = length w `div` 2

(-|-) :: Eq a => [a] -> [a] -> ([a],[a])
xs -|- ys = (xs \\ ys, ys \\ xs)


gammaVsLength :: [Int] -> Bool
gammaVsLength w = length w == sum (map abs w) + 4

