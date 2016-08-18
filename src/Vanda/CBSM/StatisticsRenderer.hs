-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CBSM.StatisticsRenderer
-- Description :  visualize statistical data generated by cbsm
-- Copyright   :  (c) Technische Universität Dresden 2016
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Visualize statistical data generated by cbsm.
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns #-}

module Vanda.CBSM.StatisticsRenderer
( renderBeam
) where


import           Codec.Picture  -- package JuicyPixels
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.List (foldl')

import qualified Control.Error
import           Vanda.Util.Timestamps (putStrLnTimestamped)


errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.CBSM.StatisticsRenderer"


-- | Visualize the beam using a heat map.
--
-- The input file is usually called @statistics-evaluations.csv@.
renderBeam
  :: FilePath  -- ^ input csv file
  -> FilePath  -- ^ output png file
  -> IO ()
renderBeam fileIn fileOut = do
  putStrLnTimestamped "Analyzing data …"
  (w, h) <- getDimensions <$> readCSV fileIn
  putStrLnTimestamped
    $ "Writing image of dimensions " ++ show w ++ "×" ++ show h ++ " …"
  writePng fileOut
    .   toImage w h
    =<< readCSV fileIn
  putStrLnTimestamped "Done."


getDimensions :: [[C.ByteString]] -> (Int, Int)
getDimensions
  =   foldl' step (0, 0)
  .   map parseRow
  .   tail
  where
    step (!w, !h) (i, _, hi, _, _) = (max w hi, max h i)


toImage :: Int -> Int -> [[C.ByteString]] -> Image PixelRGB8
toImage w h
  = snd
  . (\ acc -> generateFoldImage step acc w h)
  . concatMap (expand . parseRow)
  . tail
  where
    expand :: (Int, Int, Int, Double, Double) -> [(Int, Int, Double)]
    expand (i, bl, bh, e, _)
      = [(pred i, b, e) | b <- [pred bl .. pred bh]]

    step ((y1, x1, e) : as) x2 y2
      | x1 == x2  &&  y1 == y2  =  (as, colormap e)
    step as _ _  =  (as, errCol)

    errCol = PixelRGB8 0xFF 0xFF 0xFF


{-
toImage :: Int -> Int -> [[C.ByteString]] -> Image PixelRGB8
toImage w h
  = snd
  . (\ acc -> generateFoldImage step acc w h)
  . map (decr . parseRow)
  . tail
  where
    decr (i, bl, bh, e1, e2) = (pred i, pred bl, pred bh, e1, e2)

    step as@((i, bl, bh, e, _) : as') x y
      = if bl <= x && y == i
        then case compare x bh of
               LT -> (as , colormap1 e)
               EQ -> (as', colormap1 e)
               GT -> step as' x y
        else (as, errCol)
    step [] _ _ = ([], errCol)

    errCol = PixelRGB8 0xFF 0xFF 0xFF
-}


readCSV :: FilePath -> IO [[C.ByteString]]
readCSV file
  =   map (C.split ',')
  .   C.lines
  <$> C.readFile file


-- expected columns:
--   * iteration
--   * beam index low
--   * beam index high
--   * log₂ evaluation of merge
--   * evaluation of merge
parseRow :: [C.ByteString] -> (Int, Int, Int, Double, Double)
parseRow [x1, x2, x3, x4, x5]
  = ( unsafeReadInt x1
    , unsafeReadInt x2
    , unsafeReadInt x3
    , unsafeRead    x4
    , unsafeRead    x5
    )
  where
    unsafeRead = read . C.unpack  -- TODO: read is awfully slow!
    unsafeReadInt x
      = case C.readInt x of
          Just (i, y) -> if C.null y then i else err
          _           -> err
      where
        err = errorHere "parseRow.unsafeReadInt" "No parse."
parseRow _
  = error "parseRow" "Wrong number of columns."


-- gnuplot> show palette
--         palette is COLOR
--         rgb color mapping by rgbformulae are 7,5,15
--         figure is POSITIVE
--         all color formulae ARE NOT written into output postscript file
--         allocating ALL remaining color positions for discrete palette terminals
--         Color-Model: RGB
--         gamma is 1.5
--
-- gnuplot> show palette rgbformulae
--           * there are 37 available rgb color mapping formulae:
--              0: 0               1: 0.5             2: 1
--              3: x               4: x^2             5: x^3
--              6: x^4             7: sqrt(x)         8: sqrt(sqrt(x))
--              9: sin(90x)       10: cos(90x)       11: |x-0.5|
--             12: (2x-1)^2       13: sin(180x)      14: |cos(180x)|
--             15: sin(360x)      16: cos(360x)      17: |sin(360x)|
--             18: |cos(360x)|    19: |sin(720x)|    20: |cos(720x)|
--             21: 3x             22: 3x-1           23: 3x-2
--             24: |3x-1|         25: |3x-2|         26: (3x-1)/2
--             27: (3x-2)/2       28: |(3x-1)/2|     29: |(3x-2)/2|
--             30: x/0.32-0.78125 31: 2*x-0.84       32: 4x;1;-2x+1.84;x/0.08-11.5
--             33: |2*x - 0.5|    34: 2*x            35: 2*x - 0.5
--             36: 2*x - 1
--           * negative numbers mean inverted=negative colour component
--           * thus the ranges in `set pm3d rgbformulae' are -36..36
colormap :: Double -> PixelRGB8
colormap x
  = PixelRGB8
      (round $ 0xFF * sqrt p)
      (round $ 0xFF * p ^ (3 :: Int))
      (round $ 0xFF * (0 `max` sin (2 * pi * p)))
  where
    p = (((-20) `max` x `min` 0) + 20) / 20


{-
colormap :: Double -> PixelRGB8
colormap
  = colorgradient
    [ (-20, PixelRGB8 0x00 0x00 0x00)
    , (-15, PixelRGB8 0x00 0x00 0xFF)
    , (-10, PixelRGB8 0xFF 0x00 0x00)
    , (  0, PixelRGB8 0xFF 0xFF 0x00)
    ]


colorgradient :: [(Double, PixelRGB8)] -> Double -> PixelRGB8
colorgradient (stop0@(hi0, hiCol0) : stops0) x
  = if x <= hi0
    then hiCol0
    else go stop0 stops0
  where
    go :: (Double, PixelRGB8) -> [(Double, PixelRGB8)] -> PixelRGB8
    go (_, col) [] = col
    go (lo, PixelRGB8 lr lg lb) (stop@(hi, hiCol@(PixelRGB8 hr hg hb)) : stops)
      = case compare x hi of
          LT -> PixelRGB8 (crossfade lr hr) (crossfade lg hg) (crossfade lb hb)
          EQ -> hiCol
          GT -> go stop stops
      where
        position = (x - lo) / (hi - lo)
        crossfade from to = round $ (1 - position) * fromIntegral from + position * fromIntegral to
colorgradient [] _
  = errorHere "colorgradient" "Empty list."
-}
