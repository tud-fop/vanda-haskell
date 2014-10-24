module Vanda.Util.Timestamps
( printTimestamp
, putStrLnTimestamped
, putStrLnTimestamped'
) where


import Control.DeepSeq
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (getZonedTime)
import System.CPUTime (cpuTimePrecision, getCPUTime)
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)



printTimestamp :: IO ()
printTimestamp = do
  cpuTime <- getCPUTime
  wallclockTime <- getZonedTime
  printf format
    (1e-12 * fromIntegral cpuTime :: Double)
    (formatTime defaultTimeLocale "%F %T %Z" wallclockTime)


putStrLnTimestamped :: String -> IO ()
putStrLnTimestamped cs = printTimestamp >> putStrLn cs


putStrLnTimestamped' :: String -> IO ()
putStrLnTimestamped' cs = cs `deepseq` printTimestamp >> putStrLn cs


format :: String
format = "[%" ++ show (9 + precision) ++ "." ++ show precision ++ "f | %s] "


precision :: Integer
precision = let p = 12 - logI cpuTimePrecision in if p < 0 then 0 else p


logI :: Integer -> Integer
logI = go 0
  where
    go e n
      | n <= 1    = e
      | otherwise = go (e + 1) (div n 10)
