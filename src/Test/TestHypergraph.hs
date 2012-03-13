{-# LANGUAGE BangPatterns #-}
module Main where

import Codec.Compression.GZip ( compress, decompress )

import Control.DeepSeq ( ($!!) , deepseq , force)
import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import Data.Int ( Int8, Int16, Int32 )
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Environment ( getArgs )

import Vanda.Features
import Vanda.Hypergraph
import Vanda.Hypergraph.Binary
import Vanda.Hypergraph.NFData

instance (Show v, Show l, Show i) => Show (Hyperedge v l i) where
  show e
    = show (to e)
      ++ " -> "
      ++ show (label e)
      ++ " "
      ++ unwords (map show $ from e)
      ++ " # "
      ++ show (i e)

instance (Show v, Show l, Show i) => Show (Candidate v l i x) where
  show (Candidate w d _) = show w ++ " -- " ++ show d


main :: IO ()
main = do 
  args <- getArgs
  case args of
    ["-z", zhgfile] -> do
      weights :: VU.Vector Double
        <- fmap
             (VU.fromList . B.decode . decompress)
           $ B.readFile
           $ zhgfile ++ ".weights.gz"
      el :: EdgeList Int32 Int32 Int32
        <- fmap
             (force . B.decode . decompress)
           $ B.readFile zhgfile
      let pN !l !i xs = (weights VU.! fromIntegral i) + (sum xs)
      --el `seq` print "ok"
      weights `seq` el `seq` putStr
        $ unlines
        $ map show
        $ take 25000
        $ (A.! 1132)
        $ bests el (Feature pN V.singleton) (V.singleton 1)

