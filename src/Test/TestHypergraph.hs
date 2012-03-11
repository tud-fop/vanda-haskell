{-# LANGUAGE BangPatterns #-}
module Main where

import Codec.Compression.GZip ( compress, decompress )

-- import Control.Applicative
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
-- import Vanda.Hypergraph.EdgeList as EL (toBackwardStar)

instance (Show v, Show l, Show i) => Show (Hyperedge v l i) where
  show e
    = show (to e)
      ++ " -> "
      ++ show (label e)
      ++ " "
      ++ unwords (map show $ from e)
      ++ " # "
      ++ show (i e)

-- "~lindal/Downloads/SHK/Berkeley/t_eng_sm6.txt.grammar.hg" 
-- "~lindal/Downloads/SHK/Berkeley/t_small.hg" 
-- "/home/student/lindal/Downloads/SHK/Berkeley/berkeley.bhg"

main :: IO ()
main = mainKnuth

mainKnuth :: IO ()
mainKnuth = do 
  args <- getArgs
  case args of
    ["-z", zhgfile] -> do
      weights :: VU.Vector Double
        <- fmap VU.fromList $ B.decodeFile $ zhgfile ++ ".weights"
      el :: EdgeList Int32 Int32 Int32
        <- fmap (force . B.decode . decompress) (B.readFile zhgfile)
      let pN (l,i) xs = (weights VU.! fromIntegral i) + (sum xs)
      --el `seq` print "ok"
      print
        $ (A.! 1132)
        $ knuth el (Feature pN V.singleton) (V.singleton 1)

-- heap: ~350MB
-- time: ~102s
      
{-mainNbest :: IO()
mainNbest = do                                       
  args <- getArgs
  case args of
    ["-z", zhgfile] -> do
      wghts <- B.decodeFile $ zhgfile ++ ".weights" :: IO([Double])
      hg' <- B.readFile zhgfile
      let !hg = B.decode . decompress $! hg'  :: [Hyperedge Int32 Int32 Int32]
          !weights = array (0,fromIntegral $ (length wghts) - 1) $ zip [0..] wghts 
          !el = ($!!) EdgeList (bounds weights) hg
          processNode = (\(l,i) xs -> (weights ! i) + (sum xs))
          feat = ($!!) Feature processNode V.singleton
          vector = V.singleton 1
      deepseq (wghts)
              print $ bests 
              (EL.toBackwardStar el)
              feat
              (V.singleton 1)
  -}            
-- heap: ~700MB
-- time: ~104s


