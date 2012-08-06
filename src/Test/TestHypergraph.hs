{-# LANGUAGE BangPatterns #-}
module Main where

import Codec.Compression.GZip ( decompress )

import qualified Data.Map as M
import Control.DeepSeq ( force)
import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import Data.Int ( Int32 )
import qualified Data.Text.Lazy.IO as T
import qualified Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Environment ( getArgs )

import Vanda.Features
import Vanda.Hypergraph hiding ( nodes )
import Vanda.Hypergraph.Binary ()
import Vanda.Hypergraph.NFData ()
import Vanda.Token

{-
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
-}

makeItSo
  :: TokenArray -> TokenArray -> Candidate Token Token Int32 x -> String
makeItSo tok nodes (Candidate w d _)
  = show w ++ " -- " ++ makeItSo' tok nodes d
makeItSo'
  :: TokenArray -> TokenArray -> Derivation Token Token Int32 -> String
makeItSo' tok _ (T.Node e [])
  = getString tok (label e)
makeItSo' tok nodes (T.Node e es)
  = "(" ++ getString tok (label e) ++ " "
    ++ unwords (map (makeItSo' tok nodes) es)
    ++ ")"

main :: IO ()
main = do 
  args <- getArgs
  case args of
    [] -> do
      let he = head (B.decode (B.encode (take 500000 (repeat (mkHyperedge (1::Int) [1] (1::Int) (1::Int)))))) :: Hyperedge Int Int Int
      print he
    ["-z", zhgFile, "-t", tokFile] -> do
      weights :: VU.Vector Double
        <- fmap
             (VU.fromList . B.decode . decompress)
           $ B.readFile
           $ zhgFile ++ ".weights.gz"
      el :: EdgeList Int32 Int32 Int32
        <- fmap
             (force . B.decode . decompress)
           $ B.readFile zhgFile
      tok :: TokenArray
        <- fmap fromText $ T.readFile tokFile
      nodes :: TokenArray
        <- fmap fromText $ T.readFile (zhgFile ++ ".nodes")
      let pN !_ !i xs = (weights VU.! fromIntegral i) * Prelude.product xs
      --el `seq` print "ok"
      weights `seq` el `seq` putStr
        -- $ unlines
        -- $ map show
        $ makeItSo tok nodes
        $ (!! 0)
        $ (M.! 1132)
        $ knuth el (Feature pN V.singleton) (V.singleton 1)
    _ -> error "Usage: TestHypergraph -z zhgFile -t tokenFile"

