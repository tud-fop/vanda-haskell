{-# LANGUAGE BangPatterns, RankNTypes #-}
module Main where

import Codec.Compression.GZip ( decompress )

import Control.Monad.ST
import qualified Data.Map as M
import Control.DeepSeq ( deepseq, force )
import qualified Data.Array as A
import qualified Data.Array.IArray as IA
import qualified Data.Array.MArray as MA
import qualified Data.Array.ST as STA
import qualified Data.Array.Unboxed as UA
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import Data.Int ( Int32 )
import Data.STRef
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Debug.Trace
import System.Environment ( getArgs )

import Vanda.Features
import Vanda.Functions ( GHKM )
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

compute :: [Hyperedge v l i] -> [Int]
compute es =
  let
    -- a :: [Hyperedge v l i] -> (forall s0. ST s0 (STA.STUArray s0 Int Int))
    -- a :: forall s0. ST s0 (STA.STUArray s0 Int Int)
    a es = do
      -- test <- newSTRef (0 :: Int)
      ar <- MA.newArray (0, 1000) 0 :: ST s (STA.STUArray s Int Int) 
      sequence_ $ map (ac ar . arity) es
      return ar
    -- ac :: STA.STUArray s Int Int -> Int -> ST s ()
    ac ar i = do
      n :: Int <- MA.readArray ar i
      let n' = n + 1
      n' `seq` MA.writeArray ar i n'
  -- in ((IA.elems . STA.runSTUArray) :: (forall s. ST s (STA.STUArray s Int Int)) -> [Int]) $ a
  in IA.elems $ STA.runSTUArray $ a es


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
    ["-b", bhgFile, "-s", statFile] -> do
      el :: EdgeList Token (GHKM Token) Int
        <- fmap
           ( B.decode
           . decompress
           )
           $ B.readFile (bhgFile ++ ".bhg.gz")
      -- stat :: [Int]
      let stat = compute (edges el)
      T.writeFile statFile $ T.unlines $ map (T.pack . show) $ stat
    ["-z", zhgFile, "-t", tokFile] -> do
      el :: EdgeList Int32 Int32 Int32
        <- fmap (B.decode . decompress) (B.readFile zhgFile)
      weights :: VU.Vector Double
        <- fmap (VU.fromList . B.decode . decompress)
           $ B.readFile (zhgFile ++ ".weights.gz")
      tok :: TokenArray
        <- fmap fromText $ T.readFile tokFile
      nodes :: TokenArray
        <- fmap fromText $ T.readFile (zhgFile ++ ".nodes")
      let pN !_ !i xs = (weights VU.! fromIntegral i) * Prelude.product xs
      weights `seq` el `deepseq`
        putStr
        $ makeItSo tok nodes
        $ (!! 0)
        $ (M.! 1132)
        $ knuth el (Feature pN V.singleton) (V.singleton 1)
    _ -> error "Usage: TestHypergraph -z zhgFile -t tokenFile"

