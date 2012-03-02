module Main where

import Codec.Compression.GZip ( compress, decompress )

import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import Data.Int ( Int8 )
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V
import System.Environment ( getArgs )

import Vanda.Hypergraph
import Vanda.Hypergraph.Binary
import Vanda.Hypergraph.Text

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
main = do 
  args <- getArgs
  case args of
    ["-h", hgfile, "-b", bhgfile] -> do
      text1 <- T.readFile hgfile 
      text2 <- T.readFile hgfile
      let (EdgeList vs1 es1) 
              = parseHypergraph text1 :: EdgeList (Int, Int8) Int Double
      let (EdgeList vs2 es2) 
              = parseHypergraph text2 :: EdgeList (Int, Int8) Int Double
      B.encodeFile bhgfile (EdgeList vs1 es2)
    ["-h", hgfile, "-z", zhgfile] -> do
      text1 <- T.readFile hgfile 
      text2 <- T.readFile hgfile
      let (EdgeList vs1 es1) 
              = parseHypergraph text1 :: EdgeList (Int, Int8) Int Double
      let (EdgeList vs2 es2) 
              = parseHypergraph text2 :: EdgeList (Int, Int8) Int Double
      B.writeFile zhgfile $ compress $ B.encode (EdgeList vs1 es2)
    ["-b", bhgfile, "-z", zhgfile] -> do
      bhgstring <- B.readFile bhgfile
      B.writeFile zhgfile (compress bhgstring)
    ["-b", bhgfile] -> do
      bhgstring <- B.readFile bhgfile
      let (EdgeList vs es) = B.decode bhgstring :: EdgeList (Int, Int8) Int Double
      putStr $ unlines $ map show $ take 1000 $ {-drop 1000000 $-} es 
    ["-z", zhgfile] -> do
      bhgstring <- fmap decompress $ B.readFile zhgfile
      let (EdgeList vs es) = B.decode bhgstring :: EdgeList (Int, Int8) Int Double
      putStr $ unlines $ map show $ take 1000 $ {-drop 1000000 $-} es 
      -- putStr $ show vs 
      -- unlines $ map show es
