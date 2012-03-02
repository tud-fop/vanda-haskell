module Main where

import qualified Data.Binary as B
import Data.Int ( Int8 )
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V

import Vanda.Hypergraph
import Vanda.Hypergraph.Binary
import Vanda.Hypergraph.Text

instance Show s => Show (Hyperedge (s, Int8) s Double) where
  show (Hyperedge t f s i)
    = show t
      ++ " -> "
      ++ show s
      ++ " # "
      ++ show i

main :: IO ()
main = do 
        text1 <- T.readFile
          "/home/student/lindal/Downloads/SHK/Berkeley/t_eng_sm6.txt.grammar.hg" 
        text2 <- T.readFile
          "/home/student/lindal/Downloads/SHK/Berkeley/t_eng_sm6.txt.grammar.hg" 
          -- "/home/student/lindal/Downloads/SHK/Berkeley/t_small.hg" 
        let (EdgeList vs1 es1) 
                = parseHypergraph text1 :: EdgeList (Int, Int8) Int Double
        let (EdgeList vs2 es2) 
                = parseHypergraph text2 :: EdgeList (Int, Int8) Int Double
        B.encodeFile 
          "/home/student/lindal/Downloads/SHK/Berkeley/berkeley.bhg"
            (EdgeList vs1 es2)
        -- putStr $ show vs 
        -- unlines $ map show es
