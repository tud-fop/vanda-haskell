{-# LANGUAGE
      RecordWildCards
    , ScopedTypeVariables
    , TypeSynonymInstances
    , FlexibleInstances
    , MultiParamTypeClasses
    , TypeFamilies
    , FlexibleContexts
    , BangPatterns
    #-}
module Main where

import Codec.Compression.GZip ( compress, decompress )
import Control.DeepSeq ( NFData )
import Control.Monad.ST
import qualified Data.Array as A
import qualified Data.Array.IArray as IA
import qualified Data.Array.MArray as MA
import qualified Data.Array.ST as STA
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import Data.List ( foldl', intersperse, elemIndex )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V
import System.Environment ( getArgs )

import Vanda.Algorithms.IntEarley
import qualified Vanda.Algorithms.Earley.WSA as WSA
import Vanda.Grammar.XRS.Functions
import Vanda.Grammar.XRS.Binary ()
import Vanda.Grammar.XRS.IRTG
import Vanda.Grammar.XRS.Text
import Vanda.Hypergraph.IntHypergraph
import qualified Vanda.Hypergraph.Tree as T
import Vanda.Token

import Debug.Trace ( traceShow )

compute :: [Hyperedge l i] -> [Int]
compute _es =
  let
    a es = do
      ar <- MA.newArray (0, 99) 0 :: ST s (STA.STUArray s Int Int) 
      sequence_ $ map (ac ar . arity) es
      return ar
    ac ar i = do
      n :: Int <- MA.readArray ar i
      let n' = n + 1
      n' `seq` MA.writeArray ar i n'
  in IA.elems $ STA.runSTUArray $ a _es


--nttToString :: TokenArray -> TokenArray -> NTT -> String
--nttToString ta _ (T i) = if i < 0 then "@" else getString ta i
--nttToString _ na (NT i) = getString na i 


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-z", zhgFile, "-s", statFile] -> do
      IRTG{ .. } :: IRTG Int
        <- fmap (B.decode . decompress) $ B.readFile (zhgFile ++ ".bhg.gz")
      ws :: V.Vector Double
        <- fmap (V.fromList . B.decode . decompress)
           $ B.readFile (zhgFile ++ ".weights.gz")
      print (nodes rtg)
      print (length (edges rtg))
      print (V.length h1)
      print (V.length h2)
      print (V.length ws)
      let stat = compute (edges rtg)
      TIO.writeFile statFile $ T.unlines $ map (T.pack . show) $ stat
    ["-z", zhgFile, "-s2", statFile] -> do
      IRTG{ .. } :: IRTG Int
        <- fmap (B.decode . decompress) $ B.readFile (zhgFile ++ ".bhg.gz")
      TIO.writeFile statFile $ T.unlines $ concat
        $ [ map (T.pack . show) (edges rtg)
          , map (T.pack . show) (V.toList h1)
          , map (T.pack . show) (V.toList h2)
          ]
    ["-e", eMapFile, "-f", fMapFile, "-z", zhgFile, "--convert"] -> do
      IRTG{ .. } :: IRTG Int
        <- fmap (B.decode . decompress) $ B.readFile (zhgFile ++ ".bhg.gz")
      ws :: V.Vector Double
        <- fmap (V.fromList . B.decode . decompress)
           $ B.readFile (zhgFile ++ ".weights.gz")
      em :: TokenArray <- fmap fromText $ TIO.readFile eMapFile
      fm :: TokenArray <- fmap fromText $ TIO.readFile fMapFile
      let rules
            = ("[S] ||| [" ++ show initial ++ ",1] ||| [" ++ show initial ++ ",1] ||| 1.0")
            : [ "[" ++ show (to e) ++ "] ||| "
                ++ concat (intersperse " "
                   [ case x of
                       NT i -> let Just j = elemIndex i l
                               in "[" ++ show (from e !! j) ++ "," ++ show (j + 1) ++ "]"
                       T i -> TS.unpack $ getString fm i
                   | x <- lhs
                   ])
                ++ " ||| "
                ++ concat (intersperse " "
                   [ case x of
                       NT i -> let j = l !! i
                               in "[" ++ show (from e !! j) ++ "," ++ show (j + 1) ++ "]"
                       T i -> if i < 0 then "" else TS.unpack $ getString em i
                   | x <- rhs
                   ])
                ++ " ||| "
                ++ show w
              | e <- edges rtg
              , arity e <= 2
              , let lhs = h2 V.! _snd (label e)
              , lhs /= [NT 0]
              , let rhs = T.front $ h1 V.! _fst (label e)
              , let l = [ i | NT i <- lhs ]
              , let idente = ident e
              , let w = if idente < 0 then 1.0 else ws V.! idente
              , w `seq` True
              ]
      TIO.writeFile (zhgFile ++ ".joshua") $ T.unlines $ map T.pack rules
    ["-e", eMapFile, "-f", fMapFile, "-z", zhgFile] -> do
      emf <- TIO.readFile eMapFile
      fmf <- TIO.readFile fMapFile
      gf <- TIO.getContents
      -- TODO mit ghc deutlich schneller als mit runghc, untersuchen!
      case mkIRTG
             (fromText emf, fromText fmf, emptyTS :: TokenMap)
             (map parseXRSRule $ T.lines gf) of
        (irtg, ws, em, fm, nm) -> do
          B.writeFile (zhgFile ++ ".bhg.gz") $ compress $ B.encode irtg 
          B.writeFile (zhgFile ++ ".weights.gz") $ compress $ B.encode ws
          TIO.writeFile (eMapFile ++ ".new") (toText em)
          TIO.writeFile (fMapFile ++ ".new") (toText fm)
          TIO.writeFile (zhgFile ++ ".nodes") (toText nm)
    _ -> error $ "Usage: XRSToHypergraph -e eMapFile -f fMapFile "
                 ++ "-g grammarFile -z zhgFile"
