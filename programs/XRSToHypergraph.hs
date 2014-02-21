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
import Data.Maybe ( catMaybes )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V
import System.Directory ( doesFileExist, renameFile )
import System.Environment ( getArgs )

import Vanda.Algorithms.IntEarley
import qualified Vanda.Algorithms.Earley.WSA as WSA
-- import Vanda.Grammar.XRS.Functions
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


loadTokenMap :: FilePath -> IO TokenMap
loadTokenMap fp = do
  b <- doesFileExist fp
  if b then fmap fromText $ TIO.readFile fp
       else return $ emptyTS


saveTokenMap :: FilePath -> TokenMap -> IO ()
saveTokenMap fp tm = let fpnew = fp ++ ".new" in do
  TIO.writeFile fpnew (toText tm)
  renameFile fpnew fp

--nttToString :: TokenArray -> TokenArray -> NTT -> String
--nttToString ta _ (T i) = if i < 0 then "@" else getString ta i
--nttToString _ na (NT i) = getString na i 


type Pretty
  = TokenArray
  -> TokenArray
  -> TokenArray
  -> IRTG Int
  -> V.Vector Double
  -> Hyperedge StrictIntPair Int
  -> T.Text

type PrettyPredicate = IRTG Int -> Hyperedge StrictIntPair Int -> Bool

type PrettyExtra = IRTG Int -> TokenArray -> [T.Text] -> [T.Text]


predJoshua IRTG{ .. } e
  = arity e <= 2 && V.toList (h2 V.! _snd (label e)) /= [NT 0]

peJoshua IRTG{ .. } nm
  = let si = show initial
    in (T.pack ("[S] ||| [" ++ si ++ ",1] ||| [" ++ si ++ ",1] ||| 1.0") :)


bin2text
  :: FilePath -> FilePath -> FilePath
  -> Pretty -> PrettyExtra -> PrettyPredicate -> IO ()
bin2text eMapFile fMapFile zhgFile pretty pe pp = do
  irtg :: IRTG Int
    <- fmap (B.decode . decompress) $ B.readFile (zhgFile ++ ".bhg.gz")
  ws :: V.Vector Double
    <- fmap (V.fromList . B.decode . decompress)
       $ B.readFile (zhgFile ++ ".weights.gz")
  em :: TokenArray <- fmap fromText $ TIO.readFile eMapFile
  fm :: TokenArray <- fmap fromText $ TIO.readFile fMapFile
  nm :: TokenArray <- fmap fromText $ TIO.readFile (zhgFile ++ ".nodes")
  TIO.putStr
    $ T.unlines
    $ pe irtg nm
    $ map (pretty em fm nm irtg ws)
    $ filter (pp irtg) $ edges $ rtg irtg


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
    ["b2j", "-e", eMapFile, "-f", fMapFile, "-z", zhgFile] ->
      bin2text eMapFile fMapFile zhgFile prettyPrintJoshua peJoshua predJoshua
    ["b2t", "-e", eMapFile, "-f", fMapFile, "-z", zhgFile] ->
      bin2text eMapFile fMapFile zhgFile prettyPrint (\ _ _ -> id) (\ _ _ -> True)
    ["t2b", "-e", eMapFile, "-f", fMapFile, "-z", zhgFile] -> do
      em <- loadTokenMap eMapFile
      fm <- loadTokenMap fMapFile
      gf <- TIO.getContents
      -- TODO mit ghc deutlich schneller als mit runghc, untersuchen!
      case mkIRTG (em, fm, emptyTS) (catMaybes . map parseXRSRule . filter (not . T.null) $ T.lines gf) of
        (irtg, ws, em', fm', nm) -> do
          B.writeFile (zhgFile ++ ".bhg.gz") $ compress $ B.encode irtg 
          B.writeFile (zhgFile ++ ".weights.gz") $ compress $ B.encode ws
          saveTokenMap eMapFile em'
          saveTokenMap fMapFile fm'
          TIO.writeFile (zhgFile ++ ".nodes") (toText nm)
    _ -> putStr $ "Usage:\n\n"
                 ++ "  text to binary: XRSToHypergraph t2b "
                 ++ "-e eMapFile -f fMapFile -z zhgFile < grammarFile\n"
                 ++ "  binary to text: XRSToHypergraph b2t "
                 ++ "-e eMapFile -f fMapFile -z zhgFile > grammarFile\n"
