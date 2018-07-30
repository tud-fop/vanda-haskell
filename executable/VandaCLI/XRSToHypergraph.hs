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
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  VandaCLI.XRSToHypergraph
-- Copyright   :  (c) Technische Universität Dresden 2018
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module VandaCLI.XRSToHypergraph where

import Codec.Compression.GZip ( compress, decompress )
import Control.Monad.ST
import qualified Data.Array.IArray as IA
import qualified Data.Array.MArray as MA
import qualified Data.Array.ST as STA
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import Data.Maybe ( catMaybes )
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V
import System.Directory ( doesFileExist, renameFile )

import Vanda.Algorithms.IntEarley
import Vanda.Grammar.XRS.IRTG
import Vanda.Grammar.XRS.Text
import Vanda.Hypergraph.IntHypergraph
import Vanda.Token

import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Explicit.Misc


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

predJoshua :: IRTG i1 -> Hyperedge StrictIntPair i2 -> Bool
predJoshua IRTG{ .. } e
  = arity e <= 2 && V.toList (h2 V.! _snd (label e)) /= [NT 0]

peJoshua :: IRTG i -> p -> [T.Text] -> [T.Text]
peJoshua IRTG{ .. } _
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


data Args = Help { help :: String }
          | ToBinary { eFile :: String, fFile :: String, zFile :: String}
          | ToText { eFile :: String, fFile :: String, zFile :: String} deriving Show

cmdArgs :: Mode Args
cmdArgs = (modes ""
                (Help $ defaultHelp cmdArgs)
                "Transformations between hypergraph formats"
                [ (modeEmpty $ ToBinary undefined undefined undefined)
                  { modeNames = ["t2b", "text-to-binary"]
                  , modeGroupFlags = toGroup flags
                  }
                , (modeEmpty $ ToText undefined undefined undefined)
                  { modeNames = ["b2t", "binary-to-text"]
                  , modeGroupFlags = toGroup flags
                  }
                ]){ modeNames = ["xrs-to-hypergraph", "XRSToHypergraph"] }
  where
    flags = [ flagReq ["e"] (\ s x -> Right x{ eFile = s }) "FILE" "eMapFile"
            , flagReq ["f"] (\ s x -> Right x{ fFile = s }) "FILE" "fMapFile"
            , flagReq ["z"] (\ s x -> Right x{ zFile = s }) "FILE" "zhgFile"
            ]

mainArgs :: Args -> IO ()
mainArgs (Help cs) = putStr cs
mainArgs (ToText e f z) = bin2text e f z prettyPrint (\ _ _ -> id) (\ _ _ -> True)
mainArgs (ToBinary e f z) = 
  do em <- loadTokenMap e
     fm <- loadTokenMap f
     gf <- TIO.getContents
     case mkIRTG (em, fm, emptyTS) (catMaybes . map parseXRSRule . filter (not . T.null) $ T.lines gf) of
          (irtg, ws, em', fm', nm) -> do
            B.writeFile (z ++ ".bhg.gz") $ compress $ B.encode irtg 
            B.writeFile (z ++ ".weights.gz") $ compress $ B.encode ws
            saveTokenMap e em'
            saveTokenMap f fm'
            TIO.writeFile (z ++ ".nodes") (toText nm)