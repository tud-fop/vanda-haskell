{-# LANGUAGE
      TupleSections
    , RecordWildCards
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
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import Data.List ( foldl' )
import qualified Data.Text as TS
import qualified Data.Text.Lazy.IO as TIO
import System.Environment ( getArgs )

import Vanda.Hypergraph.IntHypergraph ( nodes )
import Vanda.Grammar.XRS.Binarize ( binarizeXRS )
import Vanda.Grammar.XRS.Binary ()
import Vanda.Grammar.XRS.IRTG
import Vanda.Token
import Vanda.Util

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-z", zhgFile] -> do
      tm :: TokenMap <- fmap fromText $ TIO.readFile (zhgFile ++ ".nodes")
      irtg@IRTG{} :: IRTG Int
        <- fmap (B.decode . decompress) $ B.readFile (zhgFile ++ ".bhg.gz")
      let birtg = binarizeXRS irtg
      let virt0 = snd $ getBounds tm
      let tm' = foldl' (\ tm_ i -> fst $ updateToken tm_ (TS.pack $ "virt" ++ show i)) tm
              $ [ virt0 .. nodes (rtg birtg) - 1 ]
      B.writeFile (zhgFile ++ ".bin.bhg.gz") $ compress $ B.encode birtg
      TIO.writeFile (zhgFile ++ ".bin.nodes") $ toText tm'
    _ -> putStr "Usage: Binarize -z zhgFile\n"
