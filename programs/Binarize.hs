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
import Control.DeepSeq ( NFData )
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import System.Environment ( getArgs )

import Vanda.Grammar.XRS.Binarize ( binarizeXRS )
import Vanda.Grammar.XRS.Binary ()
import Vanda.Grammar.XRS.IRTG

{-# OPTIONS_GHC -fno-warn-orphans #-}
instance NFData StrictIntPair

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-z", zhgFile] -> do
      irtg@IRTG{ .. } :: IRTG Int
        <- fmap (B.decode . decompress) $ B.readFile (zhgFile ++ ".bhg.gz")
      let birtg = binarizeXRS irtg
      B.writeFile (zhgFile ++ ".bin.bhg.gz") $ compress $ B.encode birtg
