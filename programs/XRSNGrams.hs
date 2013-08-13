{-# LANGUAGE RecordWildCards #-}
module Main where

import Codec.Compression.GZip ( compress, decompress )
import System.Environment ( getArgs )
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Array as A
import qualified Data.Vector.Unboxed as VU

import qualified Vanda.Grammar.XRS.Functions as IF
import qualified Vanda.Grammar.XRS.IRTG as I
import qualified Vanda.Grammar.NGrams.Functions as LM
import qualified Vanda.Algorithms.IntersectWithNGram as IS
import qualified Vanda.Algorithms.IntersectWithNGramUtil as ISU
import qualified Vanda.Hypergraph.IntHypergraph as HI
import qualified Vanda.Grammar.LM as LM
import qualified Vanda.Token as TK
import qualified Vanda.Grammar.NGrams.WTA as WTA

import Debug.Trace

main
  :: IO ()
main = do
  args <- getArgs
  case args of
    ["-f", fMapFile, "-z", zhgFile, "-l", lmFile] -> do
      irtg1 <- IF.loadIRTG (zhgFile ++ ".bhg.gz")
      ws    <- IF.loadWeights (zhgFile ++ ".weights.gz")
      na    <- IF.loadTokenArray (zhgFile ++ ".nodes")
      nm    <- IF.loadTokenMap (zhgFile ++ ".nodes")
      fa    <- IF.loadTokenArray fMapFile
      fm    <- IF.loadTokenMap fMapFile
      lm    <- LM.loadNGrams lmFile
      let xrs   = I.XRS irtg1 (VU.generate (V.length ws) (ws V.!))
      let (xrs1, states)
                = ISU.intersect IS.intersect lm
                . ISU.relabel (LM.indexOf lm . TK.getString fa)
                $ xrs
      let xrs'  = ISU.relabel (TK.getToken fm . LM.getText lm) xrs1
      let states'
                = V.map (ISU.mapCState id (TK.getToken fm . LM.getText lm)) states
      B.writeFile (zhgFile ++ ".new.bhg.gz") . compress 
                                             . B.encode
                                             . I.irtg
                                             $ xrs' 
      B.writeFile (zhgFile ++ ".new.weights.gz") . compress
                                                 . B.encode
                                                 . VU.toList
                                                 . I.weights
                                                 $ xrs'
      TIO.writeFile (zhgFile ++ ".new.nodes") . TK.toText
                                              . TK.TokenArray
                                              . (\x -> A.listArray (0, length x - 1) x)
                                              . map (T.pack . show)
                                              . V.toList
                                              . V.map (ISU.mapCState (TK.getString na) (TK.getString fa))
                                              $ states'
