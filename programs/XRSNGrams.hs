{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.WTA as WTA
import qualified Vanda.Algorithms.Earley.WSA as WSA
import qualified Vanda.Algorithms.IntersectWithNGram as IS
import qualified Vanda.Algorithms.IntersectWithNGramUtil as ISU
import qualified Vanda.Grammar.LM as LM
import qualified Vanda.Grammar.NGrams.Functions as LM
import qualified Vanda.Grammar.XRS.Functions as IF
import qualified Vanda.Grammar.XRS.IRTG as I
import qualified Vanda.Hypergraph.IntHypergraph as HI
import qualified Vanda.Token as TK

import Codec.Compression.GZip ( compress, decompress )
import Vanda.Algorithms.IntEarley
import System.Environment ( getArgs )

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
                = ISU.intersect IS.intersectUnsmoothed lm
                . ISU.relabel (LM.indexOf lm . T.fromStrict . TK.getString fa)
                $ xrs
      let xrs'  = ISU.relabel (TK.getToken fm . T.toStrict . LM.getText lm) xrs1
      let states'
                = V.map (ISU.mapState id (TK.getToken fm . T.toStrict . LM.getText lm)) states
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
                                              . map (TS.pack . show)
                                              . V.toList
                                              . V.map (ISU.mapState (TK.getString na) (TK.getString fa))
                                              $ states'
    ["--BHPS", "-f", fMapFile, "-z", zhgFile, "-l", lmFile] -> do
      irtg1 <- IF.loadIRTG (zhgFile ++ ".bhg.gz")
      ws    <- IF.loadWeights (zhgFile ++ ".weights.gz")
      na    <- IF.loadTokenArray (zhgFile ++ ".nodes")
      nm    <- IF.loadTokenMap (zhgFile ++ ".nodes")
      fa    <- IF.loadTokenArray fMapFile
      fm    <- IF.loadTokenMap fMapFile
      lm    <- LM.loadNGrams lmFile
      let xrs   = I.XRS irtg1 (VU.generate (V.length ws) (ws V.!))
      let (xrs1, states)
                = ISU.intersect IS.intersectBHPS lm
                . ISU.relabel (LM.indexOf lm . T.fromStrict . TK.getString fa)
                $ xrs
      let xrs'  = ISU.relabel (TK.getToken fm . T.toStrict . LM.getText lm) xrs1
      let states'
                = V.map (ISU.mapState id (TK.getToken fm . T.toStrict . LM.getText lm)) states
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
                                              . map (TS.pack . show)
                                              . V.toList
                                              . V.map (ISU.mapState (TK.getString na) (TK.getString fa))
                                              $ states'
    [wtaStyle, "-f", fMapFile, "-z", zhgFile, "-l", lmFile] -> do
      irtg1 <- IF.loadIRTG (zhgFile ++ ".bhg.gz")
      ws    <- IF.loadWeights (zhgFile ++ ".weights.gz")
      na    <- IF.loadTokenArray (zhgFile ++ ".nodes")
      nm    <- IF.loadTokenMap (zhgFile ++ ".nodes")
      fa    <- IF.loadTokenArray fMapFile
      fm    <- IF.loadTokenMap fMapFile
      lm    <- LM.loadNGrams lmFile
      let xrs   = I.XRS irtg1 (VU.generate (V.length ws) (ws V.!))
      let (xrs1, states)
                = case wtaStyle of
                       "--unsmoothed" -> ISU.intersect IS.intersectUnsmoothed lm
                       "--smoothed"   -> ISU.intersect IS.intersectSmoothed lm
                . ISU.relabel (LM.indexOf lm . T.fromStrict . TK.getString fa)
                $ xrs
      let xrs'  = ISU.relabel (TK.getToken fm . T.toStrict . LM.getText lm) xrs1
      let states'
                = V.map (ISU.mapState id (TK.getToken fm . T.toStrict . LM.getText lm)) states
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
                                              . map (TS.pack . show)
                                              . V.toList
                                              . V.map (ISU.mapState (TK.getString na) (TK.getString fa))
                                              $ states'
    ["--translate", wtaStyle, "-e", eMapFile, "-f", fMapFile, "-z", zhgFile, "-l", lmFile] -> do
      I.IRTG{ .. } <- IF.loadIRTG (zhgFile ++ ".bhg.gz")
      ws  <- IF.loadWeights (zhgFile ++ ".weights.gz")
      em  <- IF.loadTokenArray eMapFile
      fm  <- IF.loadTokenMap fMapFile
      fa  <- IF.loadTokenArray fMapFile
      lm  <- LM.loadNGrams lmFile
      inp <- TIO.getContents
      let translate input = output
            where
            wsa = IF.toWSAmap fm input
            comp = V.toList . (h2 V.!) . I._snd
            -- rrtg = dropNonproducing $ prune comp (getTerminals wsa) rtg
            (mm, ip, _) = earley (toBackwardStar rtg comp) comp wsa fst initial
            initial' = mm M.! (0, initial, fst . head . WSA.finalWeights $ wsa)
            xrs = I.XRS (I.IRTG ip initial' h1 h2) (VU.generate (V.length ws) (ws V.!))
            (xrs', sts)
              = case wtaStyle of
                  "--unsmoothed" -> ISU.intersect IS.intersectUnsmoothed lm
                  "--smoothed"   -> ISU.intersect IS.intersectSmoothed lm
              . ISU.relabel (LM.indexOf lm . T.fromStrict . TK.getString fa)
              $ xrs
            feat _ i xs = (if i < 0 then 1 else (I.weights xrs') VU.! i) * product xs
            ba = flip HI.knuth feat . I.rtg $ I.irtg xrs'
            best = ba A.! 0 -- target symbol is always 0
            otree = map (IF.getTree' ((h1 V.!) . I._fst) . HI.deriv) best
            output = IF.toString em otree
      TIO.putStr . T.unlines . map translate $ T.lines inp
    _ -> do
           putStr "usage: XRSNGrams [--translate] [WTA_STYLE] [-e EMAP] -f FMAP -z ZHG -l LM\n"
