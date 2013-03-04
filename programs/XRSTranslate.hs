{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Vanda.Algorithms.Earley.WSA as WSA
import Vanda.Algorithms.IntEarley
import Vanda.Grammar.XRS.IRTG
import Vanda.Hypergraph.IntHypergraph

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Vanda.Grammar.XRS.Functions
import System.Environment ( getArgs )

import Debug.Trace ( traceShow )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-e", eMapFile, "-f", fMapFile, "-z", zhgFile, "--single"] -> do
      IRTG{ .. } <- loadIRTG (zhgFile ++ ".bhg.gz")
      ws <- loadWeights (zhgFile ++ ".weights.gz")
      em <- loadTokenArray eMapFile
      fm <- loadTokenMap fMapFile
      input <- TIO.getContents
      let wsa = toWSAmap fm input
          comp = V.toList . (h2 V.!) . _snd
          rrtg = dropNonproducing $ prune comp (getTerminals wsa) rtg
          (mm, ip, _) = earley (toBackwardStar rrtg comp) comp wsa fst initial
          initial' = mm M.! (0, initial, fst . head . WSA.finalWeights $ wsa)
          feat _ i xs = (if i < 0 then 1 else ws V.! i) * product xs
          ba = knuth ip feat
          best = ba A.! initial'
          otree = map (getTree' ((h1 V.!) . _fst) . deriv) best
          translate = toString em otree
      TIO.putStr translate
    ["-e", eMapFile, "-f", fMapFile, "-z", zhgFile, "--complicated"] -> do
      IRTG{ .. } <- loadIRTG (zhgFile ++ ".bhg.gz")
      ws <- loadWeights (zhgFile ++ ".weights.gz")
      em <- loadTokenArray eMapFile
      fm <- loadTokenMap fMapFile
      inp <- TIO.getContents
      let translate input = output
            where
            wsa = toWSAmap fm input
            comp = V.toList . (h2 V.!) . _snd
            -- rrtg = dropNonproducing $ prune comp (getTerminals wsa) rtg
            (mm, ip, _) = earley (toBackwardStar rtg comp) comp wsa fst initial
            initial' = mm M.! (0, initial, fst . head . WSA.finalWeights $ wsa)
            feat _ i xs = (if i < 0 then 1 else ws V.! i) * product xs
            ba = knuth ip feat
            best = ba A.! initial'
            otree = map (getTree' ((h1 V.!) . _fst) . deriv) best
            output = toString em otree
      TIO.putStr (T.unlines (map translate (T.lines inp)))
    ["-e", eMapFile, "-f", fMapFile, "-z", zhgFile] -> do
      x@(irtg, _) <- fmap prepareInputProduct $ loadIRTG (zhgFile ++ ".bhg.gz")
      ws <- loadWeights (zhgFile ++ ".weights.gz")
      em <- loadTokenArray eMapFile
      fm <- loadTokenMap fMapFile
      let piE = toString em . getOutputTree irtg
          toWSA = toWSAmap fm
          h f = piE (bestDeriv (toWSA f `doInputProduct` x) ws)
      TIO.interact (T.unlines . map h . T.lines)

