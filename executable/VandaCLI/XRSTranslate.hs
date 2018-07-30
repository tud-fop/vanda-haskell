{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module VandaCLI.XRSTranslate where

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

import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Explicit.Misc

data Args = Help { help :: String }
          | Single { e :: String, f :: String, z :: String}
          | Complicated { e :: String, f :: String, z :: String}
          | Default { e :: String, f :: String, z :: String} deriving (Show)

cmdArgs :: Mode Args
cmdArgs = (modeEmpty $ Default undefined undefined undefined)
          { modeNames = ["xrs-translate", "XRSTranslate"]
          , modeHelp = "Generates a tree corpus from a sentence corpus and a GHKM hypergraph"
          , modeGroupFlags = (toGroup flags){ groupNamed = [("mode flag", mfs)]}
          }
  where
    flags = [ flagReq ["e"] (\ s x -> Right x{ e = s }) "FILE" "eMapFile"
            , flagReq ["f"] (\ s x -> Right x{ f = s }) "FILE" "fMapFile"
            , flagReq ["z"] (\ s x -> Right x{ z = s }) "FILE" "zhgFile"
            ]
    mfs = [ flagNone ["help"] (\ _ -> Help $ defaultHelp cmdArgs) "prints help"
          , flagNone ["single"] (transformMode Single) "single mode"
          , flagNone ["complicated"] (transformMode Complicated) "complicated mode"
          ]
    transformMode _ (Help s) = Help s
    transformMode m x = m (e x) (f x) (z x)


mainArgs :: Args -> IO ()
mainArgs (Help cs) = putStr cs
mainArgs (Default e f z) =
  do x@(irtg, _) <- fmap prepareInputProduct $ loadIRTG (z ++ ".bhg.gz")
     ws <- loadWeights (z ++ ".weights.gz")
     em <- loadTokenArray e
     fm <- loadTokenMap f
     let piE = toString em . getOutputTree irtg
         toWSA = toWSAmap fm
         h func = piE (bestDeriv (toWSA func `doInputProduct` x) ws)
     TIO.interact (T.unlines . map h . T.lines)
mainArgs (Single e f z) =
  do IRTG{ .. } <- loadIRTG (z ++ ".bhg.gz")
     ws <- loadWeights (z ++ ".weights.gz")
     em <- loadTokenArray e
     fm <- loadTokenMap f
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
mainArgs (Complicated e f z) =
  do IRTG{ .. } <- loadIRTG (z ++ ".bhg.gz")
     ws <- loadWeights (z ++ ".weights.gz")
     em <- loadTokenArray e
     fm <- loadTokenMap f
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

