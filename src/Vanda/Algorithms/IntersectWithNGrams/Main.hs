{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Technische Universität Dresden 2018
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Algorithms.IntersectWithNGrams.Main
( main
, mainArgs
, cmdArgs
, Args()
) where


import           Codec.Compression.GZip ( compress )

import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import           Vanda.Algorithms.IntEarley
import qualified Vanda.Algorithms.Earley.WSA as WSA
import qualified Vanda.Algorithms.IntersectWithNGram as IS
import qualified Vanda.Algorithms.IntersectWithNGramUtil as ISU
import qualified Vanda.Grammar.NGrams.Functions as LM
import qualified Vanda.Grammar.XRS.Functions as IF
import qualified Vanda.Grammar.XRS.IRTG as I
import qualified Vanda.Hypergraph.IntHypergraph as HI
import qualified Vanda.Token as TK

import           System.Console.CmdArgs.Explicit
import           System.Console.CmdArgs.Explicit.Misc


data Args
  = Help String
  | Product
    { strategy :: Strategy
    , transducer :: Transducer
    , model :: FilePath
    }
  | Translate
    { strategy :: Strategy
    , transducer :: Transducer
    , model :: FilePath
    }
  deriving Show

data Strategy
  = BHPS | Backoff | NoBackoff | Pruning Int | NoBackoffPruning Int deriving (Eq, Show)

data Transducer
  = Transducer { zhg :: FilePath, mape :: FilePath, mapf :: FilePath } deriving Show

cmdArgs :: Mode Args
cmdArgs
  = modes "xrsngrams" (Help $ defaultHelp cmdArgs) "algorithms for combining n-gram models and translation models"
  [ (modeEmpty $ Product BHPS (Transducer undefined undefined undefined) undefined)
    { modeNames = ["product"]
    , modeHelp = "computes the output product of an XRS translation model with an n-gram model"
    , modeArgs = ( [ flagArgPZhg{argRequire = True}, flagArgPMapE{argRequire = True}, flagArgPMapF{argRequire = True}, flagArgPModel{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup [ flagNoneBHPS, flagNoneBackoff, flagNoneNoBackoff, flagReqPruning, flagReqNoBackoffPruning ]
    }
  , (modeEmpty $ Translate BHPS (Transducer undefined undefined undefined) undefined)
    { modeNames = ["translate"]
    , modeHelp = "computes the output product of an XRS translation model with an n-gram model and then the input product with a given word"
    , modeArgs = ( [ flagArgTZhg{argRequire = True}, flagArgTMapE{argRequire = True}, flagArgTMapF{argRequire = True}, flagArgTModel{argRequire = True} ], Nothing )
    , modeGroupFlags = toGroup [ flagNoneTBHPS, flagNoneTBackoff, flagNoneTNoBackoff, flagReqTPruning, flagReqTNoBackoffPruning ]
    }
  ]
  where
    flagArgPZhg
      = flagArg (\ a x -> Right x{transducer = (transducer x){zhg = a}}) "ZHG"
    flagArgPMapE
      = flagArg (\ a x -> Right x{transducer = (transducer x){mape = a}}) "MAP.E"
    flagArgPMapF
      = flagArg (\ a x -> Right x{transducer = (transducer x){mapf = a}}) "MAP.F"
    flagArgPModel
      = flagArg (\ a x -> Right x{model = a}) "MODEL"
    flagNoneBHPS
      = flagNone ["bhps"]
                 (\ x -> x{strategy = BHPS})
                 "use BHPS strategy"
    flagNoneBackoff
      = flagNone ["b", "backoff"]
                 (\ x -> x{strategy = Backoff})
                 "use shortened states and backoff"
    flagNoneNoBackoff
      = flagNone ["n", "no-backoff"]
                 (\ x -> x{strategy = BHPS})
                 "use shortened states but no backoff"
    flagReqPruning
      = flagReq ["p", "prune"]
                (\ a x -> Right x{strategy = Pruning (read a)})
                "BEAMWIDTH"
                "use shortened states and create at most BEAMWIDTH new states per old state"
    flagReqNoBackoffPruning
      = flagReq ["no-backoff-prune"]
                (\ a x -> Right x{strategy = NoBackoffPruning (read a)})
                "BEAMWIDTH"
                "use shortened states but no backoff and create at most BEAMWIDTH new states per old state"
    flagArgTZhg
      = flagArg (\ a x -> Right x{transducer = (transducer x){zhg = a}}) "ZHG"
    flagArgTMapE
      = flagArg (\ a x -> Right x{transducer = (transducer x){mape = a}}) "MAP.E"
    flagArgTMapF
      = flagArg (\ a x -> Right x{transducer = (transducer x){mapf = a}}) "MAP.F"
    flagArgTModel
      = flagArg (\ a x -> Right x{model = a}) "MODEL"
    flagNoneTBHPS
      = flagNone ["bhps"]
                 (\ x -> x{strategy = BHPS})
                 "use BHPS strategy"
    flagNoneTBackoff
      = flagNone ["b", "backoff"]
                 (\ x -> x{strategy = Backoff})
                 "use shortened states and backoff"
    flagNoneTNoBackoff
      = flagNone ["n", "no-backoff"]
                 (\ x -> x{strategy = BHPS})
                 "use shortened states but no backoff"
    flagReqTPruning
      = flagReq ["p", "prune"]
                (\ a x -> Right x{strategy = Pruning (read a)})
                "BEAMWIDTH"
                "use shortened states and create at most BEAMWIDTH new states per old state"
    flagReqTNoBackoffPruning
      = flagReq ["no-backoff-prune"]
                (\ a x -> Right x{strategy = NoBackoffPruning (read a)})
                "BEAMWIDTH"
                "use shortened states but no backoff and create at most BEAMWIDTH new states per old state"


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs


mainArgs :: Args -> IO ()

mainArgs (Help cs) = putStr cs

mainArgs (Product s (Transducer z _ _) m)
  = case s of BHPS               -> go $ IS.intersectBHPS rel
              Backoff            -> go $ IS.intersectSmoothed rel
              NoBackoff          -> go $ IS.intersectUnsmoothed rel
              Pruning b          -> go $ IS.intersectPruning rel b
              NoBackoffPruning b -> go $ IS.intersectUnsmoothedPruning rel b
    where
      rel = id
      go function = do
        irtg1 <- IF.loadIRTG (z ++ ".bhg.gz")
        ws    <- IF.loadWeights (z ++ ".weights.gz")
        let xrs = I.XRS irtg1 (VU.generate (V.length ws) (ws V.!))
        lm    <- LM.loadNGrams m
        let (xrs1, states) = ISU.intersect function lm xrs
        B.writeFile (z ++ ".new.bhg.gz") . compress
                                           . B.encode
                                           $ I.irtg xrs1
        B.writeFile (z ++ ".new.weights.gz") . compress
                                               . B.encode
                                               . VU.toList
                                               $ I.weights xrs1
        TIO.writeFile (z ++ ".new.nodes") . TK.toText
                                            . TK.TokenArray
                                            . (\x -> A.listArray (0, length x - 1) x)
                                            . map (TS.pack . show)
                                            $ V.toList states

mainArgs (Translate s (Transducer z me mf) m)
  = case s of BHPS               -> go $ IS.intersectBHPS rel
              Backoff            -> go $ IS.intersectSmoothed rel
              NoBackoff          -> go $ IS.intersectUnsmoothed rel
              Pruning b          -> go $ IS.intersectPruning rel b
              NoBackoffPruning b -> go $ IS.intersectUnsmoothedPruning rel b
    where
     rel = id
     go function = do
       fm    <- IF.loadTokenMap mf
       ea    <- IF.loadTokenArray me
       irtg1 <- IF.loadIRTG (z ++ ".bhg.gz")
       ws    <- IF.loadWeights (z ++ ".weights.gz")
       let xrs = I.XRS irtg1 (VU.generate (V.length ws) (ws V.!))
       lm   <- LM.loadNGrams m
       inp <- TIO.getContents
       let translate input = output where
             wsa = IF.toWSAmap fm input
             comp = V.toList . (I.h2 (I.irtg xrs) V.!) . I._snd
             (mm, ip, _) = earley (toBackwardStar (I.rtg $ I.irtg xrs) comp) comp wsa fst . I.initial $ I.irtg xrs
             initial' = mm M.! (0, I.initial $ I.irtg xrs, fst . head . WSA.finalWeights $ wsa)
             xrs1 = I.XRS (I.IRTG ip initial' (I.h1 $ I.irtg xrs) (I.h2 $ I.irtg xrs)) (VU.generate (VU.length $ I.weights xrs) (I.weights xrs VU.!))
             (xrs', _)
               = ISU.intersect
                   function
                   lm
                   xrs1
             feat _ i xs = (if i < 0 then 1 else I.weights xrs' VU.! i) * product xs
             ba = flip HI.knuth feat . I.rtg $ I.irtg xrs'
             best = ba A.! 0
             otree = map (IF.getTree' ((I.h1 (I.irtg xrs') V.!) . I._fst) . HI.deriv) best
             output = IF.toString ea otree
       TIO.putStr . T.unlines . map translate $ T.lines inp
