{-# LANGUAGE RecordWildCards #-}
module Main where

import Codec.Compression.GZip ( compress )
import Control.DeepSeq ( NFData )
import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V
import System.Environment ( getArgs )

import Vanda.Grammar.XRS.Binary ()
import Vanda.Grammar.XRS.IRTG
import Vanda.Grammar.XRS.Text
import Vanda.Hypergraph.IntHypergraph
import Vanda.Token

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

instance NFData StrictIntPair

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ "-e", eMapFile, "-f", fMapFile, "-g", grammarFile
      , "-z", zhgFile ] -> do
      emf <- TIO.readFile eMapFile
      fmf <- TIO.readFile fMapFile
      gf <- TIO.readFile grammarFile
      case parseXRSMap
             updateToken
             (fromText emf)
             updateToken
             (fromText fmf)
             updateToken
             (emptyTS :: TokenMap)
             gf
        of ((em, fm, nm, (tm, tmc), (sm, smc)), ews) ->
            let ws = S.toList $ S.fromList $ map snd ews
                wmap = M.fromList $ zip ws [(0 :: Int) ..]
                es = [ mapHEi (const (wmap M.! w)) e
                     | (e, w) <- ews
                     ]
                rtg = mkHypergraph es
                h1 = V.fromList $ A.elems $ A.array (0, tmc - 1)
                   $ map swap $ M.toList tm
                h2 = V.fromList $ A.elems $ A.array (0, smc - 1)
                   $ map swap $ M.toList sm
                irtg = IRTG { .. } 
            in do
                 B.writeFile zhgFile $ compress $ B.encode irtg 
                 B.writeFile (zhgFile ++ ".weights.gz") $ compress
                                                        $ B.encode ws
                 TIO.writeFile (eMapFile ++ ".new") (toText em)
                 TIO.writeFile (fMapFile ++ ".new") (toText fm)
                 TIO.writeFile (zhgFile ++ ".nodes") (toText nm)
    _ -> error $ "Usage: XRSToHypergraph -e eMapFile -f fMapFile "
                 ++ "-g grammarFile -z zhgFile"
