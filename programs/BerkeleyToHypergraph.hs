module Main where

import Codec.Compression.GZip ( compress )
import Control.Arrow ( (***) )
import Control.DeepSeq ( ($!!) )
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.IO as TIO
import System.Environment ( getArgs )

import Vanda.Grammar.Berkeley.Text
import Vanda.Hypergraph
import Vanda.Hypergraph.Binary ()
import Vanda.Token

{-- FIXME should not use show for this --
instance (Show v, Show l, Show i) => Show (Hyperedge v l i) where
  show e
    = show (to e)
      ++ " -> "
      ++ show (label e)
      ++ " "
      ++ unwords (map show $ from e)
      -- ++ " # "
      -- ++ show (i e)
--}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ "-m", mapFile, "-l", lexiconFile, "-g", grammarFile
      , "-z", zhgFile ] -> do
      mf <- TIO.readFile mapFile
      lf <- TIO.readFile lexiconFile
      gf <- TIO.readFile grammarFile
      let ((ml, mv), (es :: [Hyperedge Int Int Int], wgts))
            = id *** unzip
            $!! parseBerkeleyMap
                updateToken
                (fromText mf)
                updateToken
                (emptyTS :: TokenMap)
                lf
                gf
      let hyp = EdgeList (nodesL es) es -- (getBounds mv) es
      B.writeFile zhgFile $ compress $ B.encode hyp 
      B.writeFile (zhgFile ++ ".weights.gz") $ compress $ B.encode wgts
      TIO.writeFile (mapFile ++ ".new") (toText ml)
      TIO.writeFile (zhgFile ++ ".nodes") (toText mv)
    _ -> error $ "Usage: BerkeleyToHypergraph -m mapFile -l lexiconFile "
                 ++ "-g grammarFile -z zhgFile"
