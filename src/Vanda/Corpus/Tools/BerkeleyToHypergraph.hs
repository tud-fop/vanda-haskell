module Main where

import Codec.Compression.GZip ( compress, decompress )
import Control.Arrow ( (***), (&&&) )
import Control.DeepSeq ( ($!!) )
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import Data.Int ( Int8, Int16, Int32 )
import Data.List ( intersperse )
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V
import Data.Word ( Word16 )
import System.Environment ( getArgs )

import Text
import Vanda.Hypergraph
import Vanda.Hypergraph.Binary
import Vanda.Token

instance (Show v, Show l, Show i) => Show (Hyperedge v l i) where
  show e
    = show (to e)
      ++ " -> "
      ++ show (label e)
      ++ " "
      ++ unwords (map show $ from e)
      -- ++ " # "
      -- ++ show (i e)

main = do
  args <- getArgs
  case args of
    [ "-m", mapFile, "-l", lexiconFile, "-g", grammarFile
      , "-z", zhgFile ] -> do
      mf <- TIO.readFile mapFile
      lf <- TIO.readFile lexiconFile
      gf <- TIO.readFile grammarFile
      {-let x :: (((),()), ([Hyperedge Int16 Int16 Int32], [Double]))
      -- ((ml, mv), (es :: [Hyperedge Int16 Int16 Int32], wgts))
            = id *** unzip
            $ parseBerkeleyMap
                (curry $ id *** const 0)
                ()
                (curry $ id *** const 0)
                ()
                lf
                gf-}
      {-let (_, (es :: [Hyperedge Int16 Int16 Int32], _))
            = id *** unzip
            $ parseBerkeleyMap
                updateToken
                (fromText mf)
                updateToken
                (emptyTS :: TokenMap)
                lf1
                gf1
      let (_, (_, wgts))
            = id *** unzip
            $ parseBerkeleyMap
                updateToken
                (fromText mf)
                updateToken
                (emptyTS :: TokenMap)
                lf2
                gf2 -}
      let ((ml, mv), (es :: [Hyperedge Int32 Int32 Int32], wgts))
            = id *** unzip
            $!! parseBerkeleyMap
                updateToken
                (fromText mf)
                updateToken
                (emptyTS :: TokenMap)
                lf
                gf
      let hyp = EdgeList (getBounds mv) es
      {-TIO.writeFile (grammarFile ++ ".hg")
        $ T.unlines
        $ map (T.pack . (\(a,b) -> a ++ " # " ++ b) . (show *** show))
        $ eswgts-}
      B.writeFile zhgFile $ compress $ B.encode hyp 
      B.encodeFile (zhgFile ++ ".weights") wgts
      TIO.writeFile (mapFile ++ ".new") (toText ml)
      TIO.writeFile (zhgFile ++ ".nodes") (toText mv)
      -- TIO.writeFile (grammarFile ++ ".wgt")
      --   $ T.unlines $ map (T.pack . show) wgts
      {- TIO.writeFile (zhgFile ++ ".weights")
        ( T.unlines
        . uncurry (:)
        . ( T.pack . show . length
          &&& map (T.pack . show)
          )
        $ wgts
        ) -}
      -- putStr $ show m'

