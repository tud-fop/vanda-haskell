module Main where

import Control.Arrow ( (***) )
import qualified Data.Binary as B
import Data.Int ( Int8 )
import Data.List ( intersperse )
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V
import System.Environment ( getArgs )

import Vanda.Token
import Vanda.Grammar.Berkeley.Text
import Vanda.Hypergraph
import Vanda.Hypergraph.Binary

instance Show s => Show (Hyperedge (s, Int8) (Maybe s) Int) where
  show (Hyperedge t f Nothing i)
    = show t
      ++ " -> "
      ++ show (fst t)
      ++ " "
      ++ unwords (map show $ V.toList f)
  show (Hyperedge t f (Just s) i)
    = show t
      ++ " -> "
      ++ show s

main = do
  args <- getArgs
  case args of
    ["-l", lexiconFile, "-g", grammarFile] -> do
      lf <- TIO.readFile lexiconFile
      gf <- TIO.readFile grammarFile
      {-let (es, wgts)
            = unzip . snd
            $ parseBerkeleyMap (curry $ id *** id) () lf gf-}
      let (x, es) = parseBerkeleyMap (curry $ id *** id) () lf gf
      {-TIO.writeFile (grammarFile ++ ".hg")
        $ T.unlines $ map (T.pack . show) es
      TIO.writeFile (grammarFile ++ ".wgt")
        $ T.unlines $ map (T.pack . show) wgts-}
      putStr $ concatMap show es
      print x
    ["-m", mapFile, "-l", lexiconFile, "-g", grammarFile] -> do
      mf <- TIO.readFile mapFile
      lf <- TIO.readFile lexiconFile
      gf <- TIO.readFile grammarFile
      -- let es = snd $ parseBerkeleyMap (curry $ id *** id) () lf gf
      let (m', eswgts) -- @(es, wgts))
            = id -- *** unzip
            $ parseBerkeleyMap updateToken (fromText mf) lf gf
      -- let es = snd $ parseBerkeleyMap (curry $ id *** id) () lf gf
      --putStr $ concatMap show es
      --B.encodeFile (grammarFile ++ ".bhg") es
      TIO.writeFile (grammarFile ++ ".hg")
        $ T.unlines
        $ map (T.pack . (\(a,b) -> a ++ " # " ++ b) . (show *** show))
        $ eswgts
      -- TIO.writeFile (grammarFile ++ ".wgt")
      --   $ T.unlines $ map (T.pack . show) wgts
      TIO.writeFile (mapFile ++ ".new") (toText m')
      -- putStr $ show m'
    _ -> do
      print "Usage 1: BerkeleyToHypergraph -l <lexicon file> -g <grammar file>"
      print "Converts Berkeley grammar into hypergraph using strings"
      print ""
      print "Usage 2: BerkeleyToHypergraph -m <map file> -l <lexicon file> -g <grammar file>"
      print "Converts Berkeley grammar into hypergraph using integers"

