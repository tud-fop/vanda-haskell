{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Codec.Compression.GZip ( compress, decompress )
import System.Environment ( getArgs )
import Vanda.Grammar.XRS.IRTG
import Vanda.Hypergraph.IntHypergraph
import Data.Interner
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import qualified Vanda.Grammar.LM as LM
import qualified Vanda.Grammar.NGrams.VandaNGrams as VN
import qualified Vanda.Grammar.NGrams.Functions as VNF
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import qualified Data.Text.Lazy.IO as TIO
import qualified Vanda.Token as TK
import Vanda.Hypergraph.Tree
import Vanda.Util.Memorysavers
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Map as M
import Control.Monad.State.Lazy
import Vanda.Grammar.XRS.Binary ()
import Data.NTT

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-z", zhgFile, "-e", eMapFile, "-l", lmFile] -> do
      irtg :: IRTG Int
         <- fmap (B.decode . decompress) $ B.readFile (zhgFile ++ ".bhg.gz")
      ea :: TK.TokenArray
         <- fmap TK.fromText $ TIO.readFile eMapFile
      lm <- VNF.loadNGrams lmFile
      let d1    = snd
                . runState ( intifyS1 
                           . map (T.pack . TS.unpack)
                           . A.elems
                           $ TK.unTokenArray ea
                           )
                $ VN.dict lm
          ea'   = V.fromList
                . map snd
                . M.toAscList
                . M.fromList
                . map (\(x, y) -> (y, x)) 
                $ M.toList d1
          rel   = (M.!) d1 . T.pack . TS.unpack . TK.getString ea
          r' (T i)
                =  T $ rel i
          r' (NT i)
                = NT i
          h1'   = V.map (fmap r') $ h1 irtg
          irtg' = IRTG (rtg irtg) (initial irtg) (h1') (h2 irtg)
      B.writeFile (zhgFile ++ ".ada.bhg.gz") $ compress $ B.encode irtg'
      TIO.writeFile (eMapFile ++ ".ada") $ toText ea'

toText :: V.Vector T.Text -> T.Text
toText v
  = T.unlines $ T.pack (show $ V.length v) : (V.toList v)