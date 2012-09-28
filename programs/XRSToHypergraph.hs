{-# LANGUAGE RecordWildCards #-}
module Main where

import Codec.Compression.GZip ( compress, decompress )
import Control.DeepSeq ( NFData )
import Control.Monad.ST
import qualified Data.Array as A
import qualified Data.Array.IArray as IA
import qualified Data.Array.MArray as MA
import qualified Data.Array.ST as STA
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import Data.List ( foldl' )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V
import System.Environment ( getArgs )

import Vanda.Algorithms.IntEarley
import qualified Vanda.Algorithms.Earley.WSA as WSA
import Vanda.Functions ( toWSAmap )
import Vanda.Grammar.XRS.Binary ()
import Vanda.Grammar.XRS.IRTG
import Vanda.Grammar.XRS.Text
import Vanda.Hypergraph.IntHypergraph
import qualified Vanda.Hypergraph.Tree as T
import Vanda.Token

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

compute :: [Hyperedge l i] -> [Int]
compute _es =
  let
    a es = do
      ar <- MA.newArray (0, 99) 0 :: ST s (STA.STUArray s Int Int) 
      sequence_ $ map (ac ar . arity) es
      return ar
    ac ar i = do
      n :: Int <- MA.readArray ar i
      let n' = n + 1
      n' `seq` MA.writeArray ar i n'
  in IA.elems $ STA.runSTUArray $ a _es


derivToTree :: (l -> T.Tree NTT) -> Derivation l i -> T.Tree NTT
derivToTree comp t = subst (map (derivToTree comp) (T.subForest t))
                           (comp (label (T.rootLabel t)))

subst :: [T.Tree NTT] -> T.Tree NTT -> T.Tree NTT
subst ts (T.Nullary (NT i)) = ts !! i
subst ts t = case T.rootLabel t of
               T _ -> T.mapChildren (subst ts) t

nttToString :: TokenArray -> TokenArray -> T.Tree NTT -> T.Tree String
nttToString ta na (T.Node (T i) ts)
  = T.Node (if i < 0 then "@" else getString ta i) (map (nttToString ta na) ts)
nttToString ta na (T.Node (NT i) ts)
  = T.Node (getString na i) (map (nttToString ta na) ts) 

getTerminals :: Ord t => WSA.WSA Int t Double -> S.Set t
getTerminals = S.fromList . map WSA.transTerminal . WSA.transitions

prune :: (l -> [NTT]) -> S.Set Int -> Hypergraph l i -> Hypergraph l i
prune comp s hg
  = Hypergraph (nodes hg) (filter p $ edges hg)
  where
    p e = arity e <= 2 && foldl' p' True (comp (label e))
    p' b (NT _) = b
    p' b (T i) = b && i `S.member` s

instance NFData StrictIntPair

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-z", zhgFile, "-s", statFile] -> do
      irtg@IRTG{ .. } :: IRTG Int
        <- fmap (B.decode . decompress) $ B.readFile (zhgFile ++ ".bhg.gz")
      ws :: V.Vector Double
        <- fmap (V.fromList . B.decode . decompress)
           $ B.readFile (zhgFile ++ ".weights.gz")
      print (nodes rtg)
      print (length (edges rtg))
      print (V.length h1)
      print (V.length h2)
      print (V.length ws)
      let stat = compute (edges rtg)
      TIO.writeFile statFile $ T.unlines $ map (T.pack . show) $ stat
    ["-z", zhgFile, "-s2", statFile] -> do
      irtg@IRTG{ .. } :: IRTG Int
        <- fmap (B.decode . decompress) $ B.readFile (zhgFile ++ ".bhg.gz")
      TIO.writeFile statFile $ T.unlines $ concat
        $ [ map (T.pack . show) (edges rtg)
          , map (T.pack . show) (V.toList h1)
          , map (T.pack . show) (V.toList h2)
          ]
    ["-e", eMapFile, "-f", fMapFile, "-z", zhgFile] -> do
      irtg@IRTG{ .. } :: IRTG Int
        <- fmap (B.decode . decompress) $ B.readFile (zhgFile ++ ".bhg.gz")
      ws :: V.Vector Double
        <- fmap (V.fromList . B.decode . decompress)
           $ B.readFile (zhgFile ++ ".weights.gz")
      em :: TokenArray <- fmap fromText $ TIO.readFile eMapFile
      fm :: TokenMap <- fmap fromText $ TIO.readFile fMapFile
      let wsa = toWSAmap fm -- "Guten Tag meine Damen und Herren ."
                -- "-LRB- Das Parlament erhebt sich zu einer Schweigeminute . -RRB-"
                -- "Zu Montag und Dienstag liegen keine Änderungen vor ."
                -- "Es besteht sogar die Gefahr eines Militärputsches ."
                -- "Frau Präsidentin , zur Geschäftsordnung ."
                -- "Ich bitte Sie , sich zu einer Schweigeminute zu erheben ."
                "Meine Frage betrifft eine Angelegenheit , die am Donnerstag zur Sprache kommen wird und auf die ich dann erneut verweisen werde ."
          comp = ((h2 V.!) . _snd)
          rrtg = dropNonproducing $ prune comp (getTerminals wsa) rtg
          (mm, ip, _) = earley rrtg comp wsa fst 7
          feat _ i xs = (if i < 0 then 1 else ws V.! i) * product xs
          ba = knuth ip feat
      print $ map (nttToString em em . derivToTree ((h1 V.!) . _fst) . deriv)
            $ ba A.! (mm M.! (0, 7, fst . head . WSA.finalWeights $ wsa))
    ["-e", eMapFile, "-f", fMapFile, "-g", grammarFile, "-z", zhgFile] -> do
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
                 B.writeFile (zhgFile ++ ".bhg.gz") $ compress $ B.encode irtg 
                 B.writeFile (zhgFile ++ ".weights.gz") $ compress
                                                        $ B.encode ws
                 TIO.writeFile (eMapFile ++ ".new") (toText em)
                 TIO.writeFile (fMapFile ++ ".new") (toText fm)
                 TIO.writeFile (zhgFile ++ ".nodes") (toText nm)
    _ -> error $ "Usage: XRSToHypergraph -e eMapFile -f fMapFile "
                 ++ "-g grammarFile -z zhgFile"
