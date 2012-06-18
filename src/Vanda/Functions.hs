module Vanda.Functions
  ( SCFG (..)
  , loadGHKM
  , loadSCFG
  , loadWeights
  , loadText
  , loadTokenMap
  , loadSentenceCorpus
  , saveText
  , saveSequence
  , saveWeights
  , toWSA
  , toWSAmap
  , inputProduct
  , inputProduct'
  , outputProduct
  , outputProduct'
  , bestDeriv
  , makeFeature
  , getInputString
  , getOutputString
  , initialWeights
  , prepareExamples
  , prepareExamplesGHKM
  , preparePartition
  , doEM
  , fakeWeights
  , getVector
  ) where 


import Codec.Compression.GZip ( compress, decompress )

import Control.Arrow ( (&&&) )
import Control.DeepSeq ( NFData )

import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Debug.Trace
import qualified Data.Text.Lazy as TIO
import qualified Data.Text.Lazy.IO as TIO

import Vanda.Algorithms.EarleyCFG ( earley )
import qualified Vanda.Algorithms.Earley.WSA as WSA
import Vanda.Features
import Vanda.Hypergraph
import Vanda.Hypergraph.Binary ()
import Vanda.Token

import Vanda.Algorithms.ExpectationMaximization

{-
instance NFData (BackwardStar Token ([Either Int Token], [Either Int Token]) Int) where
  rnf (BackwardStar nodes edges memo) 
    = (rnf nodes `seq` rnf edges) `seq` rnf memo 

instance (Show v, Show i, Show l, Show x) => Show (Candidate v l i x) where 
  show c
    = "Gewicht: " ++ (show $ weight c) ++ "\n Ableitung: "
      ++ (show $ deriv c) ++ "\fdata: "
      ++ (show $ fdata c)
  
instance (Show v, Show i, Show l, Ord v) => Show (EdgeList v l i) where 
  show g 
    = show (S.toList $ nodesEL g) ++ "\n" ++ unlines (map show (edgesEL g))
-}

type SyncPair t = ([Either Int t], [Either Int t])

type GHKM t = (T.Tree (Either Int t), [Either Int t])

data SCFG nt l i
  = SCFG
    { toHypergraph :: EdgeList nt l i
    , initNT :: nt
    }

loadSCFG
  :: String -> IO (SCFG String (SyncPair String) Int)
loadSCFG file
  = fmap
    ( (uncurry SCFG)
    . (id &&& to . head . edges{-S.findMin . nodes-})
    . B.decode
    . decompress
    )
  $ B.readFile (file ++ ".bhg.gz")

loadGHKM
  :: String -> IO (SCFG Token (GHKM Token) Int)
loadGHKM file
  = fmap
    ( (uncurry SCFG)
    . (id &&& to . head . edges{-S.findMin . nodes-})
    . B.decode
    . decompress
    )
  $ B.readFile (file ++ ".bhg.gz")

loadWeights :: String -> IO (VU.Vector Double)
loadWeights file
  = fmap
    ( VU.fromList
    . B.decode
    . decompress
    )
  $ B.readFile (file ++ ".weights.gz")

loadText :: String -> IO String
loadText file
  = fmap (TIO.unpack . head . TIO.lines) $ TIO.readFile file

loadTokenMap :: String -> IO TokenMap
loadTokenMap file
  = fmap fromText
  $ TIO.readFile file

loadSentenceCorpus :: String -> IO [String]
loadSentenceCorpus file
  = fmap (map TIO.unpack . TIO.lines) $ TIO.readFile file


saveText :: String -> String -> IO ()
saveText text file = TIO.writeFile file (TIO.pack text)

saveWeights :: VU.Vector Double -> String -> IO ()
saveWeights v file
  = B.writeFile (file ++ ".weights.gz")
  $ compress
  $ B.encode
  $ VU.toList
  $ v

saveSequence :: [(Double, VU.Vector Double)] -> String -> IO ()
saveSequence s3q file
  = TIO.writeFile file
  $ TIO.unlines
  $ map (TIO.pack . show)
  $ s3q

toWSA :: String -> WSA.WSA Int String Double 
toWSA = WSA.fromList 1.0 . L.words

toWSAmap :: TokenMap -> String -> WSA.WSA Int Token Double 
toWSAmap tm = WSA.fromList 1.0 . map (getToken tm) . L.words

{-
scfgProduct
  :: (Show t, Show nt, Show p, Show i, Ord t, Ord nt, Ord p, Ord i)
  => (SyncPair t -> [Either Int t])
  -> WSA.WSA p t Double
  -> SCFG nt t i
  -> ( SCFG (p, nt, p) t i'
     , i' -> i
     , Feature (SyncPair t) (i, Int) Double)
scfgProduct component wsa (SCFG hg initnt)
  = (scfg', fst, feat)
  where
    scfg'
      = SCFG
        { toHypergraph = hg'
        , initNT
          = ( fst . head . WSA.initialWeights $ wsa
            , initnt
            , fst . head . WSA.finalWeights $ wsa
            )
        }
    (hg', wgt) = earley hg component wsa initnt
    feat = Feature pN V.singleton
    pN _ !i xs = (wgt VU.! snd i) * Prelude.product xs
-}
scfgProduct
  :: (Show t, Show nt, Show p, Show i, Ord t, Ord nt, Ord p, Ord i)
  => (l -> [Either Int t])
  -> WSA.WSA p t Double
  -> SCFG nt l i
  -> Feature l i Double
  -> (SCFG (p, nt, p) l (i, Int), Feature l (i, Int) (Double, Double))
scfgProduct component wsa (SCFG hg initnt) f1
  = (scfg', projLeft f1 +++ projRight f2)
  where
    scfg'
      = SCFG
        { toHypergraph = hg'
        , initNT
          = ( fst . head . WSA.initialWeights $ wsa
            , initnt
            , fst . head . WSA.finalWeights $ wsa
            )
        }
    (hg', wgt) = earley hg component wsa initnt
    f2 = Feature (\_ !i xs -> (wgt VU.! i) * Prelude.product xs) V.singleton

scfgProduct'
  :: (Show t, Show nt, Show p, Show i, Ord t, Ord nt, Ord p, Ord i)
  => (l -> [Either Int t])
  -> WSA.WSA p t Double
  -> SCFG nt l i
  -> SCFG (p, nt, p) l (i, Int)
scfgProduct' component wsa (SCFG hg initnt)
  = SCFG
    { toHypergraph = fst $ earley hg component wsa initnt
    , initNT
      = ( fst . head . WSA.initialWeights $ wsa
        , initnt
        , fst . head . WSA.finalWeights $ wsa
        )
    }

inputProduct
  :: (Show t, Show nt, Show p, Show i, Ord t, Ord nt, Ord p, Ord i)
  => WSA.WSA p t Double
  -> SCFG nt ([Either Int t], l) i
  -> Feature ([Either Int t], l) i Double
  -> (SCFG (p, nt, p) ([Either Int t], l) (i, Int), Feature ([Either Int t], l) (i, Int) (Double, Double))
inputProduct = scfgProduct fst


inputProduct'
  :: (Show t, Show nt, Show p, Show i, Ord t, Ord nt, Ord p, Ord i)
  => WSA.WSA p t Double
  -> SCFG nt ([Either Int t], l) i
  -> SCFG (p, nt, p) ([Either Int t], l) (i, Int)
inputProduct' = scfgProduct' fst


outputProduct
  :: (Show t, Show nt, Show p, Show i, Ord t, Ord nt, Ord p, Ord i)
  => WSA.WSA p t Double
  -> SCFG nt (l, [Either Int t]) i
  -> Feature (l, [Either Int t]) i Double
  -> (SCFG (p, nt, p) (l, [Either Int t]) (i, Int), Feature (l, [Either Int t]) (i, Int) (Double, Double))
outputProduct = scfgProduct snd

outputProduct'
  :: (Show t, Show nt, Show p, Show i, Ord t, Ord nt, Ord p, Ord i)
  => SCFG nt (l, [Either Int t]) i
  -> WSA.WSA p t Double
  -> SCFG (p, nt, p) (l, [Either Int t]) (i, Int)
outputProduct' = flip $ scfgProduct' snd


fakeWeights :: Ord nt => SCFG nt t i -> Feature (SyncPair t) i Double
fakeWeights _ = Feature (\ _ _ _ -> 1) V.singleton

bestDeriv
  :: (NFData x, NFData i, NFData l, NFData nt, Show l, Show nt, Show i, Ord i, Ord nt)
  => SCFG nt l i
  -> Feature l i x
  -> Maybe [Candidate nt l i x]
bestDeriv scfg feat
  = M.lookup (initNT scfg)
  $ knuth (toHypergraph scfg) feat (V.singleton 1.0)


makeFeature :: VU.Vector Double -> Feature l Int Double
makeFeature nweights
  = Feature (\_ !i xs -> (nweights VU.! i) * Prelude.product xs) V.singleton


candToString
  :: (Show l, Show v, Show i)
  => (l -> [Either Int String])
  -> Derivation v l i
  -> String
candToString component cand
  = L.unwords
  . map (either ((map (candToString component) (T.subForest cand)) !!) id)
  . component
  . label
  . T.rootLabel
  $ cand

getInputString
  :: (Ord v, Show v, Show i)
  => Maybe [Candidate v (SyncPair String) i x]
  -> String
getInputString = makeString fst

getOutputString
  :: (Ord v, Show v, Show i)
  => Maybe [Candidate v (SyncPair String) i x]
  -> String
getOutputString = makeString snd

makeString
  :: (Ord v, Show v, Show i, Show l)
  => (l -> [Either Int String])
  -> Maybe [Candidate v l i x]
  -> String
makeString component best 
  = case best of
      Nothing -> "(No translation.)"
      Just [] -> "(No translation.)"
      Just (c : _) -> {-trace (T.drawTree $ fmap show $ deriv c) $-} candToString component (deriv c)

initialWeights :: Ord nt => SCFG nt t i -> VU.Vector Double
initialWeights hg = VU.replicate (length (edges (toHypergraph hg))) 0.1

prepareExamples
  :: (Ord i, Ord nt, Show i, Show nt)
  => SCFG nt (SyncPair String) i
  -> [String]
  -> [String]
  -> [ ((Int, (Int, nt, Int), Int)
     , EdgeList (Int, (Int, nt, Int), Int) (SyncPair String) ((i, Int), Int)
     , Double)]
prepareExamples scfg input output
  = [ (initNT pr, toHypergraph pr, 1)
    | (inp, oup) <- zip input output
    , let pr = (toWSA inp `inputProduct'` scfg) `outputProduct'` toWSA oup
    ]

myflatten :: T.Tree t -> [t]
myflatten T.Node{T.rootLabel = l, T.subForest = s}
  | null s = [l]
  | otherwise = concatMap myflatten s

prepareExamplesGHKM
  :: (Ord i, Ord nt, Show i, Show nt)
  => SCFG nt (GHKM Token) i
  -> TokenMap
  -> [String]
  -> [String]
  -> [ ((Int, (Int, nt, Int), Int)
     , EdgeList (Int, (Int, nt, Int), Int) (GHKM Token) ((i, Int), Int)
     , Double)]
prepareExamplesGHKM scfg tm input output
  = [ (initNT pr, toHypergraph pr, 1)
    | (inp, oup) <- zip input output
    , let pr = (scfgProduct' (myflatten . fst) (toWSAmap tm inp) scfg)
               `outputProduct'` toWSAmap tm oup
    ]

preparePartition
  :: Ord nt => SCFG nt t i -> [[i]]
preparePartition hg
  = M.elems . M.fromListWith (++)
  $ [ (to e, [ident e]) | e <- edges (toHypergraph hg) ]


doEM
  :: (Show v, Ord v, Hypergraph h, Integral i)
  => [[i]]
  -> [(v, h v l ((i, i1), i2), Double)]
  -> VU.Vector Double
  -> [(Double, VU.Vector Double)] 
doEM part exs initw
  = take 10 $ forestEMlist part exs (fst . fst . ident) initw 

getVector :: [(Double, VU.Vector Double)] -> VU.Vector Double
getVector = snd . (!! 9)
