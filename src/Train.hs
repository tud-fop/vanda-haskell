module Main where 

import Data.Either
import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S
import Data.Tree as T
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Ix as Ix
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import Codec.Compression.GZip ( decompress )
import Data.Int ( Int32 )
import Debug.Trace
import Control.DeepSeq
import qualified Data.Text.Lazy as TIO
import qualified Data.Text.Lazy.IO as TIO
import System.Environment ( getArgs, getProgName )


import qualified Vanda.Algorithms.EarleyCFG as E
import qualified Vanda.Algorithms.Earley.WSA as WSA
import Vanda.Hypergraph.BackwardStar (fromEdgeList)
import Vanda.Features
import Vanda.Token
import Vanda.Hypergraph
import Vanda.Hypergraph.Binary ()
-- import Vanda.Hypergraph.NFData ()
import Vanda.Token

import Vanda.Algorithms.InsideOutsideWeights
import Vanda.Algorithms.ExpectationMaximization


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
    

loadSCFG
  :: String
  -> IO (BackwardStar String ([Either Int String], [Either Int String]) Int)
loadSCFG file
  = fmap (fromEdgeList . B.decode . decompress) $ B.readFile file

loadWeights :: String -> IO (V.Vector Double)
loadWeights file
  = fmap (V.fromList . B.decode . decompress) $ B.readFile file
  
loadText :: String -> IO String
loadText file
  = fmap (TIO.unpack . head . TIO.lines) $ TIO.readFile file

loadSentenceCorpus :: String -> IO [String]
loadSentenceCorpus file
  = fmap (map TIO.unpack . TIO.lines) $ TIO.readFile file


saveText :: String -> String -> IO ()
saveText text file = TIO.writeFile file (TIO.pack text)

saveSequence :: [(Double, VU.Vector Double)] -> String -> IO ()
saveSequence s3q file
  = TIO.writeFile file
  $ TIO.unlines
  $ map (TIO.pack . show)
  $ s3q

toWSA :: String -> WSA.WSA Int String Double 
toWSA input = WSA.fromList 1 (L.words input)

{-
fakeWeights
  :: Hypergraph h 
  => h Token ([Either Int Token], [Either Int Token]) Int 
  -> M.Map Int Double
fakeWeights hg 
  = M.fromList $ zip (map ident $ edgesEL $ toEdgeList hg) (repeat 1)
-}

makeFeature nweights
  = (Feature pN V.singleton, V.singleton 1.0)
  where
    pN _ !i xs = (nweights V.! fromIntegral (snd i)) * Prelude.product xs

getInitial hg = S.findMin (nodes hg)

newInitial v0 text = (0, v0, length (L.words text)) 

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

makeString
  :: (Ord p, Ord v, Show v, Show i, Show p, Show l)
  => M.Map (p, v, p)  [Candidate (p, v, p) l i Double] 
  -> (p, v, p)
  -> (l -> [Either Int String])
  -> String
makeString best nv0 component 
  = case M.lookup nv0 best of
      Nothing -> "(No translation.)"
      Just [] -> "(No translation.)"
      Just (c : _) -> trace (T.drawTree $ fmap show $ deriv c) $ candToString component (deriv c)

initialWeights hg = VU.replicate (length (edges hg)) 0.1

prepareExamples hg v0 input output
  = [ (v0both
      , fst
      $ E.earley
          ( fst
          $ E.earley hg fst (toWSA inp) v0
          )
          snd (toWSA oup) v0one
      , 1
      )
    | (inp, oup) <- zip input output
    , let v0one = newInitial v0 inp
    , let v0both = newInitial v0one oup
    ]

preparePartition hg
  = M.elems . M.fromListWith (++)
  $ [ (to e, [ident e]) | e <- edges hg ]

doEM part exs initw
  = take 100 $ forestEMlist part exs (fst . fst . ident) initw 

doTrain hg input output = s3q where
  -- weights = fakeWeights hg
  v0 = getInitial hg
  examples = prepareExamples hg v0 input output -- :: WSA Int  l v 
  wvector = initialWeights hg
  part = preparePartition hg
  s3q = doEM part examples wvector


main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    ["-g", graph, "-s", inFile, "-t", outFile, "-w", s3qFile] -> do
      hg <- loadSCFG graph
      -- weights <- loadWeights
      input <- loadSentenceCorpus inFile
      output <- loadSentenceCorpus outFile
      let s3q = doTrain hg input output
      saveSequence s3q s3qFile
    _ -> print "Syntax error"
