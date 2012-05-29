module Main where 

import qualified Data.Set as S
import Debug.Trace
import System.Environment ( getArgs, getProgName )

import Vanda.Hypergraph
import Vanda.Functions

{-
instance (Show v, Show i, Show l, Show x) => Show (Candidate v l i x) where 
  show c
    = "Gewicht: " ++ (show $ weight c) ++ "\n Ableitung: "
      ++ (show $ deriv c) ++ "\fdata: "
      ++ (show $ fdata c)
-}

instance (Show v, Show i, Show l, Ord v) => Show (EdgeList v l i) where 
  show g 
    = show (S.toList $ nodes g) ++ "\n" ++ unlines (map show (edges g))

doTrain scfg input output = traceShow part $ traceShow examples $ traceShow s3q $ v where
  examples = prepareExamples scfg input output -- :: WSA Int  l v 
  wvector = initialWeights scfg
  part = preparePartition scfg
  s3q = doEM part examples wvector
  v = getVector s3q


main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    ["-g", graph, "-s", inFile, "-t", outFile, "-w", vFile] -> do
      hg <- loadSCFG graph
      -- weights <- loadWeights
      input <- loadSentenceCorpus inFile
      output <- loadSentenceCorpus outFile
      let v = doTrain hg input output
      saveWeights v vFile
    _ -> print "Syntax error"
