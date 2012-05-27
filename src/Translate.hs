module Main where 

import System.Environment ( getArgs, getProgName )

import Vanda.Functions


doTranslate scfg weights input = output where
  feat = makeFeature weights
  wsa = toWSA input -- :: WSA Int  l v 
  (scfg', feat') = inputProduct wsa scfg feat
  best = bestDeriv scfg' feat'
  output = getOutputString best


main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    ["-g", graph, "-w", weightsFile, "-s", inFile, "-t", outFile] -> do
      hg <- loadSCFG graph
      weights <- loadWeights weightsFile
      input <- loadText inFile
      let output = doTranslate hg weights input
      saveText output outFile
    _ -> print $ "Syntax error"
