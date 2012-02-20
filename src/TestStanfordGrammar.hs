-- (c) 2012 Johannes Osterholzer <Johannes.Osterholzer@tu-dresden.de>
-- (c) 2012 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

-- Disable nagging about missing type signatures
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TestStanfordGrammar where


import Algorithms.CYKExtended
import Algorithms.InsideOutsideWeights
import qualified Algorithms.KAStar as K
import Data.Hypergraph
import Data.WCFG
import Parser.StanfordGrammar
-- import TestData.TestHypergraph

import Control.DeepSeq
import Data.Either
import qualified Data.Map as M
import Data.Time.Clock
import qualified Data.Tree as T
import System.CPUTime
import System.Environment (getArgs)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (parseFromFile)


main :: IO ()
main = do
  let str = words "The big man has the ball ."
  args <- getArgs
  b <- case args of
        "kbest" : []         -> testKBest 10 str 1.0
        "kbest" : d : []     -> testKBest 10 str (read d)
        "kbest" : d : s : [] -> testKBest 10 (words s) (read d)
        "nbest" : []         -> testNBest 10 str
        "nbest" : s : []     -> testNBest 10 (words s)
        _ -> error "Usage: ... (nbest [sentence] | kbest [Double [sentence]])"
--   let b = reverse $ L.sortBy (compare `on` foo) b0
  putStrLn
    . unlines
    . map
        (\ (t, w) -> show w ++ "\n" ++ T.drawTree (fmap show $ prettyDeriv t))
    $ b
  where
    prettyDeriv (T.Node r ts)
      = T.Node ((\ (_, nt, _) -> nt) (eHead r))
      $ if null ts
        then map (flip T.Node []) $ rights (eLabel r)
        else map prettyDeriv ts
--     foo :: (T.Tree (Hyperedge Char Char Double Int), Double) -> (Double, String)
--     foo (t, w) = (w, T.drawTree (fmap drawHyperedge t))


test, test3 :: IO (Either ParseError (WCFG String String Double Int))
test = parseFromFile p_grammar
          "/home/gdp/oholzer/build/stanford-parser-2012-01-06/gram.txt"
test3 = parseFromFile p_grammar
          "/home/gdp/oholzer/build/stanford-parser-2012-01-06/gram3.txt"


-- This fails if grammar cannot be parsed
properTestGraph str
  =   fmap (\ (Right x) -> x) test
  >>= measure "loading grammar"
  >>= measure "properizing grammar"
      . (\ g -> wcfgFromHypergraph (initial g)
              $ properize
              $ productionsHypergraph g
        )
  >>= measure "cyk" . cyk str
  >>= measure "making ids distinct"
      . (\ g -> wcfgFromHypergraph (initial g)
              $ snd
              $ mapAccumIds (\ (i : is) _ -> (is, i)) [0 :: Int ..]
              $ productionsHypergraph g
        )
  >>= (\ g -> return (initial g, productionsHypergraph g))


testNBest n str = do
  (goal, graph) <- properTestGraph str
  ret <- measure "NBest" $ nBest' n goal graph
  return ret


testKBest n str r = do
  (goal, graph) <- properTestGraph str
  wm <- measure "outside weights" $ weightMap goal graph r
  ret <- measure "KBest"
       $ K.kbest graph goal (\node -> M.findWithDefault 0.0 node wm) n
  return ret
  where
    weightMap goal graph r'
      = M.map (\p -> r' * snd p + (1 - r'))
      $ viterbiInsideOutside eWeight goal graph


measure :: NFData a => String -> a -> IO a
measure cs x = do
  t0 <- getCPUTime
  t1 <- t0 `deepseq` x `deepseq` getCPUTime
  deepseq t1
    $ putStrLn
    $ (show $ picosecondsToDiffTime $ t1 - t0) ++ "\tfor " ++ cs
  return x
