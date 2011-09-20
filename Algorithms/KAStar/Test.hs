-- (c) Johannes Osterholzer <oholzer@gmx.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

module Algorithms.KAStar.Test (test)
  where

import Data.Hypergraph
import qualified TestData.TestHypergraph as Test

import qualified Data.Tree as T
import Control.Monad (zipWithM, liftM)
import Control.DeepSeq

import Algorithms.KAStar

test1 = hypergraph [ hyperedge 'g' "ab" ' ' 1.0 ()
                   , hyperedge 'a' ""   ' ' 1.0 ()
                   , hyperedge 'b' ""   ' ' 1.0 ()
                   , hyperedge 'g' "g"  ' ' 0.9 ()
                   ]


heur1 :: Fractional w => Char -> w
heur1 _ = 1.0


test2 = hypergraph [ hyperedge 'a' ""   "alpha"   1.0 ()
                   , hyperedge 'b' ""   "beta"    1.0 ()
                   , hyperedge 'g' "ab" "sigma"   0.8 ()
                   , hyperedge 'a' "g"  "delta"   0.9 ()
                   , hyperedge 'b' "g"  "epsilon" 0.8 ()
                   , hyperedge 'g' "g"  "loop"    1.0 ()
                   , hyperedge 'x' "g"  "horst"   0.5 ()
                   ]


t graph goal h k = do
  putStrLn $ drawHypergraph graph
  mapM_ (putStrLn . uncurry str) $ kbest graph goal h k
    where str t w = "w = " ++ show w ++ "\n" 
                    ++ (T.drawTree . fmap drawHyperedge $ t)


-- Let's check the reference implementation
t' graph goal k = do
  putStrLn $ drawHypergraph graph
  mapM_ (putStrLn . uncurry str) $ nBest' k goal graph
    where str t w = "w = " ++ show w ++ "\n" 
                    ++ (T.drawTree . fmap drawHyperedge $ t)


t1 = t test1 'g' heur1 20


t2 = t test2 'x' heur1 20

t3 :: [(T.Tree (Hyperedge Char Char Double ()), Double)]
t3 = kbest (Test.testHypergraphs !! 1) 'S' heur1 1000

t3' :: [(T.Tree (Hyperedge Char Char Double ()), Double)]
t3' = nBest' 1000 'S' (Test.testHypergraphs !! 1)

t4 = t (Test.testHypergraphs !! 1) 'S' heur1 10

comparison 
  :: (Fractional w, Ord v, Ord w, Ord i, Ord l, Show i, Show l, Show v) 
  => Hypergraph v l w i -> v -> (v -> w) -> Int -> IO Bool
comparison graph goal heur k = and `liftM` zipWithM put mine others
  where put w1 w2 = do 
          putStrLn $ show w1 ++ "\t" ++ show w2
          return $ w1 == w2
        mine = map snd $ kbest graph goal heur k
        others = nBest k goal graph

diff
  :: (Fractional w, Ord v, Ord w, Ord i, Ord l, Show i, Show l, Show v) 
  => Hypergraph v l w i 
  -> v 
  -> (v -> w) 
  -> Int 
  -> [((T.Tree (Hyperedge v l w i), w), (T.Tree (Hyperedge v l w i), w))]
diff graph goal heur k = filter neq $  zip mine others
  where neq ((_, w1), (_, w2)) = w1 /= w2
        mine = kbest graph goal heur k
        others = nBest' k goal graph

test :: IO ()
test = comparison (Test.testHypergraphs !! 1) 'S' heur1 10 >>= putStrLn . show
--test = t3 `deepseq` return ()
--test = t (Test.testHypergraphs !! 1) 'S' heur1 500
--test = (zipWith (\graph goal -> kbest graph goal (heur1::Char->Double) 1000) 
--                Test.testHypergraphs "AStt")
--       `deepseq` return ()
--test = mapM_ (uncurry go) (tail $ zip Test.testHypergraphs "AStt")
  -- where
  --   go graph start = mapM_ (uncurry pr) $ diff graph start heur1 50
  --   pr l r = do
  --     putStrLn "===MINE==="
  --     putStrLn . uncurry str $ l
  --     putStrLn "===OTHER==="
  --     putStrLn . uncurry str $ r
  --   str t w = "w = " ++ show w ++ "\n" 
  --             ++ (T.drawTree . fmap drawHyperedge $ t)
