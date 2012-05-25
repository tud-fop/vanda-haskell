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


import Vanda.Algorithms.Earley.Earley_WSA as E
import Vanda.Algorithms.Earley.WSA as WSA
import Vanda.Hypergraph.BackwardStar (fromEdgeList)
import Vanda.Features
import Vanda.Token
import Vanda.Hypergraph hiding ( nodes )
import Vanda.Hypergraph.Binary ()
-- import Vanda.Hypergraph.NFData ()
import Vanda.Token


instance NFData (BackwardStar Token (T.Tree (Either Int Token), [Either Int Token]) Int) where
  rnf (BackwardStar nodes edges memo) 
    = (rnf nodes `seq` rnf edges) `seq` rnf memo 

instance (Show v, Show i, Show l, Show x) => Show (Candidate v l i x) where 
  show c
    = "Gewicht: " ++ (show $ weight c) ++ "\n Ableitung: "
      ++ (show $ deriv c) ++ "\fdata: "
      ++ (show $ fdata c)
      
instance Real (Int, Int) where
  toRational a = toRational $ snd a

instance Num (Int,Int) where
  (+) = (+)
  (*) = (*)
  (-) = (-) 
  abs =  abs 
  signum = signum  
  fromInteger = fromInteger 
  
instance Enum (Int, Int) where
  succ = succ 
  
instance Integral (Int,Int) where
  quotRem a b = quotRem  a  b
  toInteger a = toInteger (snd a)  
  
instance (Show v, Show i, Show l, Ix.Ix v) => Show (EdgeList v l i) where 
  show g 
    = show (S.toList $ nodesEL g) ++ "\n" ++ show (edgesEL  g)
  
instance (NFData l, VU.Unbox l) => NFData (VU.Vector l) where
  rnf v = rnf $ VU.toList v    

instance NFData (Feature l i x) where
  rnf (Feature pn fin) = rnf pn `deepseq` rnf fin    

instance Eq (Candidate v (Tree (Either Int Token), [Either Int Token]) i x) where
  (==)  = (==)
  
main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    ["-g", graph, "-w" , weights, "-t", token, "-s", input] 
     -> if ((length $ L.lines input) == 0) 
        then error "empty word"
        else do
          hg <- testHg graph
          -- weights <- readWeights 
          let weights = testWeights (length $ edgesEL $ toEdgeList hg)
          let wghts = M.fromList 
                  $ zip (map ident $ edgesEL $ toEdgeList hg) (VU.toList weights)
              word = L.words input
              l = length word 
          words <- tokens token word --[0 .. l-1]      -- words are not yet decoded with
                                                 -- the correct integers
          let wsa = WSA.fromList 1 words -- :: WSA Int  l v 
              (iGraph,newWeights) = E.earley hg wghts wsa  
                                             -- compute the intersection of the 
                                             -- hypergraph and the word.
                                             -- The initial node should  be 0 or
                                             -- changed
              newWeightsVec = toVector newWeights                                     
              pN !_ !i xs = (newWeightsVec VU.! fromIntegral (snd i)) * Prelude.product xs
              feat = (Feature pN V.singleton)
              edgeGraph  = (toEdgeList iGraph)
              best = knuth edgeGraph feat (V.singleton 1) 
              bestAt = M.lookup (0,0,l) best  
              der = if not (bestAt == Nothing) 
                    then candToString $ head $ fromJust bestAt 
                    else error "The word is not derivable."
          wsa `deepseq` trace "+++++wsa:++++++" $ traceShow wsa
            $ hg `deepseq` trace "Hypergraph gelesen" 
            $ weights `deepseq` trace "weights berechnet"
            $ trace "wordlength" $ traceShow (length word)
            $ trace "word: " traceShow words 
            $ iGraph `deepseq` trace "hg berechnet" $ traceShow iGraph
            $ newWeights `deepseq` trace "neue Gewichte: " $ traceShow newWeights
            $ feat `deepseq` trace "feature berechnet"
            $ edgeGraph `deepseq` trace "edgeGraph berechnet" $ traceShow edgeGraph
            $ trace "---------------------------\n---------------------"
            $ print der
    _ -> print $ "Usage: " ++ progName ++ "-g graphfile -t tokenfile -s sentence"    
     
          
-- Achtung! Gewichte der Kanten sind momentan alle 1!
readHg:: String -> IO(EdgeList Token (T.Tree (Either Int Token), [Either Int Token]) Int)
      -- , M.Map Int Double)
readHg file = do 
            dec :: EdgeList Token (T.Tree (Either Int Token), [Either Int Token]) Int
             <- fmap (force . B.decode . decompress) 
                      $ B.readFile file
                          -- "/home/student/lindal/files/git/rules.small.bhg.gz"
        -- let retdec = reduce dec
            return $ force {- $ fromEdgeList -} dec  
                  -- M.fromList 
                   -- (zip (map ident $ edgesEL dec) 
                          -- (cycle [1])
                   -- )
                  -- )
                  
testHg:: String -> IO (EdgeList Token (T.Tree (Either Int Token), [Either Int Token]) Int)
      -- , M.Map Int Double)
testHg _ = return $ mkHypergraph 
                  [mkHyperedge
                    0 [1,2] (  Node (Left 17)
                          [ Node (Left 0) []
                          , Node (Left 1) []
                          ] 
                     , [Left 0, Left 1]) 0
                  , mkHyperedge 1 [] (Node (Right 56) [], [Right 24154]) 1
                  , mkHyperedge 1 [] (Node (Right 56) [], [Right 25931]) 2
                  , mkHyperedge 1 [1,3] (Node (Right 56) [], [Left 0, Left 1]) 3
                  , mkHyperedge 2 [1] ( Node (Right 53) [], [Right 24150, Left 0]) 4
                  , mkHyperedge 2 [1,3] ( Node (Right 53) [], [Right 24150, Left 0, Left 1]) 5
                  , mkHyperedge 3 [] ( Node (Right 53) [], [Right 19067]) 6
                  ]
       -- ,M.fromList [(0,1), (1,0.5), (2,0.2), (3,0.3), (4,0.6), (5,0.4), (6,1)]
       -- )
       
       
testWeights:: Int -> VU.Vector Double
testWeights n = VU.replicate n 1  
       
readWeights:: IO(VU.Vector Double)
readWeights = fmap
             (VU.fromList . B.decode . decompress)
           $ B.readFile
           $ "/home/student/lindal/Downloads/SHK/Berkeley/berkeley.bhg.gz.weights.gz"
           
tokens:: String -> [String] -> IO([Token])
tokens tokenfile input = do 
    tokenm :: TokenMap 
      <- fmap ( force . fromText) $ TIO.readFile tokenfile
    return $ map (getToken tokenm) input
           
           
toVector:: M.Map (Int,Int) Double -> VU.Vector Double
toVector weights 
  = let extract ((_,id2),w) = (id2,w)
        res = VU.fromList $ map snd $ L.sort $ map extract $ M.toList weights
    in --trace "start toVector" res `deepseq` trace "end toVector" 
        res
        
        
candToTree:: Candidate v (T.Tree (Either Int Token), [Either Int Token]) i x -> (Double, T.Tree v )
candToTree cand = undefined

candToString:: Candidate v (T.Tree (Either Int Token), [Either Int Token]) i x -> [Token]
candToString cand = concat $ map (extractTerms . label) $ T.flatten $ deriv cand  

extractTerms:: (T.Tree (Either Int Token), [Either Int Token]) -> [Token]
extractTerms (tree, _) = rights $ T.flatten tree 


     
