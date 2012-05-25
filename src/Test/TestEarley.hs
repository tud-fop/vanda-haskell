module Main where 

import Data.Tree as T
import qualified Data.Map as M
import qualified Data.Ix as Ix
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import Codec.Compression.GZip ( decompress )
import Data.Int ( Int32 )
import Debug.Trace
import Control.DeepSeq
import qualified Data.Text.Lazy as TIO
import qualified Data.Text.Lazy.IO as TIO

import Vanda.Algorithms.Earley.Earley_WSA as E
import Vanda.Algorithms.Earley.WSA
import Vanda.Hypergraph
import Vanda.Hypergraph.BackwardStar (fromEdgeList)
import Vanda.Hypergraph.Binary ()
import Vanda.Token

{-
instance (Show v, Show l, Show i) => Show (Hyperedge v l i) where
  show e
    = show (to e)
      ++ " -> "
      ++ show (label e)
      ++ " "
      ++ unwords (Prelude.map show $ from e)
      ++ " # "
      ++ show (ident e)
      ++ "\n"
-}
      
-- instance (Show v, Show l, Show i) => Show (Candidate v l i x) where
--   show (Candidate w d _) = show w ++ " -- " ++ show d
instance (Show v, Show i, Show l, Ix.Ix v) => Show (EdgeList v l i) where 
  show g 
    = show $ edgesEL  g

    
instance NFData (BackwardStar Token (T.Tree (Either Int Token), [Either Int Token]) Int) where
  rnf (BackwardStar nodes edges memo) 
    = (rnf nodes `seq` rnf edges) `seq` rnf memo 

main :: IO()
main = do (graph , weights) <- hg
          let erg = E.earley graph weights wsa 
              strerg =  show erg 
              terg = TIO.pack strerg
          erg `deepseq` trace "erg in main berechnet"
              $ strerg `deepseq` trace "strerg berechnet"
              $ terg `deepseq` trace "terg berechnet"
              -- print erg
              $ TIO.writeFile "/home/student/lindal/files/git/testEarley.txt" 
              $ TIO.pack $ show erg
              -- --print  
                -- terg
              -- --  $ (graph , weights)  
            
wsa:: WSA Int Token Double
wsa = create [ Transition 0 0 1 0.4
             , Transition 1 1 2  0.45
             , Transition 2 2 3  0.45
             , Transition 3 3 4  0.45
             -- , Transition 1 0 1  0.6
             -- , Transition 4 1 2  0.55
             -- , Transition 0 2 3  0.55
             -- , Transition 2 3 4  0.55

             ]
             [(0,1),(1,0), (2, 0), (3, 0), (4,0)] 
             [(0,0),(1,0), (2,0), (3,0), (4,1)]
             
hg:: IO(EdgeList Token (T.Tree (Either Int Token), [Either Int Token]) Int
      , M.Map Int Double)
hg = do dec :: EdgeList Token (T.Tree (Either Int Token), [Either Int Token]) Int
             <- fmap (force . B.decode . decompress) 
                      $ B.readFile "/home/student/lindal/files/git/rules.bhg.gz"
        -- let retdec = reduce dec
        return $ (force {- $ fromEdgeList -} dec , 
                  M.fromList 
                   (zip (map ident $ edgesEL dec) 
                          (cycle [1])
                   )
                  )
                  
reduce:: 
       EdgeList Token (T.Tree (Either Int Token), [Either Int Token]) Int
    -> EdgeList Token (T.Tree (Either Int Token), [Either Int Token]) Int
reduce graph = let el = edgesEL graph       
                   redEL = map (el !!) [1,101..(length el)-101]
               in mkHypergraph redEL
               
               
               
hg2:: IO (EdgeList Token (T.Tree (Either Int Token), [Either Int Token]) Int
      , M.Map Int Double)
hg2 = return (mkHypergraph 
                  [mkHyperedge
                    0 [1,2] (  Node (Left 17)
                          [ Node (Left 0) []
                          , Node (Left 1) []
                          ] 
                     , [Left 0, Left 1]) 0
                  , mkHyperedge 1 [] (Node (Right 56) [], [Right 0]) 1
                  , mkHyperedge 1 [] (Node (Right 56) [], [Right 2]) 2
                  , mkHyperedge 1 [1,3] (Node (Right 56) [], [Left 0, Left 1]) 3
                  , mkHyperedge 2 [1] ( Node (Right 53) [], [Right 1, Left 0]) 4
                  , mkHyperedge 2 [1,3] ( Node (Right 53) [], [Right 1, Left 0, Left 1]) 5
                  , mkHyperedge 3 [] ( Node (Right 53) [], [Right 3]) 6
                  ]
       ,M.fromList [(0,1), (1,0.5), (2,0.2), (3,0.3), (4,0.6), (5,0.4), (6,1)]
       )

