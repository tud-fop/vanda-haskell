module Main  where

import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import Data.Tree as T
import Codec.Compression.GZip ( compress )
import System.Environment ( getArgs, getProgName )

import Vanda.Hypergraph.Binary ()
import Vanda.Hypergraph
import Vanda.Token

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    ["-o", outfile] ->
      let rules = hgString
      in B.writeFile (outfile ++ ".bhg.gz") (compress $ B.encode rules)
    _ -> print $ "Usage: " ++ progName ++ "-o outfile"    

hgString :: EdgeList String ([Either Int String], [Either Int String]) Int
hgString
  = mkHypergraph 
      [ mkHyperedge "X" ["X","X"]
        ( [Left 0, Right "duonianlai", Right "de", Left 1]
        , [Left 1, Right "over", Right "the", Right "last", Left 0, Right "years"]
        ) 0
      , mkHyperedge "X" ["X"]
        ( [Left 0, Right "duonianlai"],
          [Right "over", Right "the", Right "last", Left 0, Right "years"]
        ) 1
      , mkHyperedge "X" []
        ( [Right "youhao"]
        , [Right "friendly"]
        ) 2
      , mkHyperedge "X" [] 
        ( [Right "hezuo"]
        , [Right "cooperation"]
        ) 3
      , mkHyperedge "X" []
        ( [Right "30"]
        , [Right "30"]
        ) 4
      , mkHyperedge "X" []
        ( [Right "nian"]
        , [Right "years"]
        ) 5
      , mkHyperedge "X" ["X","X"]
        ( [Left 0, Left 1]
        , [Left 0, Left 1]
        ) 6
      , mkHyperedge "X" ["X"]
        ( [Left 0]
        , [Left 0]
        ) 7
      , mkHyperedge "X" ["X","X"]
        ( [Right "yu", Left 0, Right "you", Left 1]
        , [Right "have", Left 1, Right "with", Left 0]
        ) 8
      , mkHyperedge "X" ["X","X"]
        ( [Left 0, Right "de", Left 1]
        , [Right "the", Left 1, Right "that", Left 0]
        ) 9
      , mkHyperedge "X" ["X"]
        ( [Left 0, Right "zhiyi"]
        , [Right "one", Right "of",  Left 0]
        ) 10
      , mkHyperedge "X" []
        ( [Right "Aozhou"]
        , [Right "Australia"]
        ) 11
      , mkHyperedge "X" [] 
        ( [Right "Beihan"]
        , [Right "North", Right "Korea"]
        ) 12
      , mkHyperedge "X" [] 
        ( [Right "shi"]
        , [Right "is"]
        ) 13
      , mkHyperedge "X" [] 
        ( [Right "bangjiao"]
        , [Right "diplomatic", Right "relations"]
        ) 14
      , mkHyperedge "X" []
        ( [Right "shaoshu", Right "guojia"]
        , [Right "few", Right "countries"]
        ) 15
      ]  
     
hgString' :: EdgeList String (T.Tree (Either Int String), [Either Int String]) Int
hgString' = mkHypergraph 
              [ mkHyperedge "X" ["X","X"] (Node (Right "") [Node (Left 0) [], Node (Right "duonianlai") [], Node (Right "de") [], Node (Left 1) []]
                            , [Left 1, Right "over", Right "the", Right "last", Left 0 , Right "years"]) 1
              ,mkHyperedge "X" ["X"] (Node (Right "") [Node (Left 0) [], Node (Right "duonianlai") []] , [Right "over", Right "the", Right "last", Left 0, Right "years"]) 2
              ,mkHyperedge "X" [] (Node (Right "") [Node (Right "youhao") [] ], [Right "friendly"]) 3
              ,mkHyperedge "X" [] (Node (Right "") [Node (Right "hezuo") []], [Right "cooperation"]) 4
              ,mkHyperedge "X" [] (Node (Right "") [Node (Right "30") []], [Right "30"] ) 5
              ,mkHyperedge "X" [] (Node (Right "") [Node (Right "nian") []], [Right "years"] ) 6
              ,mkHyperedge "X" ["X","X"] (Node (Right "") [Node (Left 0) [] ,Node (Left 1) []] , [Left 0, Left 1] ) 7
              ,mkHyperedge "X" ["X"](Node (Right "") [Node (Left 0) []] , [ Left 0] ) 8
              ,mkHyperedge "X" ["X","X"] (Node (Right "") [Node (Right "yu") [] , Node (Left 0)  [], Node (Right "you") [], Node (Left 1) [] ], [Right "have", Left 1 , Right "with", Left 0] ) 9
              ,mkHyperedge "X" ["X","X"] (Node (Right "") [Node (Left 0) [] , Node (Right "de") [], Node (Left 1)  [] ] , [Right "the", Left 1, Right "that", Left 0] ) 10
              ,mkHyperedge "X" [] (Node (Right "") [Node (Left 0) [], Node (Right "zhiyi") []] , [Right "one", Right "of",  Left 0] ) 11
              ,mkHyperedge "X" [] (Node (Right "") [Node (Right "Aozhou") []], [Right "Australia"] ) 12
              ,mkHyperedge "X" [] (Node (Right "") [Node (Right "Beihan") []] , [Right "North", Right "Korea"] ) 13
              ,mkHyperedge "X" [] (Node (Right "") [Node (Right "shi") [] ], [Right "is"] ) 14
              ,mkHyperedge "X" [] (Node (Right "") [Node (Right "bangjiao") []] , [Right "diplomatic", Right "relations"] ) 15
              ,mkHyperedge "X" [] (Node (Right "") [Node (Right "shaoshu") [] , Node (Right "guojia") []] , [Right "few", Right "countries"] ) 16
              ]  
              
hgInt :: EdgeList Token ([Either Int Token], [Either Int Token]) Int
hgInt = mkHypergraph 
              [ mkHyperedge 0 [0,0] ([(Left 0), (Right 1) , (Right 2), (Left 1)]
                            , [Left 1, Right 3, Right 4, Right 5, Left 0 , Right 6]) 1
              ,mkHyperedge 0 [0] ([ (Left 0), (Right 1)] , [Right 3, Right 4, Right 5, Left 0, Right 6]) 2
              ,mkHyperedge 0 [] ( [(Right 7) ], [Right 8]) 3
              ,mkHyperedge 0 [] ( [(Right 9) ], [Right 10]) 4
              ,mkHyperedge 0 [] ( [(Right 11) ], [Right 11] ) 5
              ,mkHyperedge 0 [] ( [(Right 12) ], [Right 6] ) 6
              ,mkHyperedge 0 [0,0] ( [(Left 0)  ,(Left 1) ] , [Left 0, Left 1] ) 7
              ,mkHyperedge 0 [0] ( [(Left 0) ] , [ Left 0] ) 8
              ,mkHyperedge 0 [0,0] ( [(Right 13)  , (Left 0)  , (Right 14) , (Left 1)  ], [Right 15, Left 1 , Right 16, Left 0] ) 9
              ,mkHyperedge 0 [0,0] ( [(Left 0)  , (Right 2) , (Left 1)   ] , [Right 17, Left 1, Right 18, Left 0] ) 10
              ,mkHyperedge 0 [0] ( [(Left 0) , (Right 19) ] , [Right 20, Right 21,  Left 0] ) 11
              ,mkHyperedge 0 [] ( [(Right 22) ], [Right 23] ) 12
              ,mkHyperedge 0 [](  [(Right 24) ] , [Right 25, Right 26] ) 13
              ,mkHyperedge 0 [](  [(Right 27)  ], [Right 28] ) 14
              ,mkHyperedge 0 [](  [(Right 29) ] , [Right 30, Right 31] ) 15
              ,mkHyperedge 0 [](  [(Right 32)  , (Right 33) ] , [Right 34, Right 35] ) 16
              ]  
         
