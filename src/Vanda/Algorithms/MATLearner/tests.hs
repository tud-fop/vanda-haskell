import Vanda.Algorithms.MATLearner.MATLearner 
import Vanda.Algorithms.MATLearner.TreeAutomaton
import Data.Tree
import Data.Map hiding (foldr,foldl,map)
import Control.Monad.State
import qualified Data.Set as S
import qualified Data.Vector as V
import Vanda.Hypergraph.Basic
import Vanda.Algorithms.MATLearner.TreesContexts


--testClosify :: 
testClosify = evalState (closify (getSigmaS s [(1,0),(2,0),(3,2)]) (Corpus [(Node 1 [])])) (OT (s,contexts,mapping)) 
--member (Node 3 [(Node 1 []),(Node 1 [])]) mapping--(obst (Node 1 []) contexts mapping)--(getTable s contexts mapping)--
    where 
        s = [(Node 1 [])]
        contexts = [X,(CNode 3 [X,(CNode 1 [])])]
        mapping = fromList [((Node 1 [])                       ,True),((Node 2 [])                       ,True),
                            ((Node 3 [(Node 1 []),(Node 1 [])]),True),((Node 3 [(Node 2 []),(Node 1 [])]),True),
                            ((Node 3 [(Node 3 [(Node 1 []),(Node 1 [])]),(Node 1 [])]),False)] 



testGenerateAutomaton = generateAutomaton (OT ([(Node 1 [])] ,[X] , fromList [((Node 1 []),True),((Node 2 [(Node 1 [])]),True)])) [(1,0),(2,1)]
{-

testGetTable = (any ((obst (Node 1 []) contexts mapping)==) (getTable s contexts mapping)) --getTable s contexts mapping
    where 
        s = [(Node 1 [])]
        contexts = [X]
        mapping = fromList [((Node 1 []),False),((Node 2 []),False)]
        -}
instance Show Corpus where
  show (Corpus a) = show a

generateCorpus :: Int -> Corpus
generateCorpus i = Corpus (generateList i)
  where 
    generateList :: Int -> [Tree Int]
    generateList 0 = []
    generateList i = (generateTree i) : (generateList (i-1))
      where 
        generateTree :: Int -> Tree Int
        generateTree i = Node i (generateList (i-1))

exampleAutomaton = Automaton
  (EdgeList 
    (S.fromList [0,1,2]) 
    [ Hyperedge 0 (V.fromList [2]) 1 0
    , Hyperedge 1 (V.fromList [0]) 1 1
    , Hyperedge 2 (V.fromList [1]) 1 2
    , Hyperedge 0 (V.fromList []) 0 3
    ]
  ) 
  (S.fromList [2])

exampleBigAutomaton = Automaton
  (EdgeList 
    (S.fromList [0,1,2,3]) 
    [ Hyperedge 3 (V.fromList [0,1]) 2 0
    , Hyperedge 3 (V.fromList [1,2]) 2 0
    , Hyperedge 3 (V.fromList [0,0]) 2 0
    , Hyperedge 3 (V.fromList [1,1]) 2 0
    , Hyperedge 3 (V.fromList [2,1]) 2 0
    , Hyperedge 3 (V.fromList [0,2]) 2 0
    , Hyperedge 3 (V.fromList [2,0]) 2 0
    , Hyperedge 3 (V.fromList [2,3]) 2 0
    , Hyperedge 3 (V.fromList [3,2]) 2 0
    , Hyperedge 3 (V.fromList [0,3]) 2 0
    , Hyperedge 3 (V.fromList [3,0]) 2 0
    , Hyperedge 3 (V.fromList [1,3]) 2 0
    , Hyperedge 3 (V.fromList [3,1]) 2 0
    , Hyperedge 3 (V.fromList [3,3]) 2 0
    , Hyperedge 2 (V.fromList [2,2]) 2 0
    , Hyperedge 2 (V.fromList [1,0]) 2 1
    , Hyperedge 1 (V.fromList []) 1 2
    , Hyperedge 0 (V.fromList []) 0 3
    ]
  ) 
  (S.fromList [2])

exampleBigAutomaton2 = Automaton
  (EdgeList 
    (S.fromList [0,1,2]) 
    [ Hyperedge 0 (V.fromList [0,0]) 2 0
    , Hyperedge 0 (V.fromList [0,1]) 2 0
    , Hyperedge 0 (V.fromList [0,2]) 2 0
    , Hyperedge 2 (V.fromList [1,0]) 2 0
    , Hyperedge 2 (V.fromList [1,1]) 2 0
    , Hyperedge 0 (V.fromList [1,2]) 2 0
    , Hyperedge 0 (V.fromList [2,0]) 2 0
    , Hyperedge 0 (V.fromList [2,1]) 2 0
    , Hyperedge 2 (V.fromList [2,2]) 2 0
    , Hyperedge 1 (V.fromList []) 1 2
    , Hyperedge 0 (V.fromList []) 0 3
    ]
  ) 
  (S.fromList [2])


exampleTree 0 = Node 0 []
exampleTree n = Node 1 [exampleTree (n-1)]