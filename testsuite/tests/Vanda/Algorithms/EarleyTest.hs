module Vanda.Algorithms.EarleyTest (tests) where
import Vanda.Algorithms.Earley

import qualified Control.Error (errorHere)
import           Vanda.Hypergraph
import           Vanda.Algorithms.Earley.WSA

import           Control.Arrow (second)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Tuple (swap)
import qualified Data.Vector.Unboxed as V
import           Test.HUnit


errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.Algorithms.EarleyTest"


tests :: Test
tests = "earley" ~:
            S.fromList (map hyperedgeToTuple $ edges hg_1_1)
        ~=? case earley hg1 id wsa1 0 of
              (vertices, g, weights)
               -> S.fromList
                $ map ( hyperedgeToTuple
                      . mapHEi (second (weights V.!))
                      . mapHE (invertMap vertices M.!)
                      )
                $ edges g


invertMap :: Ord a => M.Map k a -> M.Map a k
invertMap = M.fromListWith err . map swap . M.toList
  where err = errorHere "invertMap" "no bijection"


hyperedgeToTuple :: Hyperedge v l i -> (v, [v], l, i)
hyperedgeToTuple e = (to e, from e, label e, ident e)


hg1 :: EdgeList Int [Either Int Char] String
hg1 = mkHypergraph
  [ mkHyperedge 0 [0, 0]    (map Left [0 .. 1]) "S → SS"
  , mkHyperedge 0 [1, 0, 2] (map Left [0 .. 2]) "S → LRS"
  , mkHyperedge 0 []        []                  "S → ε"
  , mkHyperedge 1 []        [Right '(']         "L → ("
  , mkHyperedge 2 []        [Right ')']         "R → )"
  ]


wsa1 :: WSA Char Char Double
wsa1 = create
  [ Transition '(' 'e' 'e' 0.5
  , Transition ')' 'e' 'o' 0.5
  , Transition '(' 'o' 'o' 0.2
  , Transition ')' 'o' 'e' 0.8
  ]
  [ ('e', 1) ]
  [ ('e', 1) ]


hg_1_1 :: EdgeList (Char, Int, Char) [Either Int Char] (String, Double)
hg_1_1 = mkHypergraph
  [ mkHyperedge e0e [e0e, e0e]      (map Left [0 .. 1]) ("S → SS" , 1.0)
  , mkHyperedge e0o [e0e, e0o]      (map Left [0 .. 1]) ("S → SS" , 1.0)
  , mkHyperedge e0e [e0o, o0e]      (map Left [0 .. 1]) ("S → SS" , 1.0)
  , mkHyperedge e0o [e0o, o0o]      (map Left [0 .. 1]) ("S → SS" , 1.0)
  , mkHyperedge o0e [o0e, e0e]      (map Left [0 .. 1]) ("S → SS" , 1.0)
  , mkHyperedge o0o [o0e, e0o]      (map Left [0 .. 1]) ("S → SS" , 1.0)
  , mkHyperedge o0e [o0o, o0e]      (map Left [0 .. 1]) ("S → SS" , 1.0)
  , mkHyperedge o0o [o0o, o0o]      (map Left [0 .. 1]) ("S → SS" , 1.0)
  , mkHyperedge e0e [e1e, e0o, o2e] (map Left [0 .. 2]) ("S → LRS", 1.0)
  , mkHyperedge e0o [e1e, e0e, e2o] (map Left [0 .. 2]) ("S → LRS", 1.0)
  , mkHyperedge o0e [o1o, o0o, o2e] (map Left [0 .. 2]) ("S → LRS", 1.0)
  , mkHyperedge o0o [o1o, o0e, e2o] (map Left [0 .. 2]) ("S → LRS", 1.0)
  , mkHyperedge e0e []              []                  ("S → ε"  , 1.0)
  , mkHyperedge o0o []              []                  ("S → ε"  , 1.0)
  , mkHyperedge e1e []              [Right '(']         ("L → ("  , 0.5)
  , mkHyperedge e2o []              [Right ')']         ("R → )"  , 0.5)
  , mkHyperedge o1o []              [Right '(']         ("L → ("  , 0.2)
  , mkHyperedge o2e []              [Right ')']         ("R → )"  , 0.8)
  ]
  where
    e0e = ('e',0,'e')
    e0o = ('e',0,'o')
    o0e = ('o',0,'e')
    o0o = ('o',0,'o')
    e1e = ('e',1,'e')
    o1o = ('o',1,'o')
    e2o = ('e',2,'o')
    o2e = ('o',2,'e')
