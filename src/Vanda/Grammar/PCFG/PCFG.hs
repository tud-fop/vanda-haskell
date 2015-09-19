module Vanda.Grammar.PCFG.PCFG where

import Vanda.Grammar.PCFG.Util
import Vanda.Hypergraph.Basic
import Vanda.Hypergraph
import Vanda.Features hiding (product)
import Vanda.Corpus.Penn.Simple as C
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S
import Control.Monad.State
import Data.Tree as T
import Debug.Trace



-- Treebank Extraktion

extractFromForest :: [C.Sentence] -> PCFG String
extractFromForest f = let (edgelist,e) = runState (generateEdges (sentences2edges f) []) 0 in
  PCFG (mkHypergraph edgelist) (S.toList . S.fromList $ generateStartSymbols f) (V.fromList $ take e [1..])
  where generateStartSymbols :: [C.Sentence] -> [(String,Double)]
        generateStartSymbols [] = []
        generateStartSymbols ((C.Node a _):rest) = (a,1.0):generateStartSymbols rest
        generateStartSymbols ((C.Leaf a _):rest) = (a,1.0):generateStartSymbols rest
        
        generateEdges :: [(String,[String])] -> [Hyperedge String () Int] -> State Int [Hyperedge String () Int]
        generateEdges [] l = return l
        generateEdges ((a,b):rest) l = 
          let (c,id) = contains a b l in 
            if not c then do
                  i <- get
                  put (i + 1)
                  generateEdges rest ((mkHyperedge a b () i):l)
                 else do
                  -- vektor hochzählen?
                  generateEdges rest l

        sentences2edges :: [C.Sentence] -> [(String,[String])]
        sentences2edges [] = []
        sentences2edges ((C.Node a subtrees):rest) = (a,map ( T.rootLabel . toTree) subtrees) : sentences2edges subtrees ++ sentences2edges rest
        sentences2edges ((C.Leaf a b) : rest) = (a,[b]) : sentences2edges rest
        
        contains :: String -> [String] -> [Hyperedge String () Int] -> (Bool,Int)
        contains a b [] = (False,0)
        contains a b (c:cs)
          | equals a b c = (True,ident c)
          | otherwise = contains a b cs
        
        equals :: String -> [String] -> Hyperedge String () Int -> Bool
        equals to [] (Nullary to' _ _) = to == to' 
        equals to [from] (Unary to' from' _ _) = to == to' && from == from'
        equals to [from1,from2] (Binary to' from1' from2' _ _) = to == to' && from1 == from1' && from2 == from2'
        equals to from (Hyperedge to' from' _ _) = to == to' && (V.fromList from) == from'
        equals _ _ _ = False
          
          


-- Schnitt Grammatik + String

intersect :: PCFG String -> String -> PCFG String
intersect = undefined


-- EM Algorithmus

train :: PCFG String -> [String] -> String
train = undefined


-- n best derivations

--bestDerivations:: PCFG -> Int -> [Derivation String () Int]
bestDerivations pcfg n = map (\x -> (map deriv (bmap M.! x))) (fst $ unzip (startsymbols pcfg))
  where bmap = bests (productions pcfg) (defaultFeature $ weights pcfg) (weights pcfg)


defaultFeature :: V.Vector Double -> Feature () Int Double
defaultFeature v = Feature p f
  where p _ i xs = (v V.! i) * product xs
        f x = V.fromList [x]