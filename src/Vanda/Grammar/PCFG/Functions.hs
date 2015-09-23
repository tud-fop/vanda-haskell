module Vanda.Grammar.PCFG.Functions where

import Vanda.Grammar.PCFG.PCFG
import Vanda.Grammar.PCFG.Util
import Vanda.Hypergraph.Basic
import Vanda.Hypergraph
import Vanda.Features hiding (product)
import Vanda.Corpus.Penn.Simple as C
import Vanda.Corpus.Penn.Text
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Text.Lazy as T
import Control.Monad.State
import Data.Tree as T
import Debug.Trace


import qualified Control.Error
errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.Grammar.PCFG.Functions" 

-- Treebank Extraktion
{-
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
        equals _ _ _ = False-}
          
          


-- Schnitt Grammatik + String

intersect :: PCFG String String -> String -> PCFG String String
intersect = undefined


-- EM Algorithmus

train :: PCFG String String -> [String] -> String
train = undefined


-- n best derivations
data Deriv a b = DNode a [Deriv a b] | DLeaf b deriving Show

bestDerivsAsString :: (PennFamily a, Ord a) => PCFG a a -> Int -> String
bestDerivsAsString p n = T.unpack . unparsePenn . map (derivToTree . fst) $ bestDerivations p n

derivToTree :: Deriv a a -> Tree a
derivToTree (DLeaf x) = T.Node x []
derivToTree (DNode x l) = T.Node x (map derivToTree l)

bestDerivations :: (Ord nonterminalType, Eq terminalType) => PCFG nonterminalType terminalType -> Int -> [(Deriv nonterminalType terminalType,Double)]
bestDerivations pcfg n = map (\ c -> (extractDerivation $ deriv c, weight c)) candidates
  where candidates = take n . merge $ map (\ (x,y) -> map (scale y) (bmap M.! x)) (startsymbols pcfg)
        bmap = bests (productions pcfg) (defaultFeature $ weights pcfg) (V.singleton 1.0)

extractDerivation :: Tree (Hyperedge v [Either Int a] Int) -> Deriv v a
extractDerivation (T.Node he rest) = DNode (to he) (zipHE he rest)
  where zipHE :: Hyperedge v [Either Int a] Int -> [Tree (Hyperedge v [Either Int a] Int)] -> [Deriv v a]
        zipHE (Nullary _ label _) _ = map (either (errorHere "extractDerivation" "Nullary edge has invalid label") DLeaf) label
        zipHE (Unary _ _ label _) [next] = map (either (\ _ -> extractDerivation next) DLeaf) label
        zipHE (Binary _ _ _ label _) next = map (either (\ x -> extractDerivation $ V.fromList next V.! (x-1)) DLeaf) label
        zipHE (Hyperedge _ _ label _) next = map (either (\ x -> extractDerivation $ V.fromList next V.! (x-1)) DLeaf) label
        zipHE _ _ = errorHere "extractDerivation" "Pattern not matched"

scale :: Double -> Candidate v l i x -> Candidate v l i x
scale d c = c{weight = weight c * d}

merge :: [[Candidate v l i x]] -> [Candidate v l i x]
merge = foldl merge' []

merge' :: [Candidate v l i x] -> [Candidate v l i x] -> [Candidate v l i x]
merge' [] l = l
merge' l [] = l
merge' (c1:r1) (c2:r2)
  | weight c1 >= weight c2 = c1 : (merge' r1 (c2:r2))
  | otherwise              = c2 : (merge' (c1:r1) r2)

defaultFeature :: V.Vector Double -> Feature [Either Int a] Int Double
defaultFeature v = Feature p f
  where p _ i xs = (v V.! i) * product xs
        f x = V.singleton x