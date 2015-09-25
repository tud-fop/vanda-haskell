module Vanda.Grammar.PCFG.Functions where

import Vanda.Grammar.PCFG.PCFG
import Vanda.Grammar.PCFG.Util
import Vanda.Hypergraph.Basic
import Vanda.Hypergraph
import Vanda.Algorithms.ExpectationMaximization
import Vanda.Algorithms.EarleyMonadic
import Vanda.Algorithms.Earley.WSA
import Vanda.Features hiding (product)
import Vanda.Corpus.Penn.Text
import Vanda.Corpus.TreeTerm
import Vanda.Corpus.SExpression
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Text.Lazy as T
import Control.Monad.State
import Data.Tree as T
import Debug.Trace


import qualified Control.Error
errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.Grammar.PCFG.Functions" 


-- * Treebank Extraktion
extractPCFG :: [Deriv String String] -> PCFG String String
extractPCFG l = let PCFG p s w = extractPCFG' l in 
                    PCFG p s (VG.convert (normalize (map snd . partition $ edgesEL p) (VG.convert w)))
                    
extractPCFG' :: [Deriv String String] -> PCFG String String
extractPCFG' l = let (edgelist,e) = runState (generateEdges (sentences2edges l) [] (terminals l)) V.empty in
  PCFG (mkHypergraph edgelist) (map (\ (x,y) -> (x,y / (fromIntegral $ length l))) $ generateStartSymbols l) e

generateStartSymbols :: [Deriv String String] -> [(String,Double)]
generateStartSymbols [] = []
generateStartSymbols ((DNode a _):rest) = insert a $ generateStartSymbols rest
generateStartSymbols ((DLeaf a  ):rest) = insert a $ generateStartSymbols rest

insert :: String -> [(String,Double)] -> [(String,Double)]
insert a [] = [(a,1.0)]
insert a ((b,w):rest) 
  | a == b = (b,w+1):rest
  | otherwise = (b,w) : (insert a rest)
  
terminals :: [Deriv String String] -> S.Set String
terminals [] = S.empty
terminals ((DNode a li):rest) = S.union (terminals li) (terminals rest)
terminals ((DLeaf a):rest) = S.insert a (terminals rest)

generateEdges :: [(String,[String])] -> [Hyperedge String [Either Int String] Int] -> S.Set String -> State (V.Vector Double) [Hyperedge String [Either Int String] Int]
generateEdges [] l _ = return l
generateEdges ((to,b):rest) l t = 
  let (frm,lbl) = split b t 0
      (c,id) = contains to frm lbl l in 
    if not c then do
          v <- get
          put (V.snoc v 1)
          generateEdges rest ((mkHyperedge to frm lbl (V.length v)):l) t
          else do
          v <- get
          put (v V.// [(id,(v V.! id) + 1)])
          generateEdges rest l t

split :: [String] -> S.Set String -> Int -> ([String],[Either Int String])
split [] _ _ = ([],[])
split (x:xs) t i
  | S.member x t = let (a,b) = split xs t i in (a,(Right x):b)
  | otherwise = let (a,b) = split xs t (i + 1) in (x:a,(Left i):b)
                    

sentences2edges :: [Deriv String String] -> [(String,[String])]
sentences2edges [] = []
sentences2edges ((DNode a subtrees):rest) = (a,map (root) subtrees) : sentences2edges subtrees ++ sentences2edges rest
sentences2edges ((DLeaf a) : rest) = sentences2edges rest

contains :: String -> [String] -> [Either Int String] -> [Hyperedge String [Either Int String] Int] -> (Bool,Int)
contains a b l [] = (False,0)
contains a b l (c:cs)
  | equals a b l c = (True,ident c)
  | otherwise = contains a b l cs
  
equals :: String -> [String] -> [Either Int String] -> Hyperedge String [Either Int String] Int -> Bool
equals to [] l (Nullary to' label _) = to == to' && label == l
equals to [from] l (Unary to' from' label _) = to == to' && from == from' && label == l
equals to [from1,from2] l (Binary to' from1' from2' label _) = to == to' && from1 == from1' && from2 == from2' && label == l
equals to from l (Hyperedge to' from' label _) = to == to' && (V.fromList from) == from' && label == l
equals _ _ _ _ = False
          
          


-- Schnitt Grammatik + String
-- | Computes the intersection of a PCFG and a Terminal String, using 'earley\''
intersect :: (Ord a, Show a, Ord b, Show b) => PCFG a b -> [b] -> PCFG (Int,a,Int) b
intersect p s = let (el,w) = earley' (productions p) label (fromList 1 s) (map fst $ startsymbols p) in
                    PCFG (mapLabels (mapHEi fst) el) (map (\(x,y) -> ((0,x,(length (s))),y)) $ startsymbols p) (weights p)
                    


-- EM Algorithmus
-- | Trains a PCFG with the EM algorithm on a given corpus and with a maximum number of iterations.
train :: (Eq a, Ord a, Show a, Ord b, Show b) => PCFG a b -> [[b]] -> Int -> PCFG a b 
train pcfg corpus n = let pcfg'@(PCFG prod ss weight) = toSingleSS pcfg in
  fromSingleSS $ PCFG prod ss (VG.convert (forestEM (map snd . partition $ edgesEL prod) (map (intersect' pcfg') corpus) ident (\ _ x -> x <= n) (VG.convert weight)))
  where intersect' :: (Ord a, Show a, Ord b, Show b) => PCFG (a) b -> [b] -> ((Int,a,Int),EdgeList (Int,a,Int) [Either Int b] Int,Double)
        intersect' p s = let p' = intersect p s in
                             (fst . head $ startsymbols p',productions p',1.0)

toSingleSS :: Ord a => PCFG a b -> PCFG (Maybe a) b
toSingleSS (PCFG prod ss weight) =
  PCFG (EdgeList (S.union (S.map Just (nodesEL prod)) (S.singleton Nothing)) (map (mapHE Just) (edgesEL prod) ++ e')) [(Nothing,1.0)] (weight V.++ w')
    where (e',w') = makeEdges ss (length weight)
          makeEdges :: [(a,Double)] -> Int -> ([Hyperedge (Maybe a) [Either Int b] Int],V.Vector Double)
          makeEdges [] _ = ([],V.empty)
          makeEdges ((x,w):rest) i = let (hes,v) = makeEdges rest (i+1) in
            ((mkHyperedge Nothing [Just x] [Left 0] i):hes,V.cons w v)
  
fromSingleSS :: (Ord a) => PCFG (Maybe a) b -> PCFG a b
fromSingleSS (PCFG prod ss weight) = let (hes,ss,w) = fromSingleSS' (edgesEL prod) (V.map Just weight) in
  PCFG (EdgeList (S.map fromJust $ S.filter isJust (nodesEL prod)) hes) ss (V.map fromJust $ V.filter (isJust) w)
  where fromSingleSS' :: [Hyperedge (Maybe a) [Either Int b] Int] -> V.Vector (Maybe Double) -> ([Hyperedge a [Either Int b] Int],[(a,Double)],V.Vector (Maybe Double))
        fromSingleSS' [] w = ([],[],w)
        fromSingleSS' (he:rest) w 
          | isJust (to he) = ((mapHE fromJust he):hes',ss',w)
          | otherwise      = (hes',(fromJust $ from1 he,fromJust $ w V.! (ident he)):ss',w V.// [(ident he,Nothing)])
          where (hes',ss',w') = fromSingleSS' rest w 
            

-- * n best derivations
-- | Computes the n best derivations, using 'bests'.
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