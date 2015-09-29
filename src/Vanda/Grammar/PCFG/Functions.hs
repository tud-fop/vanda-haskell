{-|
Module:      Vanda.Grammar.PCFG.Functions
Description: functions to work with /PCFG/s
Copyright:   (c) Technische Universität Dresden 2015
License:     Redistribution and use in source and binary forms, with
             or without modification, is ONLY permitted for teaching
             purposes at Technische Universität Dresden AND IN
             COORDINATION with the Chair of Foundations of Programming.
Maintainer:  markus.napierkowski@mailbox.tu-dresden.de
Stability:   unknown

This module contains functions to work with PCFGs.
-}

module Vanda.Grammar.PCFG.Functions (extractPCFG, train, intersect, bestDerivations) where

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Vanda.Algorithms.EarleyMonadic
import Vanda.Algorithms.Earley.WSA
import Vanda.Algorithms.ExpectationMaximization
import Vanda.Features hiding (product)
import Vanda.Grammar.PCFG.PCFG
import Vanda.Hypergraph.Basic
import Vanda.Hypergraph

-- * Extraction from Treebank
-- | Extracts a PCFG from a list of derivations, calculating the probability 
-- for a rule by counting their occurences in the corpus.
extractPCFG :: (Eq a, Ord a) => [Deriv a a] -> PCFG a a
extractPCFG l = let PCFG p s w = extractPCFG' l in 
  PCFG p s (VG.convert 
           (normalize (map snd . partition $ edgesEL p) (VG.convert w)))

extractPCFG' :: (Ord a) => [Deriv a a] -> PCFG a a
extractPCFG' l =
  let (edgelist,e) = 
        runState (generateEdges (sentences2edges l) [] (terminals l)) V.empty 
  in
  PCFG (mkHypergraph edgelist) 
       (map (\ (x,y) -> (x,y/(fromIntegral $ length l))) 
         $ generateStartSymbols l) 
       e

generateStartSymbols :: (Eq a) => [Deriv a a] -> [(a,Double)]
generateStartSymbols [] = []
generateStartSymbols ((DNode a _):rest) = insert a $ generateStartSymbols rest
generateStartSymbols ((DLeaf a  ):rest) = insert a $ generateStartSymbols rest

insert :: (Eq a) => a -> [(a,Double)] -> [(a,Double)]
insert a [] = [(a,1.0)]
insert a ((b,w):rest) 
  | a == b = (b,w+1):rest
  | otherwise = (b,w) : (insert a rest)
  
-- | Calculate a list of all terminal symbols to differentiate between 
-- terminal symbols and nonterminal symbols.
terminals :: (Ord a) => [Deriv a a] -> S.Set a
terminals [] = S.empty
terminals ((DNode _ li):rest) = S.union (terminals li) (terminals rest)
terminals ((DLeaf a):rest) = S.insert a (terminals rest)

generateEdges :: (Ord a) 
              => [(a,[a])] 
              -> [Hyperedge a [Either Int a] Int] 
              -> S.Set a 
              -> State (V.Vector Double) 
                       [Hyperedge a [Either Int a] Int]
generateEdges [] l _ = return l
generateEdges ((to',b):rest) l t =
  let (frm,lbl) = split b t 0
      (c,id') = contains to' frm lbl l in 
    if not c then do
          v <- get
          put (V.snoc v 1)
          generateEdges rest ((mkHyperedge to' frm lbl (V.length v)):l) t
          else do
          v <- get
          put (v V.// [(id',(v V.! id') + 1)])
          generateEdges rest l t

-- | Split a derivation into its child-nonterminals and the future label in
-- the hypergraph.
split :: (Ord a) => [a] -> S.Set a -> Int -> ([a],[Either Int a])
split [] _ _ = ([],[])
split (x:xs) t i
  | S.member x t = let (a,b) = split xs t i in (a,(Right x):b)
  | otherwise = let (a,b) = split xs t (i + 1) in (x:a,(Left i):b)
                    

sentences2edges :: [Deriv a a] -> [(a,[a])]
sentences2edges [] = []
sentences2edges ((DNode a subtrees):rest) = 
  (a,map (root) subtrees) : sentences2edges subtrees ++ sentences2edges rest
sentences2edges ((DLeaf _) : rest) = sentences2edges rest

-- | Check if a list of hyperedges already contains a hyperedge, and if it
-- does, return the identifier of the already existing one.
contains :: (Eq a) 
         => a  -- ^ 'to' node of a Hyperedge
         -> [a] -- ^ 'from' list of a Hyperedge
         -> [Either Int a] -- ^ 'label' of a Hyperedge
         -> [Hyperedge a [Either Int a] Int] -- ^ the list of Hyperedges
         -> (Bool,Int)
contains _ _ _ [] = (False,0)
contains a b l (c:cs)
  | equals a b l c = (True,ident c)
  | otherwise = contains a b l cs
  
equals :: (Eq a) 
       => a 
       -> [a] 
       -> [Either Int a] 
       -> Hyperedge a [Either Int a] Int 
       -> Bool
equals to' from' label' he = 
  to' == to he && label' == label he && from' == from he
          
          
          
-- * Intersection between Grammar and String

-- | Computes the intersection of a PCFG and a 
-- Terminal String, using 'earley''
intersect :: (Ord a, Show a, Ord b, Show b) 
          => PCFG a b 
          -> [b] 
          -> PCFG (Int,a,Int) b
intersect p s = 
  let (el,_) = earley' (productions p) label (fromList 1 s) 
        (map fst $ startsymbols p) 
  in
  PCFG (mapLabels (mapHEi fst) el) 
       (map (\(x,y) -> ((0,x,(length (s))),y)) $ startsymbols p) 
       (weights p)
                    


-- * Training probabilities with the EM algorithm

-- | Trains a PCFG with the EM algorithm on a given corpus 
-- and with a maximum number of iterations.
train :: (Eq a, Ord a, Show a, Ord b, Show b) 
      => PCFG a b 
      -> [[b]] 
      -> Int 
      -> PCFG a b 
train pcfg corpus n = let pcfg'@(PCFG prod ss weight') = toSingleSS pcfg in
  fromSingleSS 
  $ PCFG prod
         ss 
         (VG.convert 
           (forestEM (map snd . partition $ edgesEL prod) 
                     (map (intersect' pcfg') corpus) 
                     ident 
                     (\ _ x -> x <= n) 
                     (VG.convert weight')))
  where intersect' :: (Ord a, Show a, Ord b, Show b) 
                   => PCFG (a) b 
                   -> [b] 
                   -> ( (Int,a,Int)
                      , EdgeList (Int,a,Int) [Either Int b] Int
                      , Double)
        intersect' p s = let p' = intersect p s in
                             (fst . head $ startsymbols p',productions p',1.0)
                             
-- | Because 'forestEM' only takes one starting node as input, 
-- any arbitrary PCFG has to be transformed into a PCFG with only one 
-- startsymbol. The type of nonterminals 'a' is replaced by 'Maybe a',
-- a new startsymbol ('Nothing') with weight 1.0 is added and for every former 
-- startsymbol an edge is added from 'Nothing' to this symbol.
toSingleSS :: Ord a => PCFG a b -> PCFG (Maybe a) b
toSingleSS (PCFG prod ss weight') =
  PCFG (EdgeList (S.union (S.map Just (nodesEL prod)) 
                 (S.singleton Nothing)) 
                 (map (mapHE Just) (edgesEL prod) ++ e')) 
       [(Nothing,1.0)] 
       (weight' V.++ w')
    where (e',w') = makeEdges ss (length weight')
          makeEdges :: [(a,Double)] 
                    -> Int 
                    -> ( [Hyperedge (Maybe a) [Either Int b] Int]
                       , V.Vector Double
                       )
          makeEdges [] _ = ([],V.empty)
          makeEdges ((x,w):rest) i = let (hes,v) = makeEdges rest (i+1) in
            ((mkHyperedge Nothing [Just x] [Left 0] i):hes,V.cons w v)

-- | The inverse function to 'toSingleSS'.
fromSingleSS :: (Ord a) => PCFG (Maybe a) b -> PCFG a b
fromSingleSS (PCFG prod _ weight') = 
  let (hes,ss,w) = fromSingleSS' (edgesEL prod) (V.map Just weight') in
    PCFG (EdgeList (S.map fromJust $ S.filter isJust (nodesEL prod)) hes) 
         ss 
         (V.map fromJust $ V.filter (isJust) w)
  where fromSingleSS' :: [Hyperedge (Maybe a) [Either Int b] Int] 
                      -> V.Vector (Maybe Double) 
                      -> ( [Hyperedge a [Either Int b] Int]
                         , [(a,Double)]
                         , V.Vector (Maybe Double)
                         )
        fromSingleSS' [] w = ([],[],w)
        fromSingleSS' (he:rest) w 
          | isJust (to he) = ((mapHE fromJust he):hes',ss',w')
          | otherwise      = 
            ( hes'
            , (fromJust $ from1 he,fromJust $ w V.! (ident he)):ss'
            , w' V.// [(ident he,Nothing)]
            )
          where (hes',ss',w') = fromSingleSS' rest w 
            

-- * Extracting the best derivations from a gramar
-- | Computes the n best derivations, using 'bests'.
bestDerivations :: (Ord nonterminalType, Eq terminalType) 
                => PCFG nonterminalType terminalType 
                -> Int 
                -> [(Deriv nonterminalType terminalType,Double)]
bestDerivations pcfg n = 
  map (\ c -> (extractDerivation $ deriv c, weight c)) candidates
  where 
    candidates = 
      take n . merge 
        $ map (\ (x,y) -> map (scale y) (bmap M.! x)) (startsymbols pcfg)
                          -- weights of derivations have to be scaled by
                          -- the weights of their startsymbol
    bmap = 
      bests (productions pcfg) 
            (defaultFeature $ weights pcfg) 
            (V.singleton 1.0)

            
extractDerivation :: Tree (Hyperedge v [Either Int a] Int) -> Deriv v a
extractDerivation (Node he rest) = DNode (to he) (extractDerivation' he rest)

extractDerivation' :: Hyperedge v [Either Int a] Int 
                   -> [Tree (Hyperedge v [Either Int a] Int)] 
                   -> [Deriv v a]
extractDerivation' he' next =
  map (either (\ x -> extractDerivation $ V.fromList next V.! x) DLeaf) 
      (label he')

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