{-# LANGUAGE UndecidableInstances, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}
             
module Vanda.Grammar.PCFG.Util (readGrammar, writeGrammar) where

import Vanda.Grammar.PCFG.PCFG
import Vanda.Hypergraph
import qualified Vanda.Hypergraph.IntHypergraph as IH
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Array as A
import Data.Binary
import Control.Monad.State
import System.Directory
import qualified Vanda.Util.Memorysavers as I

import qualified Control.Error
errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.Grammar.PCFG.Util" 

-- Text IO

data ParsedLine = 
  Empty |
  Edge String [String] Double |
  StartSymbol String Double
  

readGrammar :: Bool -> FilePath -> IO (PCFG String String) 
readGrammar bin
  | bin         = readGrammarBinary
  | otherwise   = readGrammarText
  
  

writeGrammar :: (Ord a, Show a, Ord b, Show b, Binary a, Binary b) => Bool -> FilePath -> PCFG a b -> IO ()
writeGrammar bin
  | bin         = writeGrammarBinary
  | otherwise   = writeGrammarText
  
  
-- | Reads a PCFG in the given text format from a file.
readGrammarText :: FilePath -> IO (PCFG String String)
readGrammarText file = do 
  input <- readFile file
  return $ readGrammar' input

readGrammar' :: String -> PCFG String String
readGrammar' s = let (hyperedges,ss,nonterminals) = collect $ map parseLines ( zip (map words (lines s)) [1..]) 
                     hyperedges' = map (\ (from',to',weight,i) -> (mkHyperedge from' (filter ((flip S.member) nonterminals) to') (makeLabel nonterminals to' 0) i , weight)) hyperedges
                     (edges',weights') = unzip hyperedges' in 
                     PCFG (EdgeList nonterminals edges') ss (V.fromList $ reverse weights')
                      
parseLines :: ([String],Int) -> ParsedLine
parseLines (list,line) 
  | length list == 0 = Empty
  | length list == 1 = parsingError line "Lines must either be empty or contain a production / a start symbol."
  | length list == 2 = parseStartSymbol list line
  | otherwise        = parseEdge list line

parseStartSymbol :: [String] -> Int -> ParsedLine
parseStartSymbol [symbol,weight] line = StartSymbol symbol (readDouble line weight)
parseStartSymbol _ line = parsingError line "This should never happen."

parseEdge :: [String] -> Int -> ParsedLine
parseEdge (from' : "->" : to') line = Edge from' (take ((length to') - 1) to') (readDouble line $ last to')
parseEdge _ line = parsingError line "\"->\" missing."

readDouble :: Int -> String -> Double
readDouble line value = case reads value of
  [(val, "")] -> val
  _           -> parsingError line "weight missing."

makeLabel :: S.Set String -> [String] -> Int -> [Either Int String]
makeLabel nonterminals (string:rest) current
  | S.member string nonterminals = (Left current) : (makeLabel nonterminals rest (current + 1))
  | otherwise = (Right string) : (makeLabel nonterminals rest current)
makeLabel _ [] _ = []  

-- | Separate edges and start symbols as they can occur in an arbitrary order in the input file and determine the set of nonterminals.
collect :: [ParsedLine] -> ([(String,[String],Double,Int)],[(String,Double)],S.Set String)
collect [] = ([],[],S.empty)
collect ((Edge from' to' weight):rest) = 
  let (edges',ss,nonterminals) = collect rest in
      ((from',to',weight,(length edges')):edges',ss,S.insert from' nonterminals)
collect ((StartSymbol symbol weight):rest) = 
  let (edges',ss,nonterminals) = collect rest in
      (edges', (symbol,weight):ss,S.insert symbol nonterminals)
collect (Empty : rest) = collect rest
          
          
-- | General function to output a parse error with a message.
parsingError :: Int -> String -> a
parsingError line message = errorHere "readGrammar" $ "Parsing Error in Line " ++ show line ++ " - " ++ message
        
-- | Writes a PCFG in the given text format to a file.
writeGrammarText :: (ToString nonterminalType terminalType) => FilePath -> PCFG nonterminalType terminalType -> IO ()
writeGrammarText file grammar = do 
  writeFile file (grammar2string $ toStringPCFG grammar)

grammar2string :: PCFG String String -> String
grammar2string pcfg = 
  startsymbols2string (startsymbols pcfg) (3 + (maxLength 0 (startsymbols pcfg))) ++
  "\n" ++
  edges2string  (map transform (edgesEL (productions pcfg))) (weights pcfg)

-- | Find the maximum length of a startsymbol (to display weights in the same column)
maxLength :: Int -> [(String,Double)] -> Int
maxLength c ((s,_):rest) 
  | c <= length s = maxLength (length s) rest
  | otherwise     = maxLength c rest
maxLength c [] = c

startsymbols2string :: [(String,Double)] -> Int -> String
startsymbols2string [] _ = ""
startsymbols2string ((s,d):rest) i = s ++ replicate (i-length s) (' ') ++ show d ++ "\n" ++ startsymbols2string rest i

edges2string :: [Hyperedge String () Int] -> V.Vector Double -> String
edges2string h weights' = concatMap (edges2string' (2 + arrowPos 0 h) weights') (sortEdges h)

edges2string' :: Int -> V.Vector Double -> [Hyperedge String () Int] -> String
edges2string' arrowpos weights' hs = let weightpos = 2 + weightPos 0 hs in 
    edges2string'' arrowpos weightpos weights' hs

edges2string'' :: Int -> Int -> V.Vector Double -> [Hyperedge String () Int] -> String 
edges2string'' arrowpos weightpos weights' (h:rest) = 
  to h ++ replicate (arrowpos - length (to h)) (' ') ++ "-> " 
  ++ edges2string''' weightpos weights' h ++
  edges2string'' arrowpos weightpos weights' (rest)
edges2string'' _ _ _ [] = ""

edges2string''' :: Int -> V.Vector Double -> Hyperedge String () Int -> String
edges2string''' weightpos v he = L.intercalate " " (from he) ++ replicate (weightpos - (length (L.intercalate " " (from he)))) (' ') ++ show (v V.! (ident he)) ++ "\n"

-- | Combine a hyperedge with its label, to assemble the right hand side of the rule.
transform :: Hyperedge String [Either Int String] Int -> Hyperedge String () Int
transform h = mkHyperedge (to h) (zipHE h) () (ident h)
  where zipHE :: Hyperedge String [Either Int String] Int -> [String]
        zipHE he = map (either (\ x -> V.fromList (from he) V.! x) id) (label he)

-- | Sort the hyperedges by their left hand side, so same left hand sides are printed as a block.
sortEdges :: [Hyperedge String () Int] -> [[Hyperedge String () Int]]
sortEdges [] = []
sortEdges (h:rest) = insert h (sortEdges rest)

insert :: Hyperedge String () Int -> [[Hyperedge String () Int]] -> [[Hyperedge String () Int]]
insert h [] = [[h]]
insert h ((h':rest'):rest)
  | to h == to h' = ((h:h':rest'):rest)
  | otherwise = (h':rest'):(insert h rest)
insert _ ([] : _) = errorHere "insert" "Empty sublist found."

-- | Find the maximum length of a left hand side of a rule (to display arrows in the same column)
arrowPos :: Int -> [Hyperedge String () Int] -> Int
arrowPos c [] = c
arrowPos c (h:rest)
  | c <= (length $ to h) = arrowPos (length $ to h) rest
  | otherwise = arrowPos c rest
  
-- | Find the maximu m length of a right hand side of a rule (to display weights in the same column)
weightPos :: Int -> [Hyperedge String () Int] -> Int
weightPos c [] = c
weightPos c (he:rest)
  | c <= length (L.intercalate " " (from he)) = weightPos (length (L.intercalate " " (from he))) rest
   
-- | This class is used to display PCFGs with strings as their (non-)terminal type without quotation marks around the symbols
class (ToString a b) where
  toStringPCFG :: PCFG a b -> PCFG String String

instance {-# INCOHERENT #-} ToString String String where
  toStringPCFG = id  
  
instance {-# INCOHERENT #-} Show a => ToString String a where
  toStringPCFG g = PCFG (mapLabels showLabel (productions g)) (startsymbols g) (weights g)
  
instance {-# INCOHERENT #-} Show a => ToString a String where
  toStringPCFG g = PCFG (EdgeList (S.map show (nodesEL $ productions g)) (map (mapHE show) (edgesEL $ productions g))) (map (\ (x,y) -> (show x,y)) $ startsymbols g) (weights g)

instance {-# INCOHERENT #-} (Show a, Show b) => ToString a b where
  toStringPCFG g = PCFG (mapLabels showLabel (EdgeList (S.map show (nodesEL $ productions g)) (map (mapHE show) (edgesEL $ productions g)))) (map (\ (x,y) -> (show x,y)) $ startsymbols g) (weights g)
         
showLabel :: Show a => Hyperedge v [Either Int a] i -> Hyperedge v [Either Int String] i
showLabel h = h{label = map (either (Left) (Right . show)) (label h)} 


-- Binary IO     

startsymbolPath :: [Char]
weightsPath :: [Char]
hypergraphPath :: [Char]
mappingsPath :: [Char]
startsymbolPath = "startsymbols"
weightsPath     = "weights"
hypergraphPath  = "graph"
mappingsPath    = "mappings"

readGrammarBinary :: (Ord a, Binary a, Binary b) => FilePath -> IO (PCFG a b)
readGrammarBinary fp = do
  ss <- decodeFile (fp ++ "/" ++ startsymbolPath)
  w <- decodeFile (fp ++ "/" ++ weightsPath)
  (m1,m2) <- decodeFile (fp ++ "/" ++ mappingsPath)
  hg <- decodeFile (fp++"/" ++ hypergraphPath)
  return $ PCFG (EdgeList (S.fromList $ A.elems m1) (restoreEL m1 m2 (IH.edges hg))) (map (restoreSS m1) ss) (V.fromList w)
  where restoreSS :: A.Array Int a -> (Int,Double) -> (a,Double)
        restoreSS m (x,y) = (m A.! x,y)
        
        restoreEL  :: A.Array Int a -> A.Array Int b -> [IH.Hyperedge [Either Int Int] Int] -> [Hyperedge a [Either Int b] Int]
        restoreEL _ _ [] = [] 
        restoreEL m1 m2 (he:rest) = 
          (mkHyperedge (m1 A.! (IH.to he)) (map (m1 A.!) (IH.from he)) (restoreLabel m2 (IH.label he)) (IH.ident he)):(restoreEL m1 m2 rest)
          
        restoreLabel :: A.Array Int a -> [Either Int Int] -> [Either Int a]
        restoreLabel _ [] = []
        restoreLabel m2 ((Left i):rest) = (Left i) : (restoreLabel m2 rest)
        restoreLabel m2 ((Right i):rest) = (Right (m2 A.! i)) : (restoreLabel m2 rest)

writeGrammarBinary :: (Ord a, Ord b, Binary a, Binary b, ToString a b, Show a, Show b) => FilePath -> PCFG a b -> IO ()
writeGrammarBinary fp g = let (hg,ss,(m1,m2)) = intifyGrammar (toStringPCFG g) in do
  createDirectoryIfMissing True fp
  encodeFile (fp++"/" ++ startsymbolPath) ss
  encodeFile (fp++"/" ++ weightsPath) $ V.toList (weights g)
  encodeFile (fp++"/" ++ mappingsPath) (I.invertMap m1,I.invertMap m2)
  encodeFile (fp++"/" ++ hypergraphPath) hg
  
intifyGrammar :: (Ord a, Ord b) => PCFG a b -> (IH.Hypergraph [Either Int Int] Int,[(Int,Double)],(M.Map a Int, M.Map b Int))
intifyGrammar p = (IH.Hypergraph (M.size m1) el,ss,(m1,m2))
  where (ss,m) = runState (intifySS $ startsymbols p) M.empty
        (el',m1) = runState (intifyEL . edgesEL $ productions p) m
        (el,m2) = runState (intifyEL_lbl el') M.empty
        
intifySS :: (Ord a) => [(a,Double)] -> State (M.Map a Int) [(Int,Double)]
intifySS [] = return []
intifySS ((a,d):rest) = do
  a' <- I.intifyS0 a
  rest' <- intifySS rest
  return $ (a',d) : rest'
  
intifyEL :: (Ord a) => [Hyperedge a [Either Int b] Int] -> State (M.Map a Int) [IH.Hyperedge [Either Int b] Int]
intifyEL [] = return []
intifyEL (he:rest) = do
  to' <- I.intifyS0 (to he)
  from' <- I.intifyS1 (from he)
  rest' <- intifyEL rest
  return $ (IH.mkHyperedge to' from' (label he) (ident he)):rest'

intifyEL_lbl :: (Ord b) => [IH.Hyperedge [Either Int b] Int] -> State (M.Map b Int) [IH.Hyperedge [Either Int Int] Int]
intifyEL_lbl [] = return []
intifyEL_lbl (he:rest0) = do
  l' <- intifyLabel (IH.label he)
  rest' <- intifyEL_lbl rest0
  return $ (IH.mkHyperedge (IH.to he) (IH.from he) l' (IH.ident he)):rest'
  where intifyLabel :: (Ord b) => [Either Int b] -> State (M.Map b Int) [Either Int Int]
        intifyLabel [] = return []
        intifyLabel ((Left i):rest) = do
          rest' <- intifyLabel rest
          return $ (Left i) : rest'
        intifyLabel (Right x:rest) = do
          x' <- I.intifyS0 x
          rest' <- intifyLabel rest
          return $ (Right x') : rest'

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 