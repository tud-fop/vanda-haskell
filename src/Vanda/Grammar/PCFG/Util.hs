{-# LANGUAGE UndecidableInstances, OverlappingInstances,
             FlexibleInstances, TypeSynonymInstances #-}
             
module Vanda.Grammar.PCFG.Util where

import Vanda.Hypergraph
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.List
data PCFG nonterminalType = 
  PCFG 
  { productions :: EdgeList nonterminalType () Int
  , startsymbols :: [(nonterminalType,Double)]
  , weights :: V.Vector Double
  }  

-- Text IO

data ParsedLine = 
  Empty |
  Edge String [String] Double |
  StartSymbol String Double
  

  
-- | Reads a PCFG in the given text format from a file.
readGrammar :: FilePath -> IO (PCFG String)
readGrammar file = do 
  input <- readFile file
  return $ readGrammar' input
  where readGrammar' :: String -> PCFG String
        readGrammar' s = let (hyperedges,startsymbols) = collect $ map parseLines ( zip (map words (lines s)) [1..]) 
                             (edges,weights) = unzip hyperedges in 
                             PCFG (EdgeList (S.fromList ((nodesLL edges) ++ (fst $ unzip startsymbols))) edges) startsymbols (V.fromList $ reverse weights)
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
        parseEdge (from : "->" : to) line = Edge from (take ((length to) - 1) to) (readDouble line $ last to)
        parseEdge _ line = parsingError line "\"->\" missing."
        
        readDouble :: Int -> String -> Double
        readDouble line value = case reads value of
          [(val, "")] -> val
          _           -> parsingError line "weight has to be a number."
          
        -- | Separate edges and start symbols as they can occur in an arbitrary order in the input file.
        collect :: [ParsedLine] -> ([(Hyperedge String () Int,Double)],[(String,Double)])
        collect [] = ([],[])
        collect ((Edge from to weight):rest) = 
          let (edges,startsymbols) = collect rest in
              ((mkHyperedge from to () ((length edges)),weight):edges,startsymbols)
        collect ((StartSymbol symbol weight):rest) = 
          let (edges,startsymbols) = collect rest in
              (edges, (symbol,weight):startsymbols)
        collect (Empty : rest) = collect rest
          
          
-- | General function to output a parse error with a message.
parsingError :: Int -> String -> a
parsingError line message = error $ "Parsing Error in Line " ++ show line ++ ": " ++ message
        
-- | Writes a PCFG in the  given text format to a file.
writeGrammar :: (ToString nonterminalType) => FilePath -> PCFG nonterminalType -> IO ()
writeGrammar file grammar = do 
  writeFile file (grammar2string $ toStringPCFG grammar)
  where grammar2string :: PCFG String -> String
        grammar2string pcfg = 
          startsymbols2string (startsymbols pcfg) (3 + (maxLength 0 (startsymbols pcfg))) ++
          "\n" ++
          edges2string (edgesEL (productions pcfg)) (weights pcfg)
        
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
        edges2string h weights = concatMap (edges2string' (2 + arrowPos 0 h) weights) (sortEdges h)
        
        edges2string' :: Int -> V.Vector Double -> [Hyperedge String () Int] -> String
        edges2string' arrowpos weights hs = let weightpos = 2 + weightPos 0 hs in 
            edges2string'' arrowpos weightpos weights hs
        
        edges2string'' :: Int -> Int -> V.Vector Double -> [Hyperedge String () Int] -> String 
        edges2string'' arrowpos weightpos weights (h:rest) = 
          to h ++ replicate (arrowpos - length (to h)) (' ') ++ "-> " 
          ++ edges2string''' weightpos weights h ++
          edges2string'' arrowpos weightpos weights (rest)
        edges2string'' _ _ _ [] = ""
        
        edges2string''' :: Int -> V.Vector Double -> Hyperedge String () Int -> String
        edges2string''' weightpos v (Nullary _ _ i) = replicate weightpos (' ') ++ show (v V.! i) ++ "\n"
        edges2string''' weightpos v (Unary _ from _ i) = from ++ replicate (weightpos - length from) (' ') ++ show (v V.! i) ++ "\n"
        edges2string''' weightpos v (Binary _ from1 from2 _ i) = from1 ++ " " ++ from2 ++ replicate (weightpos - (1 + length (from1++from2))) (' ') ++ show (v V.! i) ++ "\n"
        edges2string''' weightpos v (Hyperedge _ from _ i) = intercalate " " (V.toList from) ++ replicate (weightpos - (length (intercalate " " (V.toList from)))) (' ') ++ show (v V.! i) ++ "\n"
        
        -- | Sort the hyperedges by their left hand side, so same left hand sides are printed as a block.
        sortEdges :: [Hyperedge String () Int] -> [[Hyperedge String () Int]]
        sortEdges [] = []
        sortEdges (h:rest) = insert h (sortEdges rest)
        
        insert :: Hyperedge String () Int -> [[Hyperedge String () Int]] -> [[Hyperedge String () Int]]
        insert h [] = [[h]]
        insert h ((h':rest'):rest)
          | to h == to h' = ((h:h':rest'):rest)
          | otherwise = (h':rest'):(insert h rest)
        
        -- | Find the maximum length of a left hand side of a rule (to display arrows in the same column)
        arrowPos :: Int -> [Hyperedge String () Int] -> Int
        arrowPos c [] = c
        arrowPos c (h:rest)
          | c <= (length $ to h) = arrowPos (length $ to h) rest
          | otherwise = arrowPos c rest
          
        -- | Find the maximum length of a right hand side of a rule (to display weights in the same column)
        weightPos :: Int -> [Hyperedge String () Int] -> Int
        weightPos c [] = c
        weightPos c ((Nullary _ _ _):rest) = weightPos c rest
        weightPos c ((Unary _ from _ _):rest)
          | c <= length from = weightPos (length from) rest
          | otherwise = weightPos c rest
        weightPos c ((Binary _ from1 from2 _ _):rest)
          | c <= 1 + length (from1++from2) = weightPos (1 + length (from1++from2)) rest
          | otherwise = weightPos c rest
        weightPos c ((Hyperedge _ from _ _):rest)
          | c <= length (intercalate " " (V.toList from)) = weightPos (length (intercalate " " (V.toList from))) rest
          | otherwise = weightPos c rest
   
-- | This class is used to display strings without their quotation marks
class ToString a where
  toStringPCFG :: PCFG a -> PCFG String
  
instance ToString String where
  toStringPCFG = id

instance Show a => ToString a where
  toStringPCFG g = PCFG (EdgeList (S.map show (nodesEL $ productions g)) (map (mapHE show) (edgesEL $ productions g))) (map (\ (x,y) -> (show x,y)) $ startsymbols g) (weights g)
         

-- Binary IO     
{-
instance Binary PCFG where
  put = undefined
  get = undefined
  -}
 
