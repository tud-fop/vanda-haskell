module Main where 

import Data.Either
import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S
import Data.Tree as T
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Ix as Ix
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import Codec.Compression.GZip ( decompress )
import Data.Int ( Int32 )
import Debug.Trace
import Control.DeepSeq
import qualified Data.Text.Lazy as TIO
import qualified Data.Text.Lazy.IO as TIO
import System.Environment ( getArgs, getProgName )


import Vanda.Algorithms.Earley.Earley_String_String as E
import Vanda.Algorithms.Earley.WSA as WSA
import Vanda.Hypergraph.BackwardStar (fromEdgeList)
import Vanda.Features
import Vanda.Token
import Vanda.Hypergraph hiding ( nodes )
import Vanda.Hypergraph.Binary ()
-- import Vanda.Hypergraph.NFData ()
import Vanda.Token


instance NFData (BackwardStar Token ([Either Int Token], [Either Int Token]) Int) where
  rnf (BackwardStar nodes edges memo) 
    = (rnf nodes `seq` rnf edges) `seq` rnf memo 

instance (Show v, Show i, Show l, Show x) => Show (Candidate v l i x) where 
  show c
    = "Gewicht: " ++ (show $ weight c) ++ "\n Ableitung: "
      ++ (show $ deriv c) ++ "\fdata: "
      ++ (show $ fdata c)
      
instance Real (Int, Int) where
  toRational a = toRational $ snd a

instance Num (Int,Int) where
  (+) = (+)
  (*) = (*)
  (-) = (-) 
  abs =  abs 
  signum = signum  
  fromInteger = fromInteger 
  
instance Enum (Int, Int) where
  succ = succ 
  
instance Integral (Int,Int) where
  quotRem a b = quotRem  a  b
  toInteger a = toInteger (snd a)  
  
instance (Show v, Show i, Show l, Ix.Ix v) => Show (EdgeList v l i) where 
  show g 
    = show (S.toList $ nodesEL g) ++ "\n" ++ show (edgesEL  g)
    
instance Show TokenMap where
  show m =  "show " ++
            (show $ toText $ toArray m)
            ++ "showEnd"  
instance (NFData l, VU.Unbox l) => NFData (VU.Vector l) where
  rnf v = rnf $ VU.toList v    

instance NFData (Feature l i x) where
  rnf (Feature pn fin) = rnf pn `deepseq` rnf fin    

instance Eq (Candidate v l i x) where
  (==)  = (==)
  
main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    ["-g", graph, "-w" , weights,  "-t", token, "-d", dir, "-s", input] 
     -> if ((length $ L.lines input) == 0) 
        then error "empty word"
        else do
          hg <- readHg graph
          tok <- tokens token
          -- weights <- readWeights 
          let weights = testWeights (length $ edgesEL $ toEdgeList hg)
          let wghts = weightsToMap weights hg
              word = L.words input
              l = length word 
              component :: (([Either Int Token],[Either Int Token]) -> [Either Int Token])
                = if dir == "lr" then snd else fst 
              direction = if dir == "lr" then True else False
              words = toTokens tok input -- toTokens tok word --[0 .. l-1]      -- words are not yet decoded with
                                                 -- the correct integers
              wsa = toWSA tok input -- :: WSA Int  l v 
              (iGraph,newWeights) = E.earley hg direction wghts wsa 
                                             -- compute the intersection of the 
                                             -- hypergraph and the word.
                                             -- The initial node should  be 0 or
                                             -- changed
              newWeightsVec = toVector newWeights                                     
              pN !_ !i xs = (newWeightsVec VU.! fromIntegral (snd i)) * Prelude.product xs
              feat = feature pN
              -- edgeGraph  = (toEdgeList iGraph)
              best = knuth iGraph feat (V.singleton 1) 
              -- bestAt = M.lookup (0,0,l) best  
              der = makeString tok best (partWords input) component -- if not (bestAt == Nothing) 
                    -- then {- tokToStr tok $-} candToToken (head $ fromJust bestAt) component 
                    -- else error "The word is not derivable."
          -- wsa `deepseq` trace "+++++wsa:++++++" $ traceShow wsa
            -- $ hg `deepseq` trace "Hypergraph gelesen" 
            -- $ weights `deepseq` trace "weights berechnet"
            -- $ trace "wordlength" $ traceShow (length word)
            -- $ trace "word: " traceShow words 
            -- $ iGraph `deepseq` trace "hg berechnet" $ traceShow iGraph
            -- $ newWeights `deepseq` trace "neue Gewichte: " $ traceShow newWeights
            -- $ feat `deepseq` trace "feature berechnet"
            -- $ edgeGraph `deepseq` trace "edgeGraph berechnet" -- $ traceShow edgeGraph
            -- $ trace "---------------------------\n---------------------"
            -- $ traceShow words
          print der
    _ -> print $ "Usage: " ++ progName ++ "-g graphfile -t tokenfile -s sentence"    
     
          
readHg:: String -> IO(EdgeList Token ([Either Int Token], [Either Int Token]) Int)
readHg file = do 
            dec :: EdgeList Token ([Either Int Token], [Either Int Token]) Int
             <- fmap (force . B.decode . decompress) 
                      $ B.readFile file
            return $ force dec  
       
testWeights:: Int -> VU.Vector Double
testWeights n = VU.replicate n 1  
       
weightsToMap:: (Hypergraph h) 
  => VU.Vector Double 
  -> h Token ([Either Int Token],[Either Int Token]) Int 
  -> M.Map Int Double
weightsToMap weights hg 
  = M.fromList $ zip (map ident $ edgesEL $ toEdgeList hg) (VU.toList weights)

readWeights:: String -> IO(VU.Vector Double)
readWeights file = fmap
             (VU.fromList . B.decode . decompress)
             $ B.readFile file
           
partWords:: String -> [String]
partWords = L.words
             
tokens:: String  -> IO(TokenMap)
tokens tokenfile = do 
    tokenm :: TokenMap 
      <- fmap ( force . fromText) $ TIO.readFile tokenfile
    return tokenm
    
toWSA :: TokenMap -> String -> WSA Int Token Double 
toWSA tok input = WSA.fromList 1 $ toTokens tok input

iStartState :: [String] -> (Int, Token, Int)
iStartState word = (0,startNT,length word)
    
featWeight = V.singleton 1

feature ::  (l -> i -> [Double] -> Double) ->  Feature l i Double 
feature pN = (Feature pN V.singleton)

makeString 
  :: TokenMap 
  -> M.Map (Int, Token, Int) 
            [Candidate (Int, Token, Int) 
                ([Either Int Token], [Either Int Token]) i Double] 
  -> [String]
  -> (([Either Int Token],[Either Int Token]) -> [Either Int Token])
  -> String
makeString tok best words component 
  = let bestAt = M.lookup (iStartState words) best
    in if not (bestAt == Nothing) 
       then tokToStr tok $ candToToken (head $ fromJust bestAt) component 
       else error "The word is not derivable."

startNT :: Token
startNT = 0 

toTokens :: TokenMap -> String -> [Token]
toTokens tokenm input 
     = map (getToken tokenm) $ partWords input
    -- return list
           
ex_tokens:: String -> [String] -> IO([Token])
ex_tokens _ input = do 
    let list = map (\x -> read x :: Token) input
    return $ list 
           
toVector:: M.Map (Int,Int) Double -> VU.Vector Double
toVector weights 
  = let extract ((_,id2),w) = (id2,w)
        res = VU.fromList $ map snd $ L.sort $ map extract $ M.toList weights
    in --trace "start toVector" res `deepseq` trace "end toVector" 
        res
        
        
-- candToTree:: Candidate v ([Either Int Token], [Either Int Token]) i x -> (Double, T.Tree v )
-- candToTree cand = undefined

candToToken:: Candidate v ([Either Int Token], [Either Int Token]) i x 
            -> (([Either Int Token],[Either Int Token]) -> [Either Int Token])
            -> [Token]
candToToken cand component 
  = concat $ map (extractTerms . component . label) $ T.flatten $ deriv cand  

extractTerms::  [Either Int Token] -> [Token]
extractTerms list = rights list

tokToStr:: TokenMap -> [Token] -> String
tokToStr tok list = traceShow list 
                    $ traceShow (toMap tok)
                    $ L.unwords $ map (getString (toArray tok)) list 

            -- -> ((v,v) -> v)
            -- -> [Token]
-- candToString cand component = concat $ rights $ component $ T.flatten $ deriv cand
-- 
-- candToString:: Candidate v (T.Tree (Either Int Token), [Either Int Token]) i x -> [Token]


-- concat $ map (extractTerms . label) $ T.flatten $ deriv cand  

-- extractTerms:: ([Either Int Token], [Either Int Token]) -> [Token]
-- extractTerms (tree, _) = rights $ T.flatten tree 


      -- hg <- testHg graph
      -- weights <- testWeights graph -- replace by weights as soon as there are some
      -- tokenmap <- getTokenMap token
      -- let translation = translate tokenMap 
  
