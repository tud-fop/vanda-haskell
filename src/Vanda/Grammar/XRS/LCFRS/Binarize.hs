module Vanda.Grammar.XRS.LCFRS.Binarize where

import qualified Data.Array as A
import           Data.List (sortBy)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Ord (comparing)
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V
import           Text.Printf (printf)

import           Vanda.Hypergraph.IntHypergraph
import qualified Vanda.Hypergraph.Tree as VT
import           Vanda.Corpus.Negra.Text (parseNegra)

import           Vanda.Grammar.XRS.LCFRS
import           Vanda.Grammar.XRS.LCFRS.Extraction
import           Vanda.Grammar.XRS.LCFRS.Evaluation

-- This is what I'm currently working on so this is where the main function stays.

main :: IO ()
main = do
       corpusText' <- TIO.readFile "/home/sjm/programming/LCFRS/tiger_release_aug07.export"
       
       let (pRuleMap, (a_nt, a_t)) = extractRulesFromNegra $ parseNegra corpusText'
       
       putStrLn "\nBest probabilities for epsilon = 0, np = 7:\n"
       
       -- epsilon = 0, np1 = 7, broken np4 = 49
       let printFor nt = printBestIn a_nt a_t (printf "%.4f") $ fromJust $ M.lookup nt pRuleMap
       mapM_ printFor [0,7]
       
       let flatPRuleMap = M.foldl' M.union M.empty pRuleMap
           rulesAndProbs = M.assocs flatPRuleMap
           myLCFRS = getMXRSFromProbabilisticRules rulesAndProbs [0] -- assumptions, assumptions...
           myFeature _ i cs = ((weights myLCFRS) V.! i) * (foldl (*) 1 cs)
           bestDerivs = A.elems $ knuth (rtg $ irtg $ myLCFRS) myFeature
       putStrLn $ unlines
                $ map ( unlines
                      . map (\(Candidate prob deriv) -> (printf "\n%.10f, " prob)
                                                     ++ show (to $ VT.rootLabel $ deriv)
                                                     ++ ":\n"
                                                     ++ (drawTree $ fmap ((A.!) a_nt . to) deriv)
                                                     ++ "\n" 
                                                     ++ (unwords $ V.toList $ sententialFront (irtg myLCFRS) a_nt a_t deriv)
                            )
                      )
                $ take 20
                $ bestDerivs
  where
    printBestIn a_nt a_t s = mapM_ (\(r, c) -> putStrLn $ cut 8 (s c) ++ retranslateRule a_nt a_t r)
                           . take 3
                           . reverse
                           . sortBy (comparing snd)
                           . M.assocs

    -- | Neat 2-dimensional drawing of a tree.
    drawTree :: VT.Tree String -> String
    drawTree  = unlines . draw

    draw :: VT.Tree String -> [String]
    draw n = (VT.rootLabel n) : drawSubTrees (VT.subForest n)
      where
        drawSubTrees [] = []
        drawSubTrees [t] =
            "|" : shift "`- " "   " (draw t)
        drawSubTrees (t:ts) =
            "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts
        shift first other = zipWith (++) (first : repeat other)
