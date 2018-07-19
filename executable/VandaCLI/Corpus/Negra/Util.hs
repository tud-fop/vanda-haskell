-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Felix Völker 2018
-- License     :  BSD-style
--
-- Maintainer  :  Felix Völker <Felix.Voelker@tu-dresden.de>
-- Stability   :  unknown
-- Portability :  unknown
-----------------------------------------------------------------------------

module VandaCLI.Corpus.Negra.Util(
    putNegra
)
    where


import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.IO       as T
import           Vanda.Corpus.Negra
import           Vanda.Corpus.Negra.Text



readNegraFile :: FilePath -> IO ()
readNegraFile fl = do
    bar <- T.readFile fl
    mapM_ (T.putStrLn . LT.pack . show) . printNegra . parseNegra $ bar
    -- parseNegra $ T.readFile fl

putNegra :: Negra -> IO()
putNegra corp = mapM_ (T.putStrLn . LT.pack) $ printNegra corp

printNegra :: Negra -> [String]
printNegra negra = concatMap printSentence $ sentences negra

printSentence :: Sentence -> [String]
printSentence senten = (show (sId senten) ++ " " ++ show (sEditorId senten) ++ " " ++ sDate senten ++ " " ++ show (sOriginId senten)) : map printNode (sData senten)

printNode :: SentenceData -> String
printNode (SentenceWord sdw sdpt sdmt sde _ _) = fillUp 24 sdw               ++ fillUp 8 sdpt ++ fillUp 16 sdmt ++ printEdge sde
printNode (SentenceNode sdn sdpt sdmt sde _ _) = fillUp 24 ("#" ++ show sdn) ++ fillUp 8 sdpt ++ fillUp 16 sdmt ++ printEdge sde

printEdge :: Edge -> String
printEdge (Edge a b) = fillUp 7 a ++ show b

fillUp :: Int -> String -> String
fillUp n t = t ++ replicate (n - length t) ' '
