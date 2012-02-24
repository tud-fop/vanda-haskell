--Autor: Linda Leuschner
--Status: in Bearbeitung
---------------------------
-- Dieses Modul erstellt eine Dot-Datei, aus der mittels dot die graphische Modulübersicht generiert wird
-- Dazu wird die main-Funktion mit dem relativen Pfad des Quellcodeordners aufgerufen. 
-- Die graphische Modulübersicht stellt einen Graphen dar, dessen Knoten die im Quellcodeordner enthaltenen Module sind, 
-- eine gerichtete Kante (Modul A, Modul B) bedeutet, dass Modul A Modul B importiert
--
-- Module, welche sich auf einer Ebene befinden, hängen nur von Modulen der höher liegenden Ebenen ab und werden als zusammengehörig bezeichnet

module Main

where

import Data.Map as M
import System.Directory as Dir
import System.Environment(getArgs)
import System.IO as IO
import Data.List as L
import Data.IORef
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "please enter the name of the sourcefolder."
    [path] ->   do
			myMap <- getFileContents  path
			--Main.printMap $ reduce (generateFileList (return []) path)  (generateMap (return M.empty) path)
               		drawGraph $ reduce $ generateMap path myMap 

-- erstellt aus einer Map eine Dot-Datei
drawGraph:: M.Map T.Text [T.Text] -> IO ()
drawGraph map = do 
		TIO.putStrLn $ addRanks2 map
		let
		    doubleQuote = (\string -> T.concat [(T.pack "\""), string ,(T.pack "\"")])
		    dqList = L.map doubleQuote $ M.keys map               
                    stringsFromMap = L.map getDotNotation $ M.toList map               
		    dotStr = T.concat [ T.pack "digraph G {ranksep=4;\n"
				    , T.pack "node [shape=plaintext, fontsize=30];\n"
				    , T.concat $ L.map (\a -> T.append a $ T.pack "\n") dqList
    				    , T.concat $ L.map (\a -> T.append a $ T.pack "\n") stringsFromMap
				    , addRanks2 map
				    , T.pack "}\n" ]

		-- handle <- openFile "module_overview.dot" WriteMode
		-- hPutStr handle dotStr
		TIO.writeFile "module_overview.dot" dotStr
                -- hClose handle

-- generiert aus einer Map mit beliebigen Listen als Values eine Map, deren Values nur Elemente enthalten, welche auch Keys der Map sind
-- (entfernt Importverhalten der Form 'Data.List wird importiert von Data.Hypergraph')
reduce :: M.Map T.Text [T.Text] -> M.Map T.Text [T.Text]
reduce map = let valueList = L.concat $ M.elems map
             in M.filterWithKey (\key _ -> L.elem key valueList) map


-- erstellt den Teilstring für die Dot-Datei, welcher zusammengehörige Knoten auf einer Ebene platziert
addRanks2 :: M.Map T.Text [T.Text] -> T.Text
addRanks2 map = if M.null map
			then (T.pack "\n") 
			else let (standalone,dep) = M.partition (\list -> (L.length list) == 0) map 
			         dependent = M.map (flip (L.\\) (M.keys standalone)) dep
			      in T.concat[ T.pack "{ rank = same ; ", (dotNotationForRanks $  M.keys standalone), T.pack "}\n" , addRanks2 dependent]


-- generiert aus den Haskell-Dateien des mittels path übergebenen Projekts eine Map, die das Importverhalten beschreibt.
-- ein Element (Key,[Value]) der Map bedeutet dabei, dass das Modul Key von den Modulen in Value importiert wird
-- Das importierende Modul (Value) ist als Map mit Key = Pfad zum Modul, Value = Modulinhalt gespeichert
getFileContents ::IO.FilePath -> IO (M.Map IO.FilePath T.Text)
getFileContents  path = do 
        f <- doesFileExist path
        d <- doesDirectoryExist path
        if (f && ((L.take 3 $ L.reverse path) == "sh."))
           then do
		dataStream <- TIO.readFile path
		return $ M.fromList [(path,dataStream)]		
           else do if d
                    then do
                        conts <- getDirectoryContents path
                        let contents = L.filter (\nextPath ->
							not((L.take 2 $ L.reverse nextPath) == "./")  && 
							not((L.take 3 $ L.reverse nextPath) == "../") && 
							not((L.take 4 $ L.reverse nextPath) == "tig.")
						) 
					$ L.map ((path ++ "/" ) ++ ) conts
                        mapList <- sequence $ L.map getFileContents contents
                        let newMap = M.unions mapList 
                        return newMap 
                    else return M.empty     

--erstellt aus einer Map (key,value) = (Pfad einer Haskelldatei,Dateiinhalt) die Map, die das Importverhalten beschreibt
generateMap:: IO.FilePath -> Map IO.FilePath T.Text -> M.Map T.Text [T.Text] 
generateMap relPath myMap = M.unionsWith (++) $ L.map (contentHandle relPath) (M.toList myMap)

    
--liest aus dem gegebenen .hs-file die imports aus und gibt diese in Form einer Map (key,value) = (importiertes Modul,importierendes Modul) zurück
contentHandle :: IO.FilePath ->  (IO.FilePath,T.Text) -> M.Map T.Text [T.Text]
contentHandle relPath (path,content) = let imports = getImports (T.lines content) False
                             	   	   b = getModuleName relPath path
                               		in 
				   		M.fromList ((b,[]):[(a,[b])|a <- imports])
                      
-- liest aus einem gegebenen content eines .hs-files alle imports aus
-- der boolesche Wert gibt an, ob schon imports gelesen wurden
getImports :: [T.Text] -> Bool -> [T.Text]   
getImports [] _ = []
getImports content False = let  line = L.head content
				moduleName = getModule line
			   in
	                           if (moduleName /= T.empty)
        	                      then (moduleName:(getImports (L.tail content) True))
        	                      else getImports (L.tail content) False
getImports content True = let	line = L.head content
                                moduleName = getModule line
   			  in	
	                       if (moduleName /= T.empty)
   	                          then (moduleName:(getImports (L.tail content) True))
   	                          else if (T.isInfixOf (T.pack "::") line)
                                     then []
                                     else getImports (L.tail content) True
                                 

--wenn die gegebene Zeile line eine import-Deklaration enthält, wird der Name des importierten Moduls zurückgegeben, sonst T.empty
getModule :: T.Text -> T.Text 
getModule line = if (T.isInfixOf (T.pack "import") line) && not(T.isInfixOf (T.pack "--import") line) && not(T.isInfixOf (T.pack "-- import") line)
                        then let nImport = snd $ T.splitAt 6 line
                             in
                             if (T.isInfixOf (T.pack "qualified") line)
                                then let nQualified = snd $ T.splitAt 10 nImport
                                     in fst $ T.breakOn (T.singleton ' ') $ T.stripStart nQualified
                                else fst $ T.breakOn (T.singleton ' ') $ T.stripStart nImport
                        else T.empty

--gibt für einen gegebenen Pfad <path> den Modulnamen zurück, wobei der Prefix <relPath> abgeschnitten wurde
getModuleName :: IO.FilePath -> IO.FilePath -> T.Text
getModuleName relPath path = let stripPath = (\prefix word -> fromJust $ T.stripPrefix prefix word)
				in changeChar '/' '.' $ T.drop 1 $ stripPath (T.pack relPath) $ T.take (T.length (T.pack path) -3) $ T.pack path
         

-- erstellt aus einem Element der Map den Befehl für die Kantendarstellung in dot
getDotNotation :: (T.Text,[T.Text]) -> T.Text
getDotNotation (key,[]) =  T.empty
getDotNotation (key,x:xs) =  T.concat [T.pack "\"" , x , T.pack "\" -> \"" , key , T.pack "\"; \n", getDotNotation (key,xs)] 

-- erstellt aus einer Liste von Modulen, welcher auf einer Ebene liegen sollen, den entsprechenden dot-Befehl   
dotNotationForRanks:: [T.Text] -> T.Text
dotNotationForRanks [] = T.empty
dotNotationForRanks (x:xs) =  T.concat [(T.pack "\"" ), (x) , (T.pack "\"; ") , (dotNotationForRanks xs) ]


-- ersetzt im Text t alle Zeichen c1 durch das Zeichen c2 
changeChar :: Char -> Char -> T.Text -> T.Text
changeChar c1 c2 t = T.replace (T.singleton c1) (T.singleton c2) t 

show :: IO.FilePath -> String
show path =  do
  p <- path
  return p 

printMap :: IO (M.Map String [String]) -> IO () 
printMap myMap = do
        myMap' <-myMap
        IO.putStr $ M.showTree myMap'
