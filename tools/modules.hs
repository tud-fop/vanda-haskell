module Main

where

import Data.Map as M
import System.Directory as Dir
import System.Environment(getArgs)
import System.IO as IO
import Data.List as L
import Data.IORef
import Data.Maybe

main :: IO()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "please enter the name of the sourcefolder."
    [path] ->  --Main.printMap $ reduce (generateFileList (return []) path)  (generateMap (return M.empty) path)
               drawGraph (generateFileList (return []) path) $ reduce (generateFileList (return []) path)  (generateMap (return M.empty) path)  


drawGraph:: IO ([String]) -> IO (M.Map String [String]) -> IO ()
drawGraph l m = do
                map <-m
                list <- l
                handle <- openFile "module_overview.dot" WriteMode
                hPutStrLn handle "digraph G {" 
                hPutStrLn handle "ranksep = 4.0;"  
                let dqList = L.map doubleQuote list              
                    stringsFromMap = L.map getDotNotation $ M.toList map 
                --putStrLn $ Prelude.show stringsFromMap               
                sequence_ $ L.map (hPutStrLn handle) dqList 
                sequence_ $ L.map (hPutStrLn handle) stringsFromMap
                addRanks (M.toList map) handle  
                hPutStrLn handle ""
                hPutStrLn handle "}"
--                hPutStrLn handle $ Prelude.show map
                hClose handle


printMap :: IO (M.Map String [String]) -> IO ()
printMap myMap = do
        myMap' <-myMap
        putStr $ Prelude.show myMap'

printList :: IO ([String]) -> IO ()
printList myList = do
        myList' <-myList
        putStr $ Prelude.show myList'


reduce :: IO ([String]) -> IO (M.Map String [String]) -> IO (M.Map String [String])
reduce l m = do
                list <- l
                map <- m
                return $ filterWithKey (\k _ -> elem k list) map


generateFileList :: IO ([String]) -> IO.FilePath -> IO ([String])
generateFileList myList path = do 
        --putStrLn $Prelude.show path
        l <- myList
        f <- doesFileExist path
        ---putStrLn $ Prelude.show f 
        d <- doesDirectoryExist path 
        let path' = Main.show path
        if (f)
          then do if ((chkSubStr path' ".hs") && not(chkSubStr path' ".hs~"))
                        then return [getModuleName path]
                        else return []
          else do if (d && not(chkSubStr path' ".") && not(chkSubStr path' "..") && not(chkSubStr path' ".git"))
                   then do
                        putStrLn $ "checking folder..." ++ path'
                        conts <- getDirectoryContents path
                        let contents = L.map ((path' ++ "/" ) ++ ) conts
                        mapList <- sequence $ L.map (generateFileList myList) contents
                        let newList = L.concat mapList 
                        return newList 
                   else do putStrLn $ "something went wrong while reading: " ++ (Main.show path)
                           return [] 


addRanks :: [(String , [String])] -> Handle -> IO()
addRanks [] handle = putStrLn "ready"
addRanks list handle = do 
                let (standalone, dep) = L.partition (\(key,valueList) -> length valueList == 0) list
                do putStrLn $ "--------standalones: " ++ Prelude.show (L.map fst standalone)
                let dependent = M.toList $ removeFromMap (L.map fst standalone) (M.fromList dep)
                --putStrLn $ Prelude.show standalone
                --putStrLn $ Prelude.show dependent
                hPutStrLn handle $ ("{ rank = same; " ++ (dotNotationForRanks standalone)++ "}") 
                addRanks dependent handle 

removeFromMap :: [String] -> M.Map String [String] ->  M.Map String [String]
removeFromMap [] map = map
removeFromMap (x:xs) map = removeFromMap xs $ M.map (L.filter (\s -> s/=x)) map


generateMap :: IO (M.Map String [String]) -> IO.FilePath -> IO (M.Map String [String])
generateMap myMap path = do 
        --putStrLn $Prelude.show path
        m <- myMap
        f <- doesFileExist path
        --putStrLn $ Prelude.show f 
        d <- doesDirectoryExist path 
        let path' = Main.show path
        if (f)
          then seq (putStrLn "------file--------") $ filehandle True path'
          else do if (d && not(chkSubStr path' ".") && not(chkSubStr path' "..") && not(chkSubStr path' ".git"))
                   then do
                        putStrLn $ "checking folder..." ++ path'
                        conts <- getDirectoryContents path
                        let contents = L.map ((path' ++ "/" ) ++ ) conts
                        mapList <- sequence $ L.map (generateMap myMap) contents
                        let newMap = M.unionsWith (++) mapList 
                        return newMap 
                   else do --putStrLn $ "something went wrong while reading: " ++ (Main.show path)
                           return M.empty     



createExampleMap :: M.Map String [String]
createExampleMap = M.fromList [("module4",["module"]),("module3",["module"]),("module2",["module"]),("module1",["module"])]
            
    
--get the modules imported by the given .hs-file
filehandle ::Bool -> IO.FilePath -> IO (M.Map String [String])
filehandle  bool path = if (chkSubStr path ".hs") && not(chkSubStr path ".hs~") 
                                then do handle <- IO.openFile path IO.ReadMode
                                        imports <-  getImports handle False
                                        let b= [getModuleName $ Main.show path]
                                        return $  M.fromList ((head b,[]):[(a,b)| a<- imports])
                                else return M.empty                        
 
--takes a handle and iterates over the lines of the correspondening file to get the imports     
getImports :: Handle -> Bool -> IO ([String])   -- Boolescher Wert gibt an, ob schon imports gelesen wurden
getImports handle False = do eof <- hIsEOF handle
                             if not eof                       
                                then do line <- hGetLine handle
                                        --putStrLn line
                                        let moduleName = getModule line
                                        if (moduleName /= "")
                                                then do --putStrLn $  "----------False, != emptyString,  Modulname: " ++ moduleName
                                                        importsTrue <- getImports handle True
                                                        return (moduleName:importsTrue)
                                                else do --putStrLn $  "----------False, == emptyString, Modulname: " ++ moduleName
                                                        importsFalse <- getImports handle False
                                                        return importsFalse
                                else return []
getImports handle True = do eof <- hIsEOF handle
                            if not eof
                                then do     line <- hGetLine handle
                                            --putStrLn line
                                            let moduleName = getModule line
                                            if (moduleName /= "")
                                                then do --putStrLn $  "----------True, != emptyString, Modulname: " ++ moduleName
                                                        imports <- getImports handle True
                                                        return (moduleName:imports)
                                                else if (chkSubStr line "::")
                                                        then do --putStrLn $  "----------True, == emptyString, :: vorhanden, Modulname: " ++ moduleName
                                                                return []
                                                        else do --putStrLn $  "----------True, == emptyString, :: nicht vorhanden, Modulname: " ++ moduleName
                                                                imports <- getImports handle True
                                                                return imports 
                                
                                else return []


--checks wether a given line contains a moduleimport, if so, it returns the name of the imported module
getModule :: String -> String 
getModule line = if (chkSubStr line "import") && not(chkSubStr line "--import") && not(chkSubStr line "-- import")
                        then let nImport = snd $ L.splitAt 6 line
                             in
                             if (chkSubStr line "qualified")
                                then let nQualified = snd $ L.splitAt 10 nImport
                                     in fst $ Main.splitAt ' ' $ removeWhitespaces nQualified
                                else fst $ Main.splitAt ' ' $ removeWhitespaces nImport
                        else ""

--returns the modulename of the module described in the file <path>
getModuleName :: IO.FilePath -> String
getModuleName p = changeChar '/' '.' 
                                $  snd (Main.splitAt '/' $ take 
                                                                (length (Main.show p) -3) 
                                                                (Main.show p)) 
         



getDotNotation :: (String,[String]) -> String
getDotNotation (key,[]) = ""
getDotNotation (key,x:xs) =  "\"" ++ x ++ "\" -> \"" ++ key ++ "\"; \n" ++ getDotNotation (key,xs) --getStringOfValues valueList

dotNotationForRanks:: [(String,[String])] -> String
dotNotationForRanks [] = ""
dotNotationForRanks (x:xs) =  "\"" ++ (fst x) ++ "\"; " ++ (dotNotationForRanks xs)

getStringOfValues :: [String] -> String
getStringOfValues [] = ""
getStringOfValues (x:xs) = "\"" ++ x ++ "\"; " ++ getStringOfValues xs



doubleQuote ::String -> String
doubleQuote string =  "\"" ++ string ++ "\""



removeWhitespaces :: [Char] -> [Char]
removeWhitespaces [] = []
removeWhitespaces (' ': xs) = removeWhitespaces xs
removeWhitespaces (x:xs) = (x:xs)


chkSubStr:: String -> [Char] -> Bool
chkSubStr word [] = True 
chkSubStr ""  substr = False
chkSubStr word (x:xs) =or $ L.map (onePartSubStr word xs) $ L.elemIndices x word  --chkSubStr2 word s xs -1

--chkSubStr:: String -> Char -> String -> Int -> Bool --TODO mit obiger Funktion mergen
--chkSubStr:: word '' substring  _ = True
--chkSubStr:: word c substring  -1  = map onePartSubStr word  c  substring $ L.elemIndices c word 
 
onePartSubStr :: String ->  [Char] -> Int -> Bool
onePartSubStr word [] _ = True
onePartSubStr word (x:xs) i = (elem (i+1) $ L.elemIndices x word) && (onePartSubStr word xs (i+1))


changeChar :: Char -> Char -> [Char] -> [Char]
changeChar old new [] = []
changeChar old new word = let   index = fromMaybe (-1) (L.elemIndex old word)
                          in    if (index >= 1)
                                        then (L.take index word) ++ [new] ++ (changeChar old new $ snd $ L.splitAt (index+1) word)
                                        else word

splitAt :: Char -> String -> (String,String)
splitAt c word = let index = fromMaybe (-1) (L.elemIndex c word)
                     a = take index word
                     b = snd (L.splitAt (index+1) word)
                 in  if (index > -1)
                         then (a,b)
                         else (word,"")

splitStr :: [Char] -> Char -> [[Char]]
splitStr "" _  = [[]]
splitStr word a = let   index = fromMaybe (-1) (L.elemIndex a word)
                        subStrs = L.splitAt index word
                  in  if (index >= 1)
                                then ((fst subStrs):(splitStr (snd subStrs) a))
                                else [word]

showMapU:: IO (M.Map String [String]) -> M.Map String [String]
showMapU _ = undefined

show :: IO.FilePath -> String
show path =  do
  p <- path
  return p 
