module Vanda.Algorithms.MATLearner.Util where
 
import Data.Tree


parseFile :: FilePath -> (String -> t) -> IO t 
parseFile fp parser = do
  file <- readFile fp
  putStrLn $ "Parsing file " ++ fp
  return $ parser (filter (/= ' ') file)

parseCorpus :: String -> Forest String
parseCorpus s = (map parseTree $ zip (lines s) [1..])

parseTree :: (String,Int) -> Tree String
parseTree ([],line) = parsingError line "No empty Lines allowed."
parseTree (string,line)
  | symbol == "" = parsingError line "Tree Nodes can't be empty."
  | otherwise    = Node symbol (parseSubTree (subTrees,line))
  where (symbol,subTrees) = parseSymbol string line
        parseSymbol :: String -> Int -> (String,String)
        parseSymbol ('"' : s) l = parseSymbol' s l
        parseSymbol _ l = parsingError l "'\"' missing."
        
        parseSymbol' :: String -> Int -> (String, String)
        parseSymbol' ('"': s) _ = ("",s)
        parseSymbol' (c : s) l = let (restSymbol,restString) = parseSymbol' s l in (c: restSymbol,restString)
        parseSymbol' [] l = parsingError l "'\"' missing."
        
        parseSubTree :: (String,Int) -> [Tree String]
        parseSubTree (( '[' : rest),l)
          | rest == "" = parsingError l "']' missing."
          | last rest == ']' = if length rest == 1 then []
                                                   else map parseTree $ (zip (separateTrees $ take ((length rest) - 1) rest) [l,l..])
          | otherwise = parsingError l "']' missing."
        parseSubTree (_,l) = parsingError l "'[' missing."
 
parseStringToTree :: (String,String) -> Tree String
parseStringToTree ([],end) = Node end []
parseStringToTree ((a:rest),end) = Node [a] [parseStringToTree (rest,end)]

separateTrees :: String -> [String]
separateTrees s = separateTrees' 0 "" s
  where separateTrees' :: Int -> String -> String -> [String]
        separateTrees' _ currentWord [] = [currentWord]
        separateTrees' level currentWord (c:rest) 
          | and [(c == ','),(level <= 0)] = (currentWord : separateTrees rest)
          | c == '[' = separateTrees' (level + 1) (currentWord ++ "[") rest
          | c == ']' = separateTrees' (level - 1) (currentWord ++ "]") rest
          | otherwise = separateTrees' level (currentWord ++ [c]) rest
          
parsingError :: Int -> String -> a
parsingError line message = error $ "Parsing Error in Line " ++ show line ++ ": " ++ message