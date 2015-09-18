module Vanda.Algorithms.MATLearner.TreesContexts where

import Data.Tree

import Data.List (find,intercalate)

data Context a = X | CNode a [Context a]


instance Show a => Show (Context a) where
    show X            = "X"
    show (CNode l [] ) = show l
    show (CNode l [t]) = show l ++ show t
    show (CNode l ts ) = show l ++ "(" ++ (intercalate "," $ map show ts) ++ ")"



--use for testing : drawEveryForest $ getSigmaS [(Node 1 []),(Node 2 [])] [(3,2),(4,1),(5,0)]
-- | return set of trees with symbols from the ranked alpabet as root and trees from trees as substrees
getSigmaS :: Eq a => [Tree a] -> [(a,Int)] -> [Tree a]
getSigmaS _     []                  = []
getSigmaS trees ((symbol,arity):xs) = (listMinus [(Node symbol ts) | ts <- chooseWithDuplicates arity trees] trees) ++ (getSigmaS trees xs)


getSigmaSString :: (Eq a,Show a) => [Tree a] -> [(a,Int)] -> [String]
getSigmaSString _     []                  = []
getSigmaSString trees ((symbol,arity):xs) = (map fst (listMinusSnd [(show symbol ++ body (map fst ts) , Node symbol (map snd ts))| ts <- chooseWithDuplicates arity treesTxt] trees)) ++ (getSigmaSString trees xs)
    where treesTxt = zip (zipWith (++) (replicate (length trees) "t") (map show [1..])) trees

          body :: [String] -> String
          body [] = []
          body ts = "(" ++ (intercalate "," ts) ++ ")"


-- |returns all trees that should be mapped
getAllTrees :: Eq a => [Tree a] -> [(a,Int)] -> [Context a]-> [Tree a]
getAllTrees trees xs contexts = [concatTree t c | t <- trees ++ getSigmaS trees xs, c <- contexts]


-- | returns all contexts with depht 1, a symbol from the alphabet at the root and symbols from the list of trees as subtrees
-- | i.e. the contexts are of the form a(t_1,...,X,...t_n)
getContexts :: [Tree a] -> [(a,Int)] -> [Context a]
getContexts _   []                  = []
getContexts trees ((symbol,arity):alphabet)
    |arity == 0      = getContexts trees alphabet
    |True            = [(CNode symbol ts') | ts <- chooseWithDuplicates (arity-1) trees, ts' <- insertContext ts] ++ (getContexts trees alphabet)


-- | return a list of lists with exactly one X in it
-- | [t_1,t_2] --> [[X,t_1,t_2],[t_1,X,t_2],[t_1,t_2,X]]
insertContext :: [Tree a] -> [[Context a]]
insertContext trees = goInsert (map contextify trees)
    where goInsert []     = [[X]]
          goInsert (t:ts) = (X:t:ts):[t:ts' | ts' <- (goInsert ts)]


-- * tree funtions

-- | transform the tree into a list in praefix order
collapsewlr :: Tree a -> [a]
collapsewlr (Node l ts) = l:(concatMap collapsewlr ts)


-- | transform normal tree into context
contextify :: Tree a -> Context a
contextify (Node l ts) = CNode l (map contextify ts) 


-- | insert the tree into the context
concatTree :: Tree a -> Context a -> Tree a
concatTree t X            = t
concatTree t (CNode l ts) = Node l (map (concatTree t) ts)


-- | insert the context into another context
concatContext :: Context a -> Context a -> Context a
concatContext t X            = t
concatContext t (CNode l ts) = CNode l (map (concatContext t) ts)


drawEveryTree :: Show a => Tree a -> IO ()
drawEveryTree tree = putStrLn $ drawTree $ fmap show tree


drawEveryForest :: Show a => Forest a -> IO ()
drawEveryForest trees = putStrLn $ drawForest $ map (fmap show) trees


-- | return all sublists with a given number of elements (with duplicates and order matters)
-- | 2 [1,2] -> [[1,1],[1,2],[2,1],[2,2]]
chooseWithDuplicates :: Int -> [a] -> [[a]]
chooseWithDuplicates 0 _  = [[]]
chooseWithDuplicates _ [] = []
chooseWithDuplicates n xs = [(x:xs') | x <- xs, xs' <- chooseWithDuplicates (n-1) xs]


-- | return all subsets with a given number of elements
-- | 2 [1,2,3] -> [[1,2],[1,3],[2,3]]
choose :: Int -> [a] -> [[a]]
choose 0 _      = [[]]
choose _ []     = []
choose n (x:xs) = [(x:xs') | xs' <- choose (n-1) xs] ++ (choose n xs)


-- | xs - ys
listMinus :: Eq a => [a] -> [a] -> [a]
listMinus [] ys       = []
listMinus (x:xs) ys 
    | x `notElem` ys  = x : (listMinus xs ys)
    | otherwise       = listMinus xs ys


-- | xs - ys
listMinusSnd :: Eq b => [(a,b)] -> [b] -> [(a,b)]
listMinusSnd [] ys       = []
listMinusSnd ((x,y):xs) ys 
    | y `notElem` ys  = (x,y) : (listMinusSnd xs ys)
    | otherwise       = listMinusSnd xs ys


-- | Checks whether a given tree and a ranked alphabet match, potentially returning an errornous symbol and its correct rank. Returns rank -1, if the symbol is not in the alphabet.
checkValidity :: Eq a => Tree a -> [(a,Int)] -> Maybe (a,Int)
checkValidity (Node symbol children) alphabet = 
                                    let rank = go $ lookup symbol alphabet in
                                    if length children == rank 
                                        then 
                                            do 
                                                r <- find (\ x -> x /= Nothing) $ map (flip checkValidity alphabet) children
                                                r' <- r
                                                return r'
                                        else Just (symbol,rank) 
                                    where go (Just r) = r
                                          go (Nothing) = -1