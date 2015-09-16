module Vanda.Algorithms.MATLearner.MATLearner where

import Data.Tree
import Control.Monad.State
import Vanda.Hypergraph.Basic
import qualified Data.Set as S
import qualified Data.Vector as V
import Vanda.Algorithms.MATLearner.TreeAutomaton
import Data.Map hiding (foldr,foldl,map,filter,findIndex,lookup)
import Data.List (nub,elemIndex,find,findIndex,intercalate)
import Data.Maybe
import Vanda.Algorithms.MATLearner.TreesContexts
import Vanda.Algorithms.MATLearner.Util


newtype Corpus = Corpus [Tree String]
data Interactive = Interactive
data InteractiveString = InteractiveString

class Teacher t where 
        isMember :: t -> Tree String -> IO Bool
        conjecture :: (Ord b, Show b) => t -> Automaton b -> IO (Maybe (Tree String))
        getSigma :: t -> IO [(String,Int)]
        
instance Teacher Interactive where
        isMember Interactive baum = do
          putStrLn "Is this tree part of the language?"
          putStrLn $ show' baum
          putStrLn "y/n?"
          answer <- getLine
          return $ answer == "y"
        
        conjecture Interactive automat = do
          putStrLn "Is this Automaton correct?"
          putStrLn $ show automat
          putStrLn "y/n?"
          answer <- getLine
          if answer == "y" then return Nothing
                           else do
                             putStrLn "Please enter a counterexample:" 
                             tree <- getLine
                             return $ Just (parseTree (filter (/= ' ') tree,0))                                          
                             
        getSigma Interactive = return [("sigma",2),("gamma",1),("alpha",0)]
        
        
instance Teacher InteractiveString where
        isMember InteractiveString baum = do
          putStrLn "Is this word part of the language?"
          putStrLn . reverse $ showAsString baum
          putStrLn "y/n?"
          answer <- getLine
          return $ answer == "y"
        
        conjecture InteractiveString automat = do
          putStrLn "Is this Automaton correct?"
          putStrLn $ show automat
          putStrLn "y/n?"
          answer <- getLine
          if answer == "y" then return Nothing
                           else do
                             putStrLn "Please enter a counterexample:"
                             tree <- getLine -- TODO reverse string or output of membership questions is reversed?
                             return $ Just (parseStringToTree (tree,"0"))
                             
        getSigma InteractiveString = return [("a",1),("b",1),("0",0)]
        
instance (Ord a) => Teacher (Automaton a) where
        isMember automat baum = return $ accepts automat baum
        conjecture automat1 automat2 = return $ isEmpty (unite (intersect (complement automat1) automat2) (intersect automat1 (complement automat2)))
        getSigma automat = return $ getAlphabet automat
        
instance Teacher (Corpus) where
        isMember (Corpus corpus) tree = return $ elem tree corpus
        
        conjecture (Corpus []) _ = return Nothing
        conjecture (Corpus (x:xs)) automaton
          | accepts automaton x = conjecture (Corpus xs) automaton
          | otherwise = return $ Just x
        
        getSigma (Corpus []) = return $ []
        getSigma (Corpus (tree : corpus)) = do
          l1 <- extractSymbols tree
          l2 <- getSigma (Corpus corpus)
          return $ makeSet (l1 ++ l2)
              where
                extractSymbols :: Tree String -> IO [(String,Int)]
                extractSymbols (Node x list) = do
                  l <- getSigma (Corpus list)
                  return $ (x, length list) : l
                makeSet :: Eq a => [a] -> [a]
                makeSet [] = []
                makeSet (x : xs)
                  | elem x xs = makeSet xs
                  | otherwise = x : makeSet xs


instance Ord a => Ord (Tree a) where
    (<=) t1 t2 = (collapsewlr t1) <= (collapsewlr t2)--(a <= b) || (foldl (\le (t1,t2) -> le || t1 <= t2) False (zip t1s t2s))

-- * Output

show' :: (Show a) => (Tree a) -> String
show' (Node a []  ) = show a 
show' (Node a [t] ) = show a ++ show' t
show' (Node a list) = show a ++ "(" ++ (intercalate "," $ map show' list) ++ ")"


-- | this function will present strings in the following way:
-- | every cell a new string begins in the line above or some lines below if there is enougth space
-- |     **  **  **                              
-- |    **  **  **  **  **  **  **  **  **  **   
-- |   **  **  **  **  **  **  **  **  **  **    
-- |  **  **  **  **  **  **  **  **  **  **     
-- | ************    **  **  **  **  **  **  ** 
-- | 123456789...
showContexts :: [String] -> [String] -> [String] -> String
showContexts [] contexts output
  |allEmpty contexts  = intercalate "\n" $ reverse $ map reverse $ filter (any (' '/=)) output -- filter to remove uneccessary empty lines at the top
  |True               = showContexts [] (map tail' contexts) (appendChar contexts output)
showContexts (c:cs) contexts output = showContexts cs (map tail' newContexts) (appendChar newContexts output)
                                    where
                                        newContexts = appendContext contexts (c ++ "  ") -- at least two spaces after each word

                                        appendContext :: [String] -> String -> [String]
                                        appendContext [] c = [c]
                                        appendContext (x:xs) c 
                                            | appendable 0 (x:xs) = (x ++ c):xs
                                            | True              = x:(appendContext xs c)

                                        appendable :: Int -> [String] -> Bool
                                        appendable _ [] = True
                                        appendable l (x:xs)
                                            | l < length x = False
                                            | True         = appendable (l+1) xs


allEmpty :: [[a]] -> Bool
allEmpty []      = True
allEmpty ([]:xs) = allEmpty xs
allEmpty _       = False


tail' :: [a] -> [a]
tail' []     = []
tail' (_:xs) = xs


appendChar :: [String] -> [String] -> [String]
appendChar []         []     = []
appendChar []         (x:xs) = (' ':x):(appendChar [] xs)
appendChar ([]:cs)    (x:xs) = (' ':x):(appendChar cs xs)
appendChar ((c:_):cs) (x:xs) = (c  :x):(appendChar cs xs)


showAsString :: Tree String -> String
showAsString (Node a (t:_)) = a ++ showAsString t
showAsString (Node a []) = ""
   

showObservationtable :: ObservationTable -> [(String,Int)] -> String
showObservationtable (OT (s,contexts,mapping)) alphabet = contextsPart ++ "\n" ++ separationLine ++ "\n" ++ sigmaPart ++ "\n" ++ separationLine ++ "\n" ++ sigmaSPart
                    where   sigmaTable = getTable s contexts mapping
                            sigmaTrees = map (show' . fst) sigmaTable
                            sigmaRows = map (showBool . snd) sigmaTable -- observation table(sigmaPart | upper table) as [String] with 1 and 0 instead of True and False

                            sS = getSigmaS s alphabet
                            sigmaSTable = getTable (listMinus (getSigmaS s alphabet) s) contexts mapping -- observation table(sigmaSPart | lower table) without any elements of the upper one
                            sigmaSTrees = map (show' . fst) sigmaSTable
                            sigmaSRows = map (showBool . snd) sigmaSTable


                            maxTreeLength = maximum $ map length sigmaSTrees -- length of longest tree in sigmaS (is at least as long as the longest tree in sigma)
                            separationLine = replicate (maxTreeLength + 3 + (length $ head sigmaRows)) '-' -- +3 for " | "
                            contextsPart = showContexts (map show contexts) [] (map (\_ -> " | " ++ (replicate maxTreeLength ' ')) contexts) -- " | " at the beginning because the string will be reversed in showContexts

                            sigmaPart = intercalate "\n" $ zipWith (\s r -> s ++ " | " ++ r) (map (fillWithSpaces maxTreeLength) sigmaTrees) sigmaRows
                            sigmaSPart = intercalate "\n" $ zipWith (\s r -> s ++ " | " ++ r) (map (fillWithSpaces maxTreeLength) sigmaSTrees) sigmaSRows

                            showBool :: [Bool] -> String
                            showBool []         = ""
                            showBool (True:xs)  = '1':(showBool xs)
                            showBool (False:xs) = '0':(showBool xs) 

-- | fill the string with spaces until it has the given length
fillWithSpaces :: Int -> String -> String
fillWithSpaces n str = str ++ (replicate (n - length str) ' ')

data ObservationTable = OT ([Tree String], -- ^ S
    [Context String], -- ^ C
    (Map (Tree String) Bool)) -- ^ mapping

instance Show ObservationTable where
    show (OT (s,contexts,mapping)) = "OT (" ++ (show $ map contextify s) ++ "," ++ (show contexts) ++ "," ++ (show $ map (\(k,v) -> (contextify k,v)) $ toList mapping) ++ ")"
    

-- * MAT Learner

main' :: Teacher t => t -> Bool -> IO (Automaton Int)
main' teacher withOutput = do
                initState <- initialObs teacher
                evalStateT (learn teacher withOutput) initState

-- | create and fill initial observation table 
initialObs :: Teacher t => t -> IO ObservationTable
initialObs teacher = do
                        sigma <- getSigma teacher
                        let s = take 1 (getAllTrees [] sigma [X]) in do
                            mapping <- updateMapping teacher empty (getAllTrees s sigma [X])
                            return (OT (s,[X],mapping))

-- | check whether obs is consistent and return consitified version (or old version if the table already was consistent)
consistify :: Teacher t => [[Tree String]] -> t -> StateT ObservationTable IO Bool
consistify []           _       = return True -- TODO here output if consistent
consistify ([s1,s2]:xs) teacher = do
    (OT (s,contexts,mapping)) <- get
    if ((obst s1 contexts mapping) == (obst s2 contexts mapping)) -- ^ both trees represent the same state
        then
            do
                sigma <- lift $ getSigma teacher
                consistent <- checkConsistencyContexts teacher s1 s2 (getContexts s sigma)
                if (consistent)
                    then
                        consistify xs teacher
                    else
                        return False

        else
            consistify xs teacher


-- | check whether a given pair of trees in S woth the same row in the observation table is consistent
checkConsistencyContexts
    :: Teacher t 
    => t -- ^ teacher 
    -> Tree String -- ^ first tree
    -> Tree String -- ^ second tree
    -> [Context String] -- ^ contexts for which consistency of s1 and s2 has to be checked
    -> StateT ObservationTable IO Bool
checkConsistencyContexts _       _  _   []    = return True
checkConsistencyContexts teacher s1 s2 (c:cs) = do
    (OT (_,contexts,mapping)) <- get
    consistent <- checkConsistencyOneContext teacher (obst (concatTree s1 c) contexts mapping) (obst (concatTree s2 c) contexts mapping) c contexts
    if consistent
        then
            checkConsistencyContexts teacher s1 s2 cs
        else
            return False


-- | check whether the two given rows are the same and update the observation table if neccessary
checkConsistencyOneContext     
    :: Teacher t 
    => t -- ^ teacher 
    -> [Bool] -- ^ row of s1 inserted into c
    -> [Bool] -- ^ row of s2 inserted into c
    -> Context String -- ^ context to determine the new context in case the table is inconsistent
    -> [Context String] -- ^ contexts to determine the new context in case the table is inconsistent
    -> StateT ObservationTable IO Bool
checkConsistencyOneContext _ [] [] _ _ = return True
checkConsistencyOneContext teacher (x:xs) (y:ys) context (c:cs)
    | x == y = checkConsistencyOneContext teacher xs ys context cs
    | True   = do -- inconsistent!  -- TODO here output if not consistent ???
        (OT (s,contexts,mapping)) <- get
        sigma <- lift $ getSigma teacher
        mapping' <- lift $ updateMapping teacher mapping (getAllTrees s sigma [concatContext context c]) -- we only need to ask for memberships for trees inserted into the new context
        let contexts' = contexts ++ [concatContext context c] in
            put (OT (s,contexts', mapping'))
        return False

-- | check whether Observation Table is closed and return a closed Observation Table
closify :: Teacher t => [Tree String] ->  t -> StateT ObservationTable IO Bool
closify []     _       = return True -- TODO here output if closed
closify (x:xs) teacher = do
    (OT (s,contexts,mapping)) <- get
    if (any ((obst x contexts mapping) == ) (map snd (getTable s contexts mapping))) 
        then
            closify xs teacher
        else
            do -- TODO here output if not closed
                sigma <- lift $ getSigma teacher
                mapping' <- lift $ updateMapping teacher
                                      mapping
                                      (concatMap (\t -> map (\c -> concatTree t c) contexts) -- insert the trees into all possible contexts
                                                 (map (concatTree x) (getContexts (s ++ [x]) sigma))) -- we only need to consider trees in which the new tree occurs
                put (OT (s ++ [x],contexts,mapping'))
                return False



-- | check whether the current ObservationTable represents the correct Automaton and process counterexample
correctify :: Teacher t => t -> StateT ObservationTable IO Bool
correctify teacher = do
                    obs@(OT (s,contexts,mapping)) <- get
                    sigma <- lift $ getSigma teacher
                    let automaton = generateAutomaton obs sigma in
                        do
                            maybeCounterexample <- lift $ conjecture teacher automaton
                            if maybeCounterexample == Nothing
                                then
                                    return True
                                else
                                    do
                                        counterexample <- lift $ checkCE (fromJust maybeCounterexample) mapping sigma automaton -- errors in the counterexample can only occor with an interactive teacher (hopefully)
                                        let mapping' = insert counterexample (not (accepts automaton counterexample)) mapping in -- insert membership for counterexample
                                            do
                                                put(OT(s,contexts,mapping'))
                                                x <- extract teacher
                                                            (getTable s contexts mapping') 
                                                            (getTable (listMinus (getSigmaS s sigma) s) contexts mapping') -- Simga(S)/S
                                                            counterexample
                                                (OT (_,_,mapping'')) <- get
                                                lift $ putStrLn "extraction rdy"
                                                mapping''' <- lift $ updateMapping teacher
                                                                      mapping'' 
                                                                      (concatMap (\t -> map (\c -> concatTree t c) contexts) -- insert the trees into all possible contexts
                                                                                 (map (concatTree x) (getContexts (s ++ [x]) sigma)))-- we only need to consider trees in which the new tree occurs
                                                put (OT (s ++ [x],contexts,mapping'''))
                                                return False
                            where
                                checkCE :: Tree String -> Map (Tree String) Bool -> [(String,Int)] -> Automaton Int -> IO (Tree String)
                                checkCE counterexample mapping sigma automaton = if (checkValidity counterexample sigma) /= Nothing
                                                                                    then -- symbols have wrong ranks
                                                                                        do
                                                                                            putStrLn "The counterexample is not a valid tree."
                                                                                            newcounterexample <- conjecture teacher automaton
                                                                                            checkCE (fromJust newcounterexample) mapping sigma automaton
                                                                                    else
                                                                                        if (member counterexample mapping) && (mapping ! counterexample /= (not (accepts automaton counterexample)))
                                                                                            then -- the conjectured automaton behaves correctly for the given counterexample
                                                                                                do
                                                                                                    putStrLn "Membership is already known and this tree is not a counterexample!"
                                                                                                    newcounterexample <- conjecture teacher automaton
                                                                                                    checkCE (fromJust newcounterexample) mapping sigma automaton
                                                                                            else
                                                                                                return counterexample


-- | extract subtree that has to be added to the observation table
extract :: Teacher t => t -> [(Tree String,[Bool])] -> [(Tree String,[Bool])] -> Tree String -> StateT ObservationTable IO (Tree String)
extract teacher s sigmaS counterexample
    |newcounterexample == Nothing = return replacedSubtree -- no new counterexample found
    |True                         = do
                                    (OT (s',contexts,mapping)) <- get
                                    mapping' <- lift $ updateMapping teacher mapping [fromJust newcounterexample]
                                    put(OT(s',contexts,mapping')) -- store membership of new tree in mapping
                                    if (mapping' ! counterexample) /= (mapping' ! (fromJust newcounterexample)) -- isMemberOldCounterexample not eqal isMemberNewCounterexample
                                        then
                                            return replacedSubtree -- new counterexample is no longer a counterexample
                                        else
                                            extract teacher s sigmaS (fromJust newcounterexample)
    where 
        Just (newcounterexample, replacedSubtree) = tryReduce counterexample

        tryReduce :: Tree String -> Maybe (Maybe (Tree String),Tree String)
        tryReduce tree@(Node symbol ts)
            |maybeRowOfs == Nothing = let replacedTs = (map tryReduce ts) -- the current subtree is not in Sigma(S)/S
                                          maybeIndexOfSybtree = findIndex (Nothing /=) replacedTs
                                      in
                                        if (maybeIndexOfSybtree == Nothing)
                                            then
                                                Nothing -- no substree could be reduced, this should not happen
                                            else
                                                let indexOfSybtree = fromJust maybeIndexOfSybtree
                                                    Just (newSubtree,replacedSubtree) = replacedTs !! indexOfSybtree
                                                in
                                                    if (newSubtree == Nothing) -- no s' found so return s 
                                                        then
                                                            Just (Nothing,replacedSubtree)
                                                        else
                                                            Just (Just (Node symbol ((take indexOfSybtree ts) ++ [fromJust newSubtree] ++ (drop (indexOfSybtree+1) ts))),replacedSubtree) -- replace subtree
            |True                   = let rowOfs' = find (\(_,q) -> (snd (fromJust maybeRowOfs) == q)) s in -- the current subtree is in Sigma(S)/S now try to replace it by s'
                                        if (rowOfs' == Nothing)
                                            then
                                                Just (Nothing,tree) -- extracted s from the counterexample
                                            else
                                                Just (Just (fst $ fromJust rowOfs'),tree) -- replace s by s'
                where maybeRowOfs = find (\(stree,_) -> tree == stree) sigmaS

-- | generate an Automaton from a given Observation Table and an ranked alphabet
generateAutomaton :: ObservationTable -> [(String,Int)] -> (Automaton Int)
generateAutomaton (OT (s,contexts,mapping)) sigma = Automaton 
                                                      (EdgeList 
                                                        (S.fromList [0..length rows]) -- ^ all occunring states
                                                        (map (\(qis,q,s) -> Hyperedge (getIndex q) (V.fromList (map getIndex qis)) s 0) boolTransitions) -- extract hyperedges from boolTransitions
                                                      ) 
                                                      (S.fromList (map (getIndex . snd) (filter  (\(_,(q:qs)) -> q) rows))) -- final states start with 1
    where
        rows = nub (getTable s contexts mapping)
        
        boolTransitions = concatMap (\(symbol,arity) -> map (getTransition symbol) (chooseWithDuplicates arity rows)) sigma


        getTransition :: String -> [(Tree String,[Bool])] -> ([[Bool]],[Bool],String)
        getTransition symbol rows = (map snd rows, obst (Node symbol (map fst rows)) contexts mapping, symbol)
        -- | get the number corresponding to the state
        getIndex :: [Bool] -> Int
        getIndex q = let Just i = elemIndex q (map snd rows) in i


-- | main loop in which consistency, closedness and correctness are checked
learn :: Teacher t => t -> Bool -> StateT ObservationTable IO (Automaton Int)
learn teacher withOutput = do 
    obs@(OT (s,contexts,mapping)) <- get
    sigma <- lift $ getSigma teacher
    when withOutput $ lift $ putStrLn $ showObservationtable obs sigma
    consistent <- consistify (choose 2 s) teacher
    if not consistent
        then learn teacher withOutput
        else do 
            closed <- closify (getSigmaS s sigma) teacher
            if not closed
                then learn teacher withOutput
                else do
                    correct <- correctify teacher
                    if correct
                        then do -- automaton accepted programm is finished
                            obs <- get
                            return (generateAutomaton obs sigma)
                        else do
                            learn teacher withOutput

-- * Observation Table functions

-- use for testing : obst (Node 1 []) [X,(CNode 2 [X])] (fromList [((Node 1 []),True),((Node 2 [Node 1 []]), False)])
-- | get row of tree in observation table
obst :: Ord a => Tree a -> [Context a] -> Map (Tree a) Bool -> ([Bool])
obst tree cs mapping = map (\c -> mapping ! (concatTree tree c)) cs

-- | get the filled out table
getTable :: Ord a => [Tree a] -> [Context a] -> Map (Tree a) Bool -> ([(Tree a,[Bool])])
getTable s contexts mapping = zip s (map (\x -> obst x contexts mapping) s)


-- | inserts unknown memberships of given trees  into mapping
updateMapping :: Teacher t => t -> Map (Tree String) Bool -> [Tree String] -> IO (Map (Tree String) Bool)
updateMapping teacher mapping []     = return mapping 
updateMapping teacher mapping (t:ts) = do
                                        if  notMember t mapping
                                            then
                                                do
                                                    member <- isMember teacher t
                                                    updateMapping teacher (insert t member mapping) ts
                                            else
                                                updateMapping teacher mapping ts


-- * other funtions


-- | xs - ys
listMinus :: (Eq a) => [a] -> [a] -> [a]
listMinus [] ys       = []
listMinus (x:xs) ys 
    | x `notElem` ys  = x : (listMinus xs ys)
    | otherwise       = listMinus xs ys