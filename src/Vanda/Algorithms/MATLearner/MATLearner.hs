module Vanda.Algorithms.MATLearner.MATLearner where

import Data.Tree
import Control.Monad.State
import Vanda.Hypergraph.Basic
import qualified Data.Set as S
import qualified Data.Vector as V
import Vanda.Algorithms.MATLearner.TreeAutomaton
import Data.Map hiding (foldr,foldl,map,filter,findIndex)
import Data.List (nub,elemIndex,find,findIndex)
import Data.Maybe
import Vanda.Algorithms.MATLearner.TreesContexts


newtype Corpus = Corpus [Tree Int]

data Params = AutomatonParams | CorpusParams

class Teacher a where 
        isMember :: a -> Tree Int -> IO Bool
        conjecture :: (Ord b) => a -> Automaton b -> IO (Maybe (Tree Int))
        getSigma :: a -> IO [(Int,Int)]
        
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
                extractSymbols :: Tree Int -> IO [(Int,Int)]
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


data ObservationTable = OT ([Tree Int], -- ^ S
    [Context Int], -- ^ C
    (Map (Tree Int) Bool)) -- ^ mapping

instance Show ObservationTable where
    show (OT (s,contexts,mapping)) = "OT (" ++ (show $ map contextify s) ++ "," ++ (show contexts) ++ "," ++ (show $ map (\(k,v) -> (contextify k,v)) $ toList mapping) ++ ")"

main :: IO ()
main = do
  a <- main' (Corpus [])
  putStrLn $ show a
    
main' :: Teacher t => t -> IO (Automaton Int)
main' teacher = do
                initState <- initialObs teacher
                evalStateT (learn teacher) initState

-- | create and fill initial observation table 
initialObs :: Teacher t => t -> IO ObservationTable
initialObs teacher = do
                        sigma <- getSigma teacher
                        let s = take 1 (getAllTrees [] sigma [X]) in do
                            mapping <- updateMapping teacher empty (getAllTrees s sigma [X])
                            return (OT (s,[X],mapping))

-- | check whether obs is consistent and return consitified version (or old version if the table already was consistent)
consistify :: Teacher t => [[Tree Int]] -> t -> StateT ObservationTable IO Bool
consistify []           _       = return True
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
    -> Tree Int -- ^ first tree
    -> Tree Int -- ^ second tree
    -> [Context Int] -- ^ contexts for which consistency of s1 and s2 has to be checked
    -> StateT ObservationTable IO Bool
checkConsistencyContexts _       _  _   []     = return True
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
    -> Context Int -- ^ context to determine the new context in case the table is inconsistent
    -> [Context Int] -- ^ contexts to determine the new context in case the table is inconsistent
    -> StateT ObservationTable IO Bool
checkConsistencyOneContext _ [] [] _ _ = return True
checkConsistencyOneContext teacher (x:xs) (y:ys) context (c:cs)
    |x==y = checkConsistencyOneContext teacher xs ys context cs
    |True = do -- ^ inconsistent!
        (OT (s,contexts,mapping)) <- get
        sigma <- lift $ getSigma teacher
        mapping' <- lift $ updateMapping teacher mapping (getAllTrees s sigma [concatContext context c]) -- we only need to ask for memberships for trees inserted into the new context
        let contexts' = contexts ++ [concatContext context c] in
            put (OT (s,contexts', mapping'))
        return False

-- | check whether Observation Table is closed and return a closed Observation Table
closify :: Teacher t => [Tree Int] ->  t -> StateT ObservationTable IO Bool
closify []     _       = return True
closify (x:xs) teacher = do
    (OT (s,contexts,mapping)) <- get
    if (any ((obst x contexts mapping) == ) (map snd (getTable s contexts mapping))) 
        then
            closify xs teacher
        else
            do
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
                    counterexample <- lift $ conjecture teacher (generateAutomaton obs sigma)
                    if counterexample == Nothing
                        then
                            return True
                        else
                            do
                                x <- lift $ extract teacher
                                                (getTable s contexts mapping) 
                                                (getTable (listMinus (getSigmaS s sigma) s) contexts mapping) -- ^ Simga(S)/S
                                                (fromJust counterexample)
                                mapping' <- lift $ updateMapping teacher
                                                      mapping 
                                                      (concatMap (\t -> map (\c -> concatTree t c) contexts) -- insert the trees into all possible contexts
                                                                 (map (concatTree x) (getContexts (s ++ [x]) sigma)))-- we only need to consider trees in which the new tree occurs
                                put (OT (s ++ [x],contexts,mapping'))
                                return False


-- | extract subtree that has to be added to the observation table
extract :: Teacher t => t -> [(Tree Int,[Bool])] -> [(Tree Int,[Bool])] -> Tree Int -> IO (Tree Int)
extract teacher s sigmaS counterexample
    |newcounterexample == Nothing                                                     = return replacedSubtree -- no new counterexample found
    |True                         = do
                                    isMemberOld <- isMember teacher counterexample
                                    isMemberNew <- isMember teacher (fromJust newcounterexample)
                                    if isMemberOld /= isMemberNew
                                        then
                                            return replacedSubtree -- new counterexample is no longer a counterexample
                                        else
                                            extract teacher s sigmaS (fromJust newcounterexample)
    where 
        Just (newcounterexample, replacedSubtree) = tryReduce counterexample

        tryReduce :: Tree Int -> Maybe (Maybe (Tree Int),Tree Int)
        tryReduce tree@(Node symbol ts)
            |maybeRowOfs == Nothing = let replacedTs = (map tryReduce ts) -- the current subtree is not in Sigma(S)/S
                                          maybeIndexOfSybtree = findIndex (Nothing /=) replacedTs
                                      in
                                        if (maybeIndexOfSybtree == Nothing)
                                            then
                                                Nothing -- no substree could be reduced
                                            else
                                                let indexOfSybtree = fromJust maybeIndexOfSybtree
                                                    Just (newSubtree,replacedSubtree) = replacedTs !! indexOfSybtree
                                                in
                                                    if (newSubtree == Nothing) -- no s' found so return s 
                                                        then
                                                            Just (Nothing,replacedSubtree)
                                                        else -- TODO check with teacher whether its still a counterexample 
                                                            Just (Just (Node symbol ((take indexOfSybtree ts) ++ [fromJust newSubtree] ++ (drop (indexOfSybtree+1) ts))),replacedSubtree) -- replace subtree
            |True                   = let rowOfs' = find (\(_,q) -> (snd (fromJust maybeRowOfs) == q)) s in -- the current subtree is in Sigma(S)/S now try to replace it by s'
                                        if (rowOfs' == Nothing)
                                            then
                                                Just (Nothing,tree) -- extracted s from the counterexample
                                            else
                                                Just (Just (fst $ fromJust rowOfs'),tree) -- replace s by s'
                where maybeRowOfs = find (\(stree,_) -> tree == stree) sigmaS

-- | generate an Automaton from a given Observation Table and an ranked alphabet
generateAutomaton :: ObservationTable -> [(Int,Int)] -> (Automaton Int)
generateAutomaton (OT (s,contexts,mapping)) sigma = Automaton 
                                                      (EdgeList 
                                                        (S.fromList [0..length rows]) -- ^ all occunring states
                                                        (map (\(qis,q,s) -> Hyperedge (getIndex q) (V.fromList (map getIndex qis)) s 0) boolTransitions) -- extract hyperedges from boolTransitions
                                                      ) 
                                                      (S.fromList (map (getIndex . snd) (filter  (\(_,(q:qs)) -> q) rows))) -- final states start with 1
    where
        rows = nub (getTable s contexts mapping)
        
        boolTransitions = concatMap (\(symbol,arity) -> map (getTransition symbol) (chooseWithDuplicates arity rows)) sigma


        getTransition :: Int -> [(Tree Int,[Bool])] -> ([[Bool]],[Bool],Int)
        getTransition symbol rows = (map snd rows, obst (Node symbol (map fst rows)) contexts mapping, symbol)
        -- | get the number corresponding to the state
        getIndex :: [Bool] -> Int
        getIndex q = let Just i = elemIndex q (map snd rows) in i


-- | main loop in which consistency, closedness and correctness are checked
learn :: Teacher t => t -> StateT ObservationTable IO (Automaton Int)
learn teacher = do 
    obs@(OT (s,contexts,mapping)) <- get
    consistent <- consistify (choose 2 s) teacher
    sigma <- lift $ getSigma teacher
    closed <- closify (getSigmaS s sigma) teacher
    if (not consistent || not closed)
        then learn teacher
        else do
          correct <- correctify teacher
          if correct
            then do
              obs <- get
              return (generateAutomaton obs sigma)
            else do
              learn teacher

-- * Observation Table functions

--use for testing : obst (Node 1 []) [X,(CNode 2 [X])] (fromList [((Node 1 []),True),((Node 2 [Node 1 []]), False)])
-- | get row of tree in observation table
obst :: Tree Int -> [Context Int] -> Map (Tree Int) Bool -> ([Bool])
obst tree cs mapping = map (\c -> mapping ! (concatTree tree c)) cs

-- | get the filled out table
getTable :: [Tree Int] -> [Context Int] -> Map (Tree Int) Bool -> ([(Tree Int,[Bool])])
getTable s contexts mapping = zip s (map (\x -> obst x contexts mapping) s)


-- | inserts unknown memberships of given trees  into mapping
updateMapping :: Teacher a => a -> Map (Tree Int) Bool -> [Tree Int] -> IO (Map (Tree Int) Bool)
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