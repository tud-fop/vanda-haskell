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
import Vanda.Algorithms.MATLearner.Teacher
import Graphics.UI.Gtk hiding (get)


instance Ord a => Ord (Tree a) where
    (<=) t1 t2 = (collapsewlr t1) <= (collapsewlr t2)--(a <= b) || (foldl (\le (t1,t2) -> le || t1 <= t2) False (zip t1s t2s))


data ObservationTable = 
    OT ([Tree String], --  S
    [Context String], --  C
    (Map (Tree String) Bool)) --  mapping


instance Show ObservationTable where
    show (OT (s,contexts,mapping)) = "OT (" ++ (show $ map contextify s) ++ "," ++ (show contexts) ++ "," ++ (show $ map (\(k,v) -> (contextify k,v)) $ toList mapping) ++ ")"
    

data GraphicUserInterface = 
    GUI (Dialog, -- window in which observation table is diplayed
         Table, -- observation table
         HBox,
         Label) -- status

-- main programm initialises interface, here you can choose which teacher to use
matLearner :: IO ()
matLearner = do
    initGUI
    hbox <- vBoxNew True 10
    window <- windowNew
    set window [containerBorderWidth := 10,
                containerChild       := hbox ]


    buttonInteractive <- buttonNew
    set buttonInteractive [buttonLabel := "Interactive Teacher"]

    buttonInteractiveString <- buttonNew
    set buttonInteractiveString [buttonLabel := "Interactive String Teacher"]
    
    buttonAutomaton <- buttonNew
    set buttonAutomaton [buttonLabel := "Automaton Teacher"]

    buttonACorpus <- buttonNew
    set buttonACorpus [buttonLabel := "Corpus Teacher"]

    
    boxPackStart hbox buttonInteractive PackNatural 0
    boxPackStart hbox buttonInteractiveString PackNatural 0
    boxPackStart hbox buttonAutomaton PackNatural 0
    boxPackStart hbox buttonACorpus PackNatural 0



    onClicked buttonInteractive $ main' Interactive True

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI


-- * MAT Learner
-- | initialise dialog for output and call learner
main' :: Teacher t => t -> Bool -> IO ()
main' teacher withOutput = do
                -- output
                
                -- create components
                dialog <- dialogNew
                observationTableOut <- tableNew 0 0 False
                statusOut <- labelNew Nothing
                area <- dialogGetUpper dialog
                -- horizontal growing box ,columns do not have the same width, column distance = 5
                box <- hBoxNew False 5
                boxOT <- hBoxNew False 5
                
                -- change fonts
                font <- fontDescriptionFromString "Courier"
                --widgetModifyFont observationTableOut (Just font)
                widgetModifyFont statusOut (Just font)

                -- place components
                dialogAddButton dialog "Next Step" ResponseOk
                containerAdd area box
                boxPackStart box boxOT PackNatural 0
                boxPackStart boxOT observationTableOut PackNatural 0
                boxPackStart box statusOut PackNatural 0

                -- display components
                widgetShowAll area
                ans <- dialogRun dialog

                -- call learner
                initState <- initialObs teacher
                automaton <- evalStateT (learn teacher withOutput (GUI (dialog,observationTableOut,boxOT,statusOut))) initState
                putStrLn $ show automaton
                widgetDestroy dialog


-- | create and fill initial observation table 
initialObs :: Teacher t => t -> IO ObservationTable
initialObs teacher = do
                        sigma <- getSigma teacher
                        let s = take 1 (getAllTrees [] sigma [X]) in do
                            mapping <- updateMapping teacher empty (getAllTrees s sigma [X])
                            return (OT (s,[X],mapping))


-- | main loop in which consistency, closedness and correctness are checked
learn :: Teacher t => t -> Bool -> GraphicUserInterface -> StateT ObservationTable IO (Automaton Int)
learn teacher withOutput (GUI (dialog,observationTableOut,box,status)) = do 
    obs@(OT (s,contexts,mapping)) <- get
    sigma <- lift $ getSigma teacher
    let (contextsOut,sigmaTreesOut,sigmaSTreesOut,sigmaRowsOut,sigmaSRowsOut) = formatObservationTable obs sigma
        noColor = \x -> (x,Color 0 0 0) in do
        table <- lift $ fillTableWithOT observationTableOut box (map noColor contextsOut,map noColor sigmaTreesOut,map noColor sigmaSTreesOut,map (map noColor) sigmaRowsOut,map (map noColor) sigmaSRowsOut)
        ans <- lift $ dialogRun dialog
        
        consistent <- consistify (choose 2 s) teacher
        let out = (GUI (dialog,table,box,status)) in
            if not consistent
                then learn teacher withOutput out
                else do 
                    closed <- closify (getSigmaS s sigma) teacher
                    if not closed
                        then learn teacher withOutput out
                        else do
                            correct <- correctify teacher
                            if correct
                                then do -- automaton accepted programm is finished
                                    obs <- get
                                    return (generateAutomaton obs sigma)
                                else
                                    learn teacher withOutput out


-- | check whether obs is consistent and return consitified version (or old version if the table already was consistent)
-- | the lists in the first argument must always contain 2 trees so its essentially a list of pairs
-- | the function checks for each of these pairs whether they have the same row and if thats the case whether this pair is consistent
consistify :: Teacher t => [[Tree String]] -> t -> StateT ObservationTable IO Bool
consistify []           _       = return True -- TODO here output if consistent
consistify ([s1,s2]:xs) teacher = do
    (OT (s,contexts,mapping)) <- get
    if ((obst s1 contexts mapping) == (obst s2 contexts mapping)) --  both trees represent the same state
        then do
            sigma <- lift $ getSigma teacher
            consistent <- checkConsistencyContexts teacher s1 s2 (getContexts s sigma)
            if consistent
                then
                    consistify xs teacher
                else
                    return False

        else
            consistify xs teacher


-- | check whether a given pair of trees in S with the same row in the observation table is consistent
-- | insert the two trees into every possible context and check whether they behave in the same way
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
        -- ask for new memberships
        mapping' <- lift $ updateMapping teacher mapping (getAllTrees s sigma [concatContext context c]) -- we only need to ask for memberships for trees inserted into the new context
        put (OT (s,contexts ++ [concatContext context c], mapping'))
        return False


-- | check whether Observation Table is closed and return a closed Observation Table
closify :: Teacher t => [Tree String] ->  t -> StateT ObservationTable IO Bool
closify []     _       = return True -- TODO here output if closed
closify (x:xs) teacher = do
    (OT (s,contexts,mapping)) <- get
    if (any ((obst x contexts mapping) == ) (map snd (getTable s contexts mapping))) 
        then
            closify xs teacher
        else do
            sigma <- lift $ getSigma teacher
            -- output OT not closed

            -- ask for new memberships
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
                    let automaton = generateAutomaton obs sigma in do
                        maybeCounterexample <- lift $ conjecture teacher automaton
                        if maybeCounterexample == Nothing
                            then
                                return True
                            else do
                                counterexample <- lift $ checkCE (fromJust maybeCounterexample) mapping sigma automaton -- errors in the counterexample can only occor with an interactive teacher (hopefully)
                                let mapping' = insert counterexample (not (accepts automaton counterexample)) mapping in do -- insert membership for counterexample
                                    put(OT(s,contexts,mapping'))
                                    x <- extract teacher
                                                (getTable s contexts mapping') 
                                                (getTable (getSigmaS s sigma) contexts mapping') -- Simga(S)/S
                                                counterexample
                                    (OT (_,_,mapping'')) <- get -- get mapping with the new memberships insertet in extract
                                    mapping''' <- lift $ updateMapping teacher
                                                                       mapping'' 
                                                                       (concatMap (\t -> map (\c -> concatTree t c) contexts) -- insert the trees into all possible contexts
                                                                                  (map (concatTree x) (getContexts (s ++ [x]) sigma)))-- we only need to consider trees in which the new tree occurs
                                    put (OT (s ++ [x],contexts,mapping'''))
                                    return False
                            where
                                -- | check whther the given counterexample is a correct tree (check ranks of node labels) and whether it is actually a counterexample and if not ask for a new one
                                    -- TODO right now each time the automaton is printed out aigain
                                checkCE :: Tree String -> Map (Tree String) Bool -> [(String,Int)] -> Automaton Int -> IO (Tree String)
                                checkCE counterexample mapping sigma automaton = if (checkValidity counterexample sigma) /= Nothing
                                                                                    then do -- symbols have wrong ranks
                                                                                        putStrLn "The counterexample is not a valid tree."
                                                                                        newcounterexample <- conjecture teacher automaton
                                                                                        checkCE (fromJust newcounterexample) mapping sigma automaton
                                                                                    else if (member counterexample mapping) && (mapping ! counterexample /= (not (accepts automaton counterexample)))
                                                                                        then do -- the conjectured automaton behaves correctly for the given counterexample
                                                                                            putStrLn "Membership is already known and this tree is not a counterexample!"
                                                                                            newcounterexample <- conjecture teacher automaton
                                                                                            checkCE (fromJust newcounterexample) mapping sigma automaton
                                                                                        else
                                                                                            return counterexample


-- | extract subtree that has to be added to the observation table
extract :: Teacher t => t -> [(Tree String,[Bool])] -> [(Tree String,[Bool])] -> Tree String -> StateT ObservationTable IO (Tree String)
extract teacher s sigmaS counterexample
    |newcounterexample == Nothing = return replacedSubtree -- no new counterexample found
    |otherwise                    = do
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
                                                Nothing -- no substree could be reduced
                                            else
                                                let indexOfSybtree = fromJust maybeIndexOfSybtree
                                                    Just (newSubtree,replacedSubtree) = replacedTs !! indexOfSybtree
                                                in
                                                    if (newSubtree == Nothing) -- no s' found so return s 
                                                        then
                                                            Just (Nothing,replacedSubtree)
                                                        else
                                                            Just (Just (Node symbol ((take indexOfSybtree ts) ++ [fromJust newSubtree] ++ (drop (indexOfSybtree+1) ts))),replacedSubtree) -- replace subtree
            |otherwise              = let rowOfs' = find (\(_,q) -> (snd (fromJust maybeRowOfs) == q)) s in -- the current subtree is in Sigma(S)/S now try to replace it by s'
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
                                                        (S.fromList [0..length rows]) -- all occunring states
                                                        (map (\(qis,q,s) -> Hyperedge (getIndex q) (V.fromList (map getIndex qis)) s 0) boolTransitions) -- extract hyperedges from boolTransitions
                                                      ) 
                                                      (S.fromList (map (getIndex . snd) (filter  (\(_,(q:qs)) -> q) rows))) -- final states start with 1
    where
        rows = rmdups (getTable s contexts mapping)
        
        boolTransitions = concatMap (\(symbol,arity) -> map (getTransition symbol) (chooseWithDuplicates arity rows)) sigma

        -- | remove dublicates but only consider the second component when comparing elements
        rmdups :: Eq b => [(a,b)] -> [(a,b)]
        rmdups []                         = []
        rmdups ((y,x):xs)   
            | any (\(_,x') -> x == x') xs = rmdups xs
            | otherwise                   = (y,x): rmdups xs

        -- | take a symbol and the rows from the observation table and extract all the transitions with this symbol
        getTransition :: String -> [(Tree String,[Bool])] -> ([[Bool]],[Bool],String)
        getTransition symbol rows = (map snd rows, obst (Node symbol (map fst rows)) contexts mapping, symbol)        

        -- | get the number corresponding to the state
        getIndex :: [Bool] -> Int
        getIndex q = let Just i = elemIndex q (map snd rows) in i


-- * Observation Table functions

-- use for testing : obst (Node 1 []) [X,(CNode 2 [X])] (fromList [((Node 1 []),True),((Node 2 [Node 1 []]), False)])
-- | get row for the given tree in observation table
obst :: Ord a => Tree a -> [Context a] -> Map (Tree a) Bool -> ([Bool])
obst tree cs mapping = map (\c -> mapping ! (concatTree tree c)) cs


-- | get the filled out observation table
getTable :: Ord a => [Tree a] -> [Context a] -> Map (Tree a) Bool -> [(Tree a,[Bool])]
getTable s contexts mapping = zip s (map (\x -> obst x contexts mapping) s)


-- | inserts unknown memberships of given trees  into mapping
updateMapping :: Teacher t => t -> Map (Tree String) Bool -> [Tree String] -> IO (Map (Tree String) Bool)
updateMapping teacher mapping []     = return mapping 
updateMapping teacher mapping (t:ts) = do
                                        if notMember t mapping
                                            then do
                                                member <- isMember teacher t
                                                updateMapping teacher (insert t member mapping) ts
                                            else
                                                updateMapping teacher mapping ts


-- * Output

formatObservationTable :: ObservationTable -> [(String,Int)] -> ([String],[String],[String],[[String]],[[String]])
formatObservationTable (OT (s,contexts,mapping)) alphabet = (map show contexts,sigmaTrees,sigmaSTrees,sigmaRows,sigmaSRows)
                    where   sigmaTable = getTable s contexts mapping
                            sigmaTrees = zipWith (\ treeVariable tree -> treeVariable ++ ":=" ++ tree) (zipWith (++) (replicate (length sigmaTable) "t") (map show [1..])) (map (nicerShow . fst) sigmaTable)
                            sigmaRows = map (showBool . snd) sigmaTable -- observation table(sigmaPart | upper table) as [String] with 1 and 0 instead of True and False

                            sS = getSigmaS s alphabet
                            sigmaSTable = getTable sS contexts mapping -- observation table(sigmaSPart | lower table) without any elements of the upper one
                            sigmaSTrees = getSigmaSString s alphabet
                            sigmaSRows = map (showBool . snd) sigmaSTable

                            showBool :: [Bool] -> [String]
                            showBool []         = []
                            showBool (True:xs)  = "1":(showBool xs)
                            showBool (False:xs) = "0":(showBool xs) 



-- | put the labels into the table with the given colors
fillTableWithOT :: Table -> HBox -> ([(String,Color)],[(String,Color)],[(String,Color)],[[(String,Color)]],[[(String,Color)]]) -> IO Table
fillTableWithOT tableOld box (contexts,sigmaTrees,sigmaSTrees,sigmaRows,sigmaSRows) = do
                            widgetDestroy tableOld
                            table <- tableNew (3 + (length (sigmaTrees ++ sigmaSTrees))) (2 + (length contexts)) False
                            --tableResize table (3 + (length (sigmaTrees ++ sigmaSTrees))) (2 + (length contexts))
                            -- insert contexts
                            fillOneDim table (0,2) incH contexts True
                            -- insert sigma trees
                            fillOneDim table (2,0) incV sigmaTrees False
                            -- insert sigma table
                            fillTwoDim table (2,2) incH incV sigmaRows False
                            -- insert sigmaS trees
                            fillOneDim table (3 + (length sigmaTrees),0) incV sigmaSTrees False
                            -- insert sigmaS table
                            fillTwoDim table (3 + (length sigmaTrees),2) incH incV sigmaSRows False


                            boxPackStart box table PackNatural 0
                            widgetShowAll table
                            return table


                        where   -- fill table along f
                                fillOneDim :: Table -> (Int,Int) -> ((Int,Int) -> (Int,Int)) -> [(String,Color)] -> Bool -> IO ()
                                fillOneDim _     _            _ []               _       = return ()
                                fillOneDim table (row,column) f ((txt,color):xs) rotated = do
                                                                                label <- labelNew (Just txt)
                                                                                -- rotate label only used for contexts
                                                                                when rotated (labelSetAngle label 90)
                                                                                -- set color
                                                                                widgetModifyFg label StateNormal color
                                                                                tableAttachDefaults table label column (column + 1) row (row + 1)
                                                                                -- change fonts
                                                                                font <- fontDescriptionFromString "Courier"
                                                                                widgetModifyFont label (Just font)

                                                                                fillOneDim table (f (row,column)) f xs rotated

                                -- fill table along f and g
                                fillTwoDim :: Table -> (Int,Int) -> ((Int,Int) -> (Int,Int)) -> ((Int,Int) -> (Int,Int)) -> [[(String,Color)]] -> Bool -> IO ()
                                fillTwoDim _     _    _ _ []     _       = return ()
                                fillTwoDim table cell f g (x:xs) rotated = do
                                                                    fillOneDim table cell f x rotated
                                                                    fillTwoDim table (g cell) f g xs rotated

                                -- increase vertically
                                incV :: (Int,Int) -> (Int,Int)
                                incV (x,y) = (x+1,y)

                                -- increase horizontally
                                incH :: (Int,Int) -> (Int,Int)
                                incH (x,y) = (x,y+1)
