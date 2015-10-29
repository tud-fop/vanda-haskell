module Vanda.Algorithms.MATLearner.MATLearner where

import Prelude hiding (lookup)
import Data.Tree
import Control.Monad.State
import Vanda.Hypergraph.Basic
import qualified Data.Set as S
import qualified Data.Vector as V
import Vanda.Algorithms.MATLearner.TreeAutomaton
import Vanda.Algorithms.MATLearner.Strings
import Data.Map hiding (foldr,foldl,map,filter,findIndex)
import Data.List (nub,elemIndex,find,findIndex,intercalate)
import Data.Maybe
import Vanda.Algorithms.MATLearner.TreesContexts
import Vanda.Algorithms.MATLearner.Util
import Vanda.Algorithms.MATLearner.Teacher
import Graphics.UI.Gtk hiding (get)
import System.Exit


instance Ord a => Ord (Tree a) where
    (<=) t1 t2 = (collapsewlr t1) <= (collapsewlr t2)--(a <= b) || (foldl (\le (t1,t2) -> le || t1 <= t2) False (zip t1s t2s))


data ObservationTable = 
    OT ([Tree String], --  S
    [Context String], --  C
    (Map (Tree String) Bool)) --  mapping


data ExtractGUI = 
    Extract (Dialog,-- window in which extraction table is diplayed
             VBox, -- first column
             VBox, -- second column
             VBox) -- third column
    | None

data GraphicUserInterface = 
    GUI (Dialog, -- window in which observation table is diplayed
         Table, -- observation table
         Frame, -- frame which inhabits the table needed because table has to be destroyed everytime a new one is draw (maybe find a better solution for this)
         Table, -- status
         Frame, -- frame which inhabits status table
         ExtractGUI) -- extract

-- main programm initialises interface, here you can choose which teacher to use
matLearner :: IO ()
matLearner = do
    initGUI
    -- create main window
    hbox <- vBoxNew True 10
    window <- windowNew
    set window [windowTitle          := menueTitle,
                containerBorderWidth := 10,
                containerChild       := hbox,
                windowDefaultWidth   := 200,
                windowDefaultHeight  := 100 ]

    -- create teacher buttons
    buttonInteractive <- buttonNew
    set buttonInteractive [buttonLabel := buttonInteractiveText]
    
    buttonAutomaton <- buttonNew
    set buttonAutomaton [buttonLabel := buttonAutomatonText]

    
    buttonAutomatonInt <- buttonNew
    set buttonAutomatonInt [buttonLabel := buttonAutomatonIntText]

    -- put the buttons in the window
    boxPackStart hbox buttonInteractive PackNatural 0
    boxPackStart hbox buttonAutomaton PackNatural 0
    boxPackStart hbox buttonAutomatonInt PackNatural 0


    -- add click events to the buttons
    onClicked buttonInteractive $ main' Interactive

    onClicked buttonAutomaton $ displayFileDialog (\ automat -> main' automat)

    onClicked buttonAutomatonInt $ displayFileDialog (\ automat -> main' (A automat))

    onDestroy window mainQuit
    widgetShowAll window
    mainGUI


-- | display a file dialog parse the automaton in the selected file and apply this automaton to callLearner
displayFileDialog :: (Automaton Int -> IO ()) -> IO ()
displayFileDialog callLearner = do
      -- create file dialog
      dialog <- dialogNew
      set dialog [windowTitle := fileDialogTitle, 
                  windowDefaultWidth := 500,
                  windowDefaultHeight := 400]

      area <- dialogGetUpper dialog

      fch <- fileChooserWidgetNew FileChooserActionOpen
      containerAdd area fch 
                           
      onFileActivated fch $ 
           do file <- fileChooserGetFilename fch
              case file of
                   Just fpath -> do widgetDestroy dialog
                                    automat <- parseFile fpath parseAutomaton
                                    callLearner automat
                   Nothing -> return ()

      widgetShowAll dialog

-- * MAT Learner
-- | initialise dialog for output and call learner
main' :: Teacher t => t -> IO ()
main' teacher = do
                -- output
                
                -- create components
                dialog <- dialogNew
                set dialog [windowTitle := observationTableDialogTitle,
                                           windowDefaultWidth   := 200,
                                           windowDefaultHeight  := 200]
                observationTableOut <- tableNew 0 0 False
                statusOut <- tableNew 0 0 False
                area <- dialogGetUpper dialog
                -- horizontal growing box ,columns do not have the same width, column distance = 5
                box <- hBoxNew False 5
                vbox <- vBoxNew False 5
                frameOT <- frameNew
                frameStatus <- frameNew
                
                -- set frame style and text
                frameSetShadowType frameOT ShadowOut
                frameSetLabel frameOT observationTableFrame
                frameSetShadowType frameStatus ShadowOut
                frameSetLabel frameStatus statusFrame

                -- place components
                dialogAddButton dialog nextStep ResponseOk
                containerAdd area box
                containerAdd frameOT observationTableOut
                containerAdd frameStatus statusOut
                boxPackStart box frameOT PackNatural 0
                boxPackStart box vbox PackNatural 0
                boxPackStart vbox frameStatus PackNatural 0


                -- display components
                widgetShowAll area
                ans <- dialogRun dialog

                -- call learner
                initState <- evalStateT (initObs teacher) (OT ([],[],empty),GUI (dialog,observationTableOut,frameOT,statusOut,frameStatus,None))
                automaton <- evalStateT (learn teacher) initState
                widgetDestroy dialog


-- | create and fill initial observation table 
initObs :: Teacher t => t -> StateT (ObservationTable,GraphicUserInterface) IO (ObservationTable,GraphicUserInterface)
initObs teacher = do
                    sigma <- lift $ getSigma teacher
                    let s = take 1 (getAllTrees [] sigma [X])
                    (_,out) <- get
                    put (OT (s,[X],empty),out)
                    updateMapping teacher (getAllTrees s sigma [X])
                    state <- get
                    return state


-- | main loop in which consistency, closedness and correctness are checked
learn :: Teacher t => t -> StateT (ObservationTable,GraphicUserInterface) IO (Automaton Int)
learn teacher = do 
    outputLearn teacher
    (obs@(OT (s,contexts,mapping)),out) <- get
    sigma <- lift $ getSigma teacher

    closed <- closify (getSigmaS s sigma) teacher
    if not closed
        then learn teacher
        else do 
            consistent <- consistify (choose 2 s) teacher
            if not consistent
                then learn teacher
                else do
                    correct <- correctify teacher
                    if correct
                        then do -- automaton accepted programm is finished
                            (obs,out) <- get
                            return (generateAutomaton obs sigma)
                        else
                            learn teacher


-- | check whether obs is consistent and return consitified version (or old version if the table already was consistent)
-- | the lists in the first argument must always contain 2 trees so its essentially a list of pairs
-- | the function checks for each of these pairs whether they have the same row and if thats the case whether this pair is consistent
consistify :: Teacher t => [[Tree String]] -> t -> StateT (ObservationTable,GraphicUserInterface) IO Bool
consistify []           teacher = do
    outputConsistent teacher
    return True
consistify ([s1,s2]:xs) teacher = do
    (OT (s,contexts,mapping),out) <- get
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
    -> StateT (ObservationTable,GraphicUserInterface) IO Bool
checkConsistencyContexts _       _  _   []    = return True
checkConsistencyContexts teacher s1 s2 (c:cs) = do
    (OT (_,contexts,mapping),out) <- get
    let s1' = concatTree s1 c
        s2' = concatTree s2 c
    consistent <- checkConsistencyOneContext teacher (obst s1' contexts mapping) (obst s2' contexts mapping) c contexts s1 s2 s1' s2'
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
    -> Tree String -- s1
    -> Tree String -- s2
    -> Tree String -- s1'
    -> Tree String -- s2'
    -> StateT (ObservationTable,GraphicUserInterface) IO Bool
checkConsistencyOneContext _       []     []     _       _      _  _  _   _   = return True
checkConsistencyOneContext teacher (x:xs) (y:ys) context (c:cs) s1 s2 s1' s2'
    | x == y = checkConsistencyOneContext teacher xs ys context cs s1 s2 s1' s2'
    | True   = do
        outputNotConsistent teacher s1 s2 s1' s2' c (concatContext context c)
        (OT (s,contexts,mapping),out) <- get
        sigma <- lift $ getSigma teacher
        put (OT (s,contexts ++ [concatContext context c], mapping),out)
        -- ask for new memberships
        updateMapping teacher (getAllTrees s sigma [concatContext context c]) -- we only need to ask for memberships for trees inserted into the new context
        
        return False


-- | check whether Observation Table is closed and return a closed Observation Table
closify :: Teacher t => [Tree String] ->  t -> StateT (ObservationTable,GraphicUserInterface) IO Bool
closify []     teacher = do
    outputClosed teacher
    return True
closify (x:xs) teacher = do
    (OT (s,contexts,mapping),_) <- get
    if (any ((obst x contexts mapping) == ) (map snd (getTable s contexts mapping))) 
        then
            closify xs teacher
        else do
            sigma <- lift $ getSigma teacher
            -- output OT not closed
            outputNotClosed teacher x
            (_,out) <- get
            put (OT (s ++ [x],contexts,mapping),out)
            -- ask for new memberships
            updateMapping teacher
                          (concatMap (\t -> map (\c -> concatTree t c) contexts) -- insert the trees into all possible contexts
                                     (map (concatTree x) (getContexts (s ++ [x]) sigma))) -- we only need to consider trees in which the new tree occurs
            return False



-- | check whether the current ObservationTable represents the correct Automaton and process counterexample
correctify :: Teacher t => t -> StateT (ObservationTable,GraphicUserInterface) IO Bool
correctify teacher = do
                    fillStatus 4
                    (obs@(OT (s,contexts,mapping)),out) <- get
                    sigma <- lift $ getSigma teacher
                    let automaton = generateAutomaton obs sigma
                    maybeCounterexample <- lift $ conjecture teacher False "" automaton
                    if maybeCounterexample == Nothing
                        then do
                            outputCorrect teacher automaton
                            return True
                        else do
                            counterexample <- lift $ checkCE maybeCounterexample mapping sigma automaton -- errors in the counterexample can only occor with an interactive teacher (hopefully)
                            let mapping' = insert counterexample (not (accepts automaton counterexample)) mapping -- insert membership for counterexample
                            put(OT(s,contexts,mapping'),out)
                            outputExtractInit teacher counterexample
                            x <- extract teacher
                                        (getTable s contexts mapping') 
                                        (getTable (getSigmaS s sigma) contexts mapping') -- Simga(S)/S
                                        counterexample
                            outputExtractDelete teacher x
                            (OT (_,_,mapping''),out) <- get -- get mapping with the new memberships insertet in extract
                            put (OT (s ++ [x],contexts,mapping''),out)
                            updateMapping teacher
                                          (concatMap (\t -> map (\c -> concatTree t c) contexts) -- insert the trees into all possible contexts
                                                     (map (concatTree x) (getContexts (s ++ [x]) sigma)))-- we only need to consider trees in which the new tree occurs
                            return False
                            where
                                -- | check whther the given counterexample is a correct tree (check ranks of node labels) and whether it is actually a counterexample and if not ask for a new one
                                    -- TODO right now each time the automaton is printed out aigain
                                checkCE :: Maybe (Either (Tree String) String) -> Map (Tree String) Bool -> [(String,Int)] -> Automaton Int -> IO (Tree String)
                                checkCE Nothing                      mapping sigma automaton = do -- this should be impossible
                                                                                displayDialog counterexampleNothing tryAgain
                                                                                newcounterexample <- conjecture teacher True "" automaton
                                                                                checkCE newcounterexample mapping sigma automaton
                                checkCE (Just (Right err))           mapping sigma automaton = do 
                                                                                displayDialog err tryAgain
                                                                                newcounterexample <- conjecture teacher True "" automaton
                                                                                checkCE newcounterexample mapping sigma automaton
                                checkCE (Just (Left counterexample)) mapping sigma automaton = if (checkValidity counterexample sigma) /= Nothing
                                                                                then do -- symbols have wrong ranks
                                                                                    displayDialog counterexampleNoTree tryAgain
                                                                                    newcounterexample <- conjecture teacher True (nicerShow counterexample) automaton
                                                                                    checkCE newcounterexample mapping sigma automaton
                                                                                else if (member counterexample mapping) && (mapping ! counterexample /= (not (accepts automaton counterexample)))
                                                                                    then do -- the conjectured automaton behaves correctly for the given counterexample
                                                                                        displayDialog counterexampleMember tryAgain
                                                                                        newcounterexample <- conjecture teacher True (nicerShow counterexample) automaton
                                                                                        checkCE newcounterexample mapping sigma automaton
                                                                                    else
                                                                                        return counterexample


-- | extract subtree that has to be added to the observation table
extract :: Teacher t => t -> [(Tree String,[Bool])] -> [(Tree String,[Bool])] -> Tree String -> StateT (ObservationTable,GraphicUserInterface) IO (Tree String)
extract teacher s sigmaS counterexample = do
                                    outputExtractFill1 teacher counterexample sTree
                                    newcounterexample <- getNewCandidate newCECandidates
                                    if newcounterexample == Nothing
                                        then do
                                            outputExtractFill3 teacher
                                            return sTree
                                        else do
                                            outputExtractFill2 teacher (snd $ fromJust newcounterexample)
                                            extract teacher s sigmaS (fst $ fromJust newcounterexample)
    where 
        (newCECandidates,sTree) = tryReduce counterexample

        -- check whether for some s' the tree is still a counterexample
        getNewCandidate :: [(Tree String,Tree String)] -> StateT (ObservationTable,GraphicUserInterface) IO (Maybe (Tree String,Tree String))
        getNewCandidate []             = return Nothing -- no s' to s found that fulfills all conditions
        getNewCandidate ((x,s'Tree):xs) = do
                                    updateMappingExtract teacher x -- store membership of new tree in mapping
                                    (OT (s,contexts,mapping),out) <- get
                                    lift $ displayDialog (extractIsMember (nicerShow x) (mapping!x)) nextStep
                                    if mapping ! counterexample /= mapping ! x  -- isMemberOldCounterexample not eqal isMemberNewCounterexample
                                        then
                                            getNewCandidate xs
                                        else
                                            return $ Just (x,s'Tree)

        -- return list of Trees where s is replaced by all possible s'
        tryReduce :: Tree String -> ([(Tree String,Tree String)],Tree String)
        tryReduce tree@(Node symbol ts)
            |maybeRowOfs == Nothing = let -- the current subtree is not in Sigma(S)/S
                                          insertTrees _      []          _                       = ([],tree)
                                          insertTrees tsLeft (t:tsRight) (([],_    ):tsReplaced) = insertTrees (tsLeft ++ [t]) tsRight tsReplaced
                                          insertTrees tsLeft (t:tsRight) ((tR,sTree):tsReplaced) = ([(Node symbol (tsLeft ++ [t'] ++ tsRight),oldSubtree) | (t',oldSubtree)<-tR],sTree) -- we only need one s but all possible s'
                                      in
                                        insertTrees [] ts (map tryReduce ts)
            |otherwise              = (map (\(t,_) -> (t,t)) $ filter (\(_,q) -> (snd (fromJust maybeRowOfs) == q)) s,tree) -- the current subtree is in Sigma(S)/S now return all possible s'
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


obst :: Ord a => Tree a -> [Context a] -> Map (Tree a) Bool -> [Bool]
obst tree cs mapping = map fromJust $ maybeObst tree cs mapping


getTable :: Ord a => [Tree a] -> [Context a] -> Map (Tree a) Bool -> [(Tree a,[Bool])]
getTable s contexts mapping = map (\(tree,row) -> (tree,map fromJust row)) $ maybeGetTable s contexts mapping


-- use for testing : obst (Node 1 []) [X,(CNode 2 [X])] (fromList [((Node 1 []),True),((Node 2 [Node 1 []]), False)])
-- | get row for the given tree in observation table
maybeObst :: Ord a => Tree a -> [Context a] -> Map (Tree a) Bool -> [Maybe Bool]
maybeObst tree cs mapping = map (\c -> lookup (concatTree tree c) mapping) cs


-- | get the filled out observation table
maybeGetTable :: Ord a => [Tree a] -> [Context a] -> Map (Tree a) Bool -> [(Tree a,[Maybe Bool])]
maybeGetTable s contexts mapping = zip s (map (\x -> maybeObst x contexts mapping) s)


-- | inserts unknown memberships of given trees  into mapping
updateMapping :: Teacher t => t -> [Tree String] -> StateT (ObservationTable,GraphicUserInterface) IO ()
updateMapping teacher []     = return () 
updateMapping teacher (t:ts) = do
                            (OT (s,contexts,mapping),_) <- get
                            when (notMember t mapping)
                                 (do
                                    outputUpdateMapping teacher t
                                    (_,out) <- get
                                    member <- lift $ isMember teacher t
                                    put(OT (s,contexts,(insert t member mapping)),out)
                                    outputUpdateMapping teacher t)
                            updateMapping teacher ts


-- | inserts unknown memberships of given trees  into mapping
updateMappingExtract :: Teacher t => t -> Tree String -> StateT (ObservationTable,GraphicUserInterface) IO ()
updateMappingExtract teacher t = do
                            (OT (s,contexts,mapping),_) <- get
                            when (notMember t mapping)
                                 (do
                                    (_,out) <- get
                                    member <- lift $ isMember teacher t
                                    put(OT (s,contexts,(insert t member mapping)),out))
                            return ()
-- * Output

formatObservationTable :: ObservationTable -> [(String,Int)] -> ([(String,Context String)],[(String,Tree String)],[(String,Tree String)],[[String]],[[String]])
formatObservationTable (OT (s,contexts,mapping)) alphabet = (zip (map showContext contexts) contexts,sigmaTrees ,sigmaSTrees,sigmaRows,sigmaSRows)
                    where   sigmaTable = maybeGetTable s contexts mapping
                            sigmaTrees = zip (zipWith (\ treeVariable tree -> treeVariable ++ ":=" ++ tree) (zipWith (++) (repeat "t") (map show [1..])) (map (nicerShow . fst) sigmaTable)) (map fst sigmaTable)
                            sigmaRows = map (showBool . snd) sigmaTable -- observation table(sigmaPart | upper table) as [String] with 1 and 0 instead of True and False

                            sS = getSigmaS s alphabet
                            sigmaSTable = maybeGetTable sS contexts mapping -- observation table(sigmaSPart | lower table) without any elements of the upper one
                            sigmaSTrees = getSigmaSString s alphabet
                            sigmaSRows = map (showBool . snd) sigmaSTable

                            showBool :: [Maybe Bool] -> [String]
                            showBool []              = []
                            showBool (Nothing:xs)    = "*":(showBool xs)
                            showBool (Just True:xs)  = "1":(showBool xs)
                            showBool (Just False:xs) = "0":(showBool xs) 



fillStatus :: Int -> StateT (ObservationTable,GraphicUserInterface) IO ()
fillStatus n = do
            (obs,GUI (dialog,table,box,statusOld,frameStatus,extractOut)) <- get
            lift $ widgetDestroy statusOld
            statusNew <- lift $ tableNew 6 1 False
            lift $ addStatus 1 statusNew
            lift $ addStatus 2 statusNew
            lift $ addStatus 3 statusNew
            lift $ addStatus 4 statusNew
            lift $ addStatus 5 statusNew

            button <- lift $ buttonNew
            lift $ set button [buttonLabel := helpButtonLabel]

            lift $ onClicked button $do dialog2 <- dialogNew
                                        set dialog2 [windowTitle := infoDialog]
                                        area <- dialogGetUpper dialog2
                                        label <- labelNew (Just (helpText n))

                                        -- place components
                                        boxPackStart area label PackNatural 0
                                        
                                        -- display components
                                        widgetShowAll area

                                        -- wait for ok
                                        answer <- dialogRun dialog2
                                        widgetDestroy dialog2
                                        return ()

            lift $ tableAttachDefaults statusNew button 0 1 5 6

            lift $ containerAdd frameStatus statusNew
            lift $ widgetShowAll statusNew
            put(obs,GUI (dialog,table,box,statusNew,frameStatus,extractOut))

            where 
                addStatus i statusNew = do
                            label <- labelNew (Just $ status i)
                            tableAttachDefaults statusNew label 0 1 (i-1) i
                            if i > n 
                                then widgetModifyFg label StateNormal inactiveColor
                                else widgetModifyFg label StateNormal $ activeColor i



-- | put the labels into the table with the given colors
fillTableWithOT :: ([(String,Color)],[(String,Color)],[(String,Color)],[[(String,Color)]],[[(String,Color)]]) -> StateT (ObservationTable,GraphicUserInterface) IO ()
fillTableWithOT (contexts,sigmaTrees,sigmaSTrees,sigmaRows,sigmaSRows) = do
                            (obs,GUI (dialog,tableOld,box,status,frameStatus,extractOut)) <- get
                            lift $ widgetDestroy tableOld
                            table <- lift $ tableNew (3 + (length (sigmaTrees ++ sigmaSTrees))) (2 + (length contexts)) False
                            --tableResize table (3 + (length (sigmaTrees ++ sigmaSTrees))) (2 + (length contexts))
                            lift $ do 
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

                            -- lines
                            -- TODO find antother solution for the lines
                            labelH1 <- lift $ labelNew (Just (replicate ((length contexts) + 1 + (maximum (map (length . fst) (sigmaTrees ++ sigmaSTrees)))) '-'))
                            labelH2 <- lift $ labelNew (Just (replicate ((length contexts) + 1 + (maximum (map (length . fst) (sigmaTrees ++ sigmaSTrees)))) '-'))
                            --labelV <- lift $ labelNew (Just (concat (replicate ((length (sigmaTrees ++ sigmaSTrees)) + 2 + (maximum (map (length . fst) contexts))) "|\n")))
                            font <- lift $ fontDescriptionFromString fontObservationtable
                            lift $ widgetModifyFont labelH1 (Just font)
                            lift $ widgetModifyFont labelH2 (Just font)
                            lift $ tableAttachDefaults table labelH1 0 (2 + (length contexts)) 1 2
                            lift $ tableAttachDefaults table labelH2 0 (2 + (length contexts)) (2 + (length sigmaTrees)) (3 + (length sigmaTrees))
                            --lift $ tableAttachDefaults table labelV  1 2 0 (3 + (length (sigmaTrees ++ sigmaSTrees)))
                            lift $ fillOneDim table (1,1) incV (replicate ((length (sigmaTrees ++ sigmaSTrees)) + 2) ("|",colorNormal)) False


                            lift $ containerAdd box table
                            lift $ widgetShowAll table
                            put(obs,GUI (dialog,table,box,status,frameStatus,extractOut))


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
                                                                                font <- fontDescriptionFromString fontObservationtable
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


outputClosed :: Teacher t => t -> StateT (ObservationTable,GraphicUserInterface) IO ()
outputClosed teacher = do
                    fillStatus 2
                    (obs@(OT (s,contexts,mapping)),GUI (dialog,observationTableOut,box,status,frameStatus,extractOut)) <- get
                    sigma <- lift $ getSigma teacher
                    let (contextsOut,sigmaTreesOut,sigmaSTreesOut,sigmaRowsOut,sigmaSRowsOut) = formatObservationTable obs sigma
                        noColor = \x -> (x,colorNormal)
                    fillTableWithOT (map (noColor . fst) contextsOut,map (noColor . fst) sigmaTreesOut,map (noColor . fst) sigmaSTreesOut,map (map noColor) sigmaRowsOut,map (map noColor) sigmaSRowsOut)
                    lift $ waitForNextStep dialog
                    lift $ displayDialog isClosedMsg nextStep
                    lift $ waitForNextStep dialog
                    return ()


outputNotClosed :: Teacher t => t -> Tree String -> StateT (ObservationTable,GraphicUserInterface) IO ()
outputNotClosed teacher treeClosed = do
                    fillStatus 2
                    (obs@(OT (s,contexts,mapping)),GUI (dialog,observationTableOut,box,status,frameStatus,extractOut)) <- get
                    sigma <- lift $ getSigma teacher
                    let (contextsOut,sigmaTreesOut,sigmaSTreesOutTrees,sigmaRowsOut,sigmaSRowsOut) = formatObservationTable obs sigma
                        noColor = \x -> (x,colorNormal)
                        notClosedColor = \x -> (x,colorClosed) 
                        list = map go (zip sigmaSTreesOutTrees sigmaSRowsOut)
                        sigmaSTreesOutColor = map fst list
                        sigmaSRowsOutColor = map snd list

                        go :: ((String,Tree String),[String]) -> ((String,Color),[(String,Color)])
                        go ((treeStr,tree),row)
                            | tree == treeClosed = (notClosedColor treeStr,map notClosedColor row)
                            | otherwise          = (noColor treeStr,map noColor row)
                    fillTableWithOT (map (noColor . fst) contextsOut,map (noColor . fst) sigmaTreesOut,sigmaSTreesOutColor,map (map noColor) sigmaRowsOut,sigmaSRowsOutColor)
                    lift $ waitForNextStep dialog
                    lift $ displayDialog isNotClosedMsg nextStep
                    lift $ waitForNextStep dialog
                    lift $ displayDialog (addTree treeClosed) nextStep
                    return ()


outputConsistent :: Teacher t => t -> StateT (ObservationTable,GraphicUserInterface) IO ()
outputConsistent teacher = do
                    fillStatus 3
                    (obs@(OT (s,contexts,mapping)),GUI (dialog,observationTableOut,box,status,frameStatus,extractOut)) <- get
                    sigma <- lift $ getSigma teacher
                    let (contextsOut,sigmaTreesOut,sigmaSTreesOut,sigmaRowsOut,sigmaSRowsOut) = formatObservationTable obs sigma
                        noColor = \x -> (x,colorNormal)
                    fillTableWithOT (map (noColor . fst) contextsOut,map (noColor . fst) sigmaTreesOut,map (noColor . fst) sigmaSTreesOut,map (map noColor) sigmaRowsOut,map (map noColor) sigmaSRowsOut)
                    lift $ waitForNextStep dialog
                    lift $ displayDialog isConsistentMsg nextStep
                    lift $ waitForNextStep dialog
                    return ()


outputNotConsistent :: Teacher t => t -> Tree String -> Tree String -> Tree String -> Tree String -> Context String -> Context String -> StateT (ObservationTable,GraphicUserInterface) IO ()
outputNotConsistent teacher s1 s2 s1' s2' c' newC = do
                    fillStatus 3
                    (obs@(OT (s,contexts,mapping)),GUI (dialog,observationTableOut,box,status,frameStatus,extractOut)) <- get
                    sigma <- lift $ getSigma teacher
                    let (contextsOutTrees,sigmaTreesOutTrees,sigmaSTreesOutTrees,sigmaRowsOut,sigmaSRowsOut) = formatObservationTable obs sigma
                        noColor = \x -> (x,colorNormal)
                        notConsistentColor = \x -> (x,colorConsistent)

                        listSigma = map goRow (zip sigmaTreesOutTrees sigmaRowsOut)
                        sigmaTreesOutColor = map fst listSigma
                        sigmaRowsOutColor = map snd listSigma

                        listSigmaS = map goRow (zip sigmaSTreesOutTrees sigmaSRowsOut)
                        sigmaSTreesOutColor = map fst listSigmaS
                        sigmaSRowsOutColor = map snd listSigmaS
                        contextsOut = map goContext contextsOutTrees
                        
                        goRow :: ((String,Tree String),[String]) -> ((String,Color),[(String,Color)])
                        goRow (ele@(treeStr,tree),row)
                            | elem tree [s1,s2,s1',s2'] = (notConsistentColor (fst $ goElem s1 "s1" $ goElem s2 "s2" $ goElem s1' "s1'" $ goElem s2' "s2'" ele),goCol contexts row)
                            | otherwise                 = (noColor treeStr,map noColor row)

                        goElem :: Tree String -> String -> (String,Tree String) -> (String,Tree String)
                        goElem sTree sTreeText (treeStr,tree)
                            | tree == sTree     = (sTreeText ++ "=" ++ treeStr,tree)
                            | otherwise         = (treeStr,tree)

                        goCol :: [Context String] -> [String] -> [(String,Color)]
                        goCol [] [] = []
                        goCol (c:cs) (r:rs)
                            | c == c'   = (notConsistentColor r) : (map noColor rs)
                            | otherwise = (noColor r) : (goCol cs rs)

                        goContext :: (String,Context String) -> (String,Color)
                        goContext (cStr,c)
                            | c == c'   = notConsistentColor cStr
                            | otherwise = noColor cStr
                    fillTableWithOT (contextsOut,sigmaTreesOutColor,sigmaSTreesOutColor,sigmaRowsOutColor,sigmaSRowsOutColor)
                    lift $ waitForNextStep dialog
                    lift $ displayDialog isNotConsistentMsg nextStep
                    lift $ waitForNextStep dialog
                    lift $ displayDialog (addContext newC) nextStep
                    return ()


outputCorrect :: Teacher t => t -> Automaton Int -> StateT (ObservationTable,GraphicUserInterface) IO ()            
outputCorrect teacher automaton = do
                lift $ displayDialog (show automaton) lastStep
                return ()


outputLearn :: Teacher t => t -> StateT (ObservationTable,GraphicUserInterface) IO ()            
outputLearn teacher = do
                --fillStatus 0
                (obs@(OT (s,contexts,mapping)),GUI (dialog,observationTableOut,box,status,frameStatus,extractOut)) <- get
                sigma <- lift $ getSigma teacher
                let (contextsOut,sigmaTreesOut,sigmaSTreesOut,sigmaRowsOut,sigmaSRowsOut) = formatObservationTable obs sigma
                    noColor = \x -> (x,colorNormal)
                fillTableWithOT (map (noColor . fst) contextsOut,map (noColor . fst) sigmaTreesOut,map (noColor . fst) sigmaSTreesOut,map (map noColor) sigmaRowsOut,map (map noColor) sigmaSRowsOut)
                lift $ waitForNextStep dialog
                return ()


outputUpdateMapping :: Teacher t => t -> Tree String -> StateT (ObservationTable,GraphicUserInterface) IO ()
outputUpdateMapping teacher tree = do
                fillStatus 1
                (obs@(OT (s,contexts,mapping)),GUI (dialog,observationTableOut,box,status,frameStatus,extractOut)) <- get
                sigma <- lift $ getSigma teacher
                let (contextsOut,sigmaTreesOut,sigmaSTreesOut,sigmaRowsOut,sigmaSRowsOut) = formatObservationTable obs sigma
                    sigmaTrees = map snd sigmaTreesOut
                    sigmaSTrees = map snd sigmaSTreesOut
                    sigmaRowsZipped = zipTable sigmaTrees (map snd contextsOut) sigmaRowsOut
                    sigmaRowsSZipped = zipTable sigmaSTrees (map snd contextsOut) sigmaSRowsOut

                    -- takes labels for rows, labels for column and table entrys returns a table where every entry is labeled with the corresponding row and column
                    --  12
                    -- a00  --> (0,a,1)(0,a,2)
                    -- b00      (0,b,1)(0,b,2)
                    zipTable :: [a] -> [b] -> [[c]] -> [[(c,a,b)]]
                    zipTable [] _ _ = []
                    zipTable (x:xs) ys (zs:zss) = (zip3 zs (repeat x) ys):(zipTable xs ys zss)

                    paintEntries :: (String,Tree String,Context String) -> (String,Color)
                    paintEntries (membership,t,c)
                        | concatTree t c == tree = updateColor membership
                        | otherwise              = noColor membership

                    paintContext :: [Tree String] -> (String,Context String) -> (String,Color)
                    paintContext ts (cStr,c)
                        | any (tree==) (map (\t -> concatTree t c) ts) = updateColor cStr
                        | otherwise                                    = noColor cStr

                    paintTrees :: [Context String] -> (String,Tree String) -> (String,Color)
                    paintTrees cs (tStr,t)
                        | any (tree==) (map (concatTree t) cs) = updateColor tStr
                        | otherwise                            = noColor tStr

                    noColor = \x -> (x,colorNormal)
                    updateColor = \x -> (x,colorUpdate)
                fillTableWithOT (map (paintContext (sigmaTrees ++ sigmaSTrees)) contextsOut,map (paintTrees contexts) sigmaTreesOut,map (paintTrees contexts) sigmaSTreesOut,map (map paintEntries) sigmaRowsZipped,map (map paintEntries) sigmaRowsSZipped)
                lift $ waitForNextStep dialog
                return ()


outputExtractInit :: Teacher t => t -> Tree String -> StateT (ObservationTable,GraphicUserInterface) IO ()  
outputExtractInit teacher counterexample = do
                (obs,GUI (dialog0,_,_,_,_,_)) <- get
                lift $ displayDialog (notCorrect counterexample) nextStep
                lift $ waitForNextStep dialog0
                sep1 <- lift $ vSeparatorNew
                sep2 <- lift $ vSeparatorNew

                seph1 <- lift $ hSeparatorNew
                seph2 <- lift $ hSeparatorNew
                seph3 <- lift $ hSeparatorNew
                -- begin extraction
                fillStatus 5

                (obs,GUI (dialog,observationTableOut,box,status,frameStatus,extractOutOld)) <- get
                dialogExtract <- lift $ dialogNew
                lift $ set dialogExtract [windowTitle := extractTitle]

                lift $ dialogAddButton dialogExtract nextStep ResponseOk
                area <- lift $ dialogGetUpper dialogExtract
                hBox <- lift $ hBoxNew False 5
                vBox1 <- lift $ vBoxNew False 5
                vBox2 <- lift $ vBoxNew False 5
                vBox3 <- lift $ vBoxNew False 5
                vBox4 <- lift $ vBoxNew False 5

                lift $ containerAdd area hBox
                lift $ boxPackStart hBox vBox1 PackNatural 0
                lift $ boxPackStart hBox sep1 PackNatural 0
                lift $ boxPackStart hBox vBox2 PackNatural 0
                lift $ boxPackStart hBox sep2 PackNatural 0
                lift $ boxPackStart hBox vBox3 PackNatural 0
                lift $ boxPackStart hBox vBox4 PackNatural 0

                -- fill initial row
                label1 <- lift $ labelNew (Just $ extractTableHead 1)
                label2 <- lift $ labelNew (Just $ extractTableHead 2)
                label3 <- lift $ labelNew (Just $ extractTableHead 3)
                lift $ boxPackStart vBox1 label1 PackNatural 0
                lift $ boxPackStart vBox1 seph1 PackNatural 0
                lift $ boxPackStart vBox2 label2 PackNatural 0
                lift $ boxPackStart vBox2 seph2 PackNatural 0
                lift $ boxPackStart vBox3 label3 PackNatural 0
                lift $ boxPackStart vBox3 seph3 PackNatural 0
                
                -- add help button
                button <- lift $ buttonNew
                lift $ set button [buttonLabel := helpButtonLabel]

                lift $ boxPackStart vBox4 button PackNatural 0

                lift $ onClicked button $do dialog2 <- dialogNew
                                            set dialog2 [windowTitle := infoDialog]
                                            area <- dialogGetUpper dialog2
                                            label <- labelNew (Just (helpText 5))

                                            -- place components
                                            boxPackStart area label PackNatural 0
                                            
                                            -- display components
                                            widgetShowAll area

                                            -- wait for ok
                                            answer <- dialogRun dialog2
                                            widgetDestroy dialog2
                                            return ()

                lift $ widgetShowAll dialogExtract
                put (obs,GUI (dialog,observationTableOut,box,status,frameStatus,Extract (dialogExtract,vBox1,vBox2,vBox3)))
                return ()


outputExtractFill1 :: Teacher t => t -> Tree String -> Tree String -> StateT (ObservationTable,GraphicUserInterface) IO () 
outputExtractFill1 teacher counterexample s = do
                (obs,GUI (dialog,observationTableOut,box,status,frameStatus,Extract (dialogExtract,vBox1,vBox2,vBox3))) <- get
                label1 <- lift $ labelNew (Just (nicerShow counterexample))
                label2 <- lift $ labelNew (Just (nicerShow s))
                lift $ boxPackStart vBox1 label1 PackNatural 0
                lift $ widgetShowAll dialogExtract
                ans <- lift $ dialogRun dialogExtract
                lift $ boxPackStart vBox2 label2 PackNatural 0

                lift $ widgetShowAll dialogExtract
                ans <- lift $ dialogRun dialogExtract
                --put (obs,GUI (dialog,observationTableOut,box,status,frameStatus,extractOut,boxExtract))
                return ()


outputExtractFill2 :: Teacher t => t -> Tree String -> StateT (ObservationTable,GraphicUserInterface) IO () 
outputExtractFill2 teacher s' = do
                (obs,GUI (dialog,observationTableOut,box,status,frameStatus,Extract (dialogExtract,vBox1,vBox2,vBox3))) <- get
                label3 <- lift $ labelNew (Just (nicerShow s'))
                lift $ boxPackStart vBox3 label3 PackNatural 0


                lift $ widgetShowAll dialogExtract
                ans <- lift $ dialogRun dialogExtract
                --put (obs,GUI (dialog,observationTableOut,box,status,frameStatus,extractOut,boxExtract))
                return ()


outputExtractFill3 :: Teacher t => t -> StateT (ObservationTable,GraphicUserInterface) IO () 
outputExtractFill3 teacher = do
                (obs,GUI (dialog,observationTableOut,box,status,frameStatus,Extract (dialogExtract,vBox1,vBox2,vBox3))) <- get
                label3 <- lift $ labelNew (Just "-")
                lift $ boxPackStart vBox3 label3 PackNatural 0


                lift $ widgetShowAll dialogExtract
                ans <- lift $ dialogRun dialogExtract
                --put (obs,GUI (dialog,observationTableOut,box,status,frameStatus,extractOut,boxExtract))
                return ()


outputExtractDelete :: Teacher t => t -> Tree String -> StateT (ObservationTable,GraphicUserInterface) IO () 
outputExtractDelete teacher extractedTree = do
                (obs,GUI (dialog,observationTableOut,box,status,frameStatus,Extract (dialogExtract,vBox1,vBox2,vBox3))) <- get

                lift $ widgetDestroy dialogExtract
                lift $ displayDialog (extracted extractedTree) nextStep

                put (obs,GUI (dialog,observationTableOut,box,status,frameStatus,None))
                return ()


-- | diplay dialog with the given taxt and destroy it afterwards
displayDialog :: String -> String -> IO ()
displayDialog labelText buttonText = do
            dialog <- dialogNew
            set dialog [windowTitle := infoDialog]
            area <- dialogGetUpper dialog
            label <- labelNew (Just labelText)

            -- place components
            boxPackStart area label PackNatural 0
            dialogAddButton dialog buttonText ResponseOk

            -- display components
            widgetShowAll area

            -- wait for ok
            answer <- dialogRun dialog
            widgetDestroy dialog
            return ()


waitForNextStep :: DialogClass self => self -> IO ()
waitForNextStep dialog = do
   ans <- dialogRun dialog
   if ans == ResponseOk 
    then return ()
    else exitWith ExitSuccess