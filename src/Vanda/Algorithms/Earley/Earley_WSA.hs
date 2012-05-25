-- (c) 2010 Linda Leuschner <Leuschner.Linda@mailbox.tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

-- |
-- Maintainer  :  Linda Leuschner
-- Stability   :  unbekannt
-- Portability :  portable
--
-- This module computes 'Hypergraph' out of a 'Hypergraph' and a 'WSA'. 
-- The resulting 'Hypergraph' will only recognize the given word.
-- This implementation uses the Early and the Bar-Hille algorithm.

-- The input 'Hypergraph' represents a synchronous contet-free grammar.
-- Variables in a production should start with 0. 
-- The list of nonterminals belonging to the variables is ordered by the index 
-- of the variables.

-- Left : nonterminals
-- Right: terminals

-- TODO: Resulting pscfg accepts the empty word

module Vanda.Algorithms.Earley.Earley_WSA  where

import Data.Ord as O
import Data.Tree as T
import Control.Applicative
import qualified Data.Set as S
import Data.Maybe
import Data.List as L
import qualified Data.Map as M
import Data.Either
import Control.Arrow
import Data.Int (Int32)
import Debug.Trace
import Control.DeepSeq
import qualified Data.Vector as V
import qualified Data.Text.Lazy as TIO
import qualified Data.Text.Lazy.IO as TIO

import Vanda.Hypergraph
import Vanda.Algorithms.Earley.WSA
import Vanda.Token
import Vanda.Hypergraph.BackwardStar (fromEdgeList)

instance (Show v, Show l, Show i) => Show (Hyperedge v l i) where
  show e
    = show (to e)
      ++ " -> "
      ++ show (from e)
      ++ "\n<"
      ++ show (label e)
      ++ ">\n "
      -- ++ unwords (Prelude.map show $ from e)
      ++ " # "
      ++ show (ident e)
      ++ "\n\n"

instance Show Item where
  show item
    = show (iHead item)
      ++ " -> "
      ++ show (bLeft item)
      ++ " * "
      ++ show (bRight item)
      ++ "\n   "
      ++ show (iEdge item)
      ++ "\n   "
      ++ show (stateList item)
      ++ " # nextSymb: "
      ++ show (nextSymb item)
      ++ "\n"

      
instance Ord Item where
  i <= i' = if (not $ nextSymb i == Nothing) && (not $ nextSymb i' == Nothing)
            then (fromJust $ nextSymb i) <= (fromJust $ nextSymb i')
            else (iHead i) <= (iHead i') 
  -- i == i' = if (i<=i') && (i'<=i)
           -- then    (iHead i == iHead i') 
                -- && (bLeft i == bLeft i')
                -- && (bRight i == bRight i')
                -- && (iEdge i == iEdge i')
                -- && (stateList i == stateList i')
                -- -- && (nextSymb i == nextSymb i')
           -- else (\_ _ -> False)
           
           
instance NFData Item where
  rnf (Item iHead bLeft bRight iEdge stateList nextSymb lastState)
    =       rnf iHead 
      -- `seq` rnf bLeft 
      --`seq` rnf bRight
      -- `seq` rnf iEdge
      `seq` rnf stateList
      `seq` rnf nextSymb
      `seq` rnf lastState
      
instance NFData l => NFData (V.Vector l) where
  rnf v = rnf $ V.toList v
            
      
instance NFData l => NFData (T.Tree l) where
  rnf (T.Node l ts) = rnf l `seq` rnf ts

instance NFData (BackwardStar (Int,Token,Int) 
                  (T.Tree (Either Int Token),[Either Int Token]) 
                  (Int,Int)) 
         where
  rnf (BackwardStar nodes f memo) = rnf nodes `seq` rnf memo 
    
instance (NFData k, NFData v) => NFData (M.Map k v) where
  rnf m = rnf (M.toList m)
  
-- instance (NFData v) => NFData (S.Set v) where
  -- rnf m = rnf (S.toList m)
  

  
-- Item: [iHead -> bLeft * bRight] 
-- statelist contains states of a wsa. 
-- Ignoring the bullet, the first symbol of the righthandside of an item
-- (not bRight) is wrapped by the first and the second state, the second symbol 
-- is wrapped by the second and the third state etc.
data Item = Item 
    { iHead ::  Token                 
    , bLeft ::  [Either Token Token]
    , bRight::  [Either Token Token]
    , iEdge     :: Int -- Hyperedge Token (T.Tree (Either Int Token), [Either Int Token]) Int
    , stateList::  [Int]
    , nextSymb ::  Maybe (Either Token Token)
    , lastState :: Int
    }deriving Eq
  
-- instance Ord Item where    
  -- i <= i' = (iHead i) <= (iHead i') 
    
  
-- The main procedure. Is it favourable to represent the weights as a vector 
-- instead of a map? The ids of the hyperedges do not start with 0.. (rename?)

earley 
  :: EdgeList Token (T.Tree (Either Int Token), [Either Int Token]) Int
  -> M.Map Int Double
  -> WSA Int Token Double
  -> ( EdgeList (Int,Token,Int) 
                  (T.Tree (Either Int Token),[Either Int Token]) 
                  (Int,Int)
       , M.Map (Int,Int) Double)
earley el weights wsa = earley' (fromEdgeList el) weights wsa  0

earley'
  ::BackwardStar Token (T.Tree (Either Int Token), [Either Int Token]) Int
  -> M.Map Int Double -- weights of the hypergraph
  -> WSA Int Token Double
  -> Token                      -- an initial node of the Hypergraph
  -> ( EdgeList (Int,Token,Int) 
                  (T.Tree (Either Int Token),[Either Int Token]) 
                  (Int,Int)
       , M.Map (Int,Int) Double)
earley' hg weights wsa iniNode 
  = let statelist = [s|s <- states wsa, (snd $ fromJust $ L.find (\x -> fst x == s) 
                                 $ initialWeights wsa) > 0]
        itemlist = iter (M.empty,M.empty, M.empty) hg wsa (initState hg iniNode $ statelist)
        helist = extractTransitions hg weights wsa iniNode itemlist
        erg = create_hg helist iniNode wsa
    in  trace "start earley"
        $ statelist `deepseq` trace "statelist berechnet"
        $ itemlist `deepseq` trace "-------itemlist berechnet: -------- " 
              $ traceShow itemlist
        -- $ traceShow itemlist $ trace "-----------end list"  
        -- $ helist `deepseq` trace "helist berechnet"
        -- $ erg `deepseq` trace "erg berechnet"
        erg
        
-- ^ iter computes a predict, scan and complete. 
--   The first input, a tuple of list of items contains all items computed in 
--   earlier iteration steps but not in the last one. 
--   It is partitioned by items having no next symbol , i.e. items that are 
--   'ready' and items which have still    
iter 
  :: (M.Map Token [Item],M.Map (Either Token Token) [Item], M.Map (Either Token Token) [Item]) 
  -> BackwardStar Token (T.Tree (Either Int Token),[Either Int Token]) Int
  -> WSA Int Token Double
  -> [Item] 
  -> [Item] 
iter pre@(preNoNext, preLeft, preRight) hg wsa act  
  = let   -- c1  = conc preNoNext preNext
          -- !vec = filter (\x -> not $ elem x c1) act
          sorted = L.sortBy (O.comparing (nextSymb)) act
          (actNoNext',actNext') = L.span (\x -> (==) Nothing  (nextSymb x)) sorted
          f g list = (\x -> not $ elem x $ M.findWithDefault [] (g x) list ) 
          (actNT',actT') 
            = L.span 
                (\x ->  (==) False (isRight $ fromJust $ nextSymb x)) actNext'
          actNoNext 
            = filter -- (f iHead preNoNext) actNoNext'
                (\x -> not $ elem x $ M.findWithDefault [] (iHead x) preNoNext ) 
                actNoNext'
          actNT = filter (f (fromJust . nextSymb) preLeft) actNT'
            -- = filter 
                -- (\x-> not $ elem x $  M.findWithDefault [] 
                          -- (fromJust $ nextSymb x) preLeft) actNT'
          actT = filter (f (fromJust . nextSymb) preRight) actT'
                  -- (\x-> not $ elem x $  M.findWithDefault [] 
                          -- (fromJust $ nextSymb x) preRight) actT'
    in   -- trace "Nonterminals: " $ traceShow (V.length vecNT) $ -- traceShow (vecNT) $
         -- trace "Terminals: "  $ traceShow (V.length vecT) $
         if (actNoNext == [] && actNT == [] && actT == []) 
         then concat $ map snd $ M.toList preNoNext
         else let preNT = concat $ map snd $ M.toList preLeft
                  groupActNT 
                              = groupBy (\item1 item2 
                                          -> (nextSymb item1) == (nextSymb item2))
                                        actNT
                  newPredict = (actNT `deepseq` trace "predict" $ predict hg groupActNT)
                  new 
                    = trace "new predictitems: " $ traceShow (length newPredict) $ 
                          actT `seq` (++) (scan wsa actT) newPredict-- new elements
                  concNew1 = trace "comp1" $ complete ((++) preNT actNT) actNoNext
                  concNew2 = trace "comp2" $ complete actNT 
                              (concat (map snd ( M.toList preNoNext)))
                  -- (concNewNext ,concNewNonext) 
                    -- = partition (\x -> (nextSymb x == Nothing)) concNew 
                  concatenate = (new,concNew1, concNew2) `deepseq` trace "completer" 
                            $ (++) concNew2 $ (++) concNew1 new
                  -- (newRight, newLeft) 
                      -- = L.partition (\x -> isRight $fromJust $ nextSymb x) actNext
                  newTuple@(newNoNext, appLeft, appRight) 
                    = (M.unionWith (++) (M.fromListWith (++) $ map 
                                (\x -> (iHead x,[x])) actNoNext) preNoNext
                      ,M.unionWith (++) (M.fromListWith (++) $ map 
                          (\x -> (fromJust $ nextSymb x,[x]) ) actNT) preLeft
                      ,M.unionWith (++) (M.fromListWith (++) $ map 
                          (\x -> (fromJust $ nextSymb x,[x]) ) actT) preRight
                      )
              in  actNoNext' `deepseq` trace "actNoNext' berechnet" $
                  actNext' `deepseq` trace "actNext' berechnet" $
                  -- actNext `deepseq` trace "actNext berechnet" $
                  actNoNext `deepseq` trace "actNoNext berechnet" $
                  actT `deepseq` trace "actT' berechnet" $
                  actNT `deepseq` trace "actNT' berechnet" $
                  -- newRight `deepseq` trace "newRight' berechnet" $
                  -- newLeft `deepseq` trace "newLeft' berechnet" $
                  trace "\n\n actNoNext\n\n" $ traceShow (take 10 actNoNext)
                  -- $ trace "\n\n actNext\n\n" $ traceShow (take 10 actNext)
                  $ trace "\n\n actT\n\n" $ traceShow (take 10 actT)
                  $ trace "\n\n actNT\n\n" $ traceShow (take 10 actNT) 
                  -- $ trace "\n\n newRight\n\n" $ traceShow (take 10 newRight) 
                  -- $ trace "\n\n newLeft\n\n" $ traceShow (take 10 newLeft) 
                  $ trace "\n\n preNT\n\n" $ traceShow (take 10 preNT) $
                  preNT `deepseq` trace "preNT berechnet" $
                  new `deepseq`  trace "new berechnet" $
                  concNew1 `deepseq` trace "concNew1 berechnet" $
                  concNew2 `deepseq` trace "concNew2 berechnet" $
                  concat `deepseq` trace "concat berechnet" $
                  newTuple `deepseq` trace "newTuple berechnet" $
                  -- trace "sorted: " -- $  traceShow sorted 
                   -- (TIO.writeFile "/home/student/lindal/files/git/testEarley_items.txt" 
                    -- $ TIO.pack $  "new items: " ) -- ++  (show new))
                      --(newTuple,tracer) `deepseq` trace "iter" 
                      --c1 `seq` trace "c1 berechnet"
                      -- $ vec `seq` trace "vec berechnet"
                      -- trace "iter beginnt" 
                      -- $ actNext' `deepseq` trace "actNext' berechnet"
                      -- $ actNext `deepseq` trace "actNext berechnet"
                      -- $ actT `deepseq` trace "actT berechnet"
                      -- $ preNT `deepseq` trace "preNT berechnet"
                      -- $ new `deepseq` trace "new berechnet"
                      -- $ concNew1 `deepseq` trace "concNew1 berechnet"
                      -- $ concNew2 `deepseq` trace "concNew2 berechnet"
                      -- $ concat `deepseq` trace "concat berechnet"
                      -- $ newTuple `deepseq` trace "newTuple berechnet"
                      (iter newTuple hg wsa concatenate)
                 
initState
  :: BackwardStar Token (T.Tree (Either Int Token), [Either Int Token]) Int
  -> Token      -- ^ an initial node of the Hypergraph.
  -> [Int]      -- ^ list of states of a 'WSA' 
  -> [Item]
initState hg iniNode stateList
  = let list = 
          [Item iniNode 
                []
                right
                (ident edge) 
                [q]
                (listToMaybe right)
                q
          | edge <- (backStar hg iniNode)
          , q <- traceShow stateList stateList
          , let right = (map (either (Left  . ((!!) $ from edge))  (Right . id))  
                  (snd $ label edge) )
            -- list <- combine 0 n (map (either id id) (snd $ label edge))
          ]
    in  trace "-------------------" 
        $ trace "init:" 
        $ traceShow list 
        $ trace "-------------------"
        list
          




{-
predict u v = V.empty
complete u v = V.empty
scan u v = V.empty
-}


-- completeNoNext :: [Item] -> [Item] -> [Item]
-- completeNoNext preNext newNoNext 
   -- = if (newNoNext == [])
     -- then []
     -- else let Left next = iHead $ head newNoNext 
              -- (nextEq, remain) 
                -- = partL (\item -> iHead item) 
                        -- next 
                        -- newNoNext
                              -- -- = V.partition (\x -> nextSymb x == Just (Left next)) new
              -- comp = (complete' next nextEq)
              -- newItems = --traceShow next $ traceShow (V.length $ nextEq) 
                -- comp `seq` conc comp $ completeNoNext preNext remain  
          -- in newItems
                       
                       
                       
{-  = let newItems = concatMap (complete' all) new
                   in trace "complete" $ nub newItems-}

complete :: [Item] -> [Item] -> [Item]
complete nextI noNext 
   = if (noNext == [] || nextI == [])
     then trace "complete ready" []
     else let -- nextS = traceShow (iHead $ head noNext) (iHead $ head noNext)
              -- Just (Left nextS) = nextSymb $ head next 
              -- (nextEq, remainN) 
                -- = partL (\item -> either id id $ fromJust $ nextSymb item) 
                        -- nextS 
                        -- next
                    -- = V.partition (\x -> nextSymb x == Just (Left next)) new
              -- Left head = iHead $ head noNext
              
              headord = L.sortBy (O.comparing (iHead)) noNext
              headSort = groupBy (\item1 item2 
                           -> (iHead item1) == (iHead item2))
                         headord
              Just (Left next) = nextSymb $ head nextI
              sortList = groupBy (\item1 item2 
                           -> (nextSymb item1) == (nextSymb item2))
                         nextI
              nexts = map (fromLeft . fromJust . nextSymb . head) sortList
              newItems = concatPart sortList headSort
                  -- (headEq, remainH) 
                 --  = partL (\item -> iHead item) nextS noNext
              -- comp = {- trace "complete'"-} (complete' nextEq headEq nextS)
              -- newItems = --traceShow next $ traceShow (V.length $ nextEq) 
                -- -- comp `seq` trace "complete' ready" $ 
                                -- (++) comp 
                                -- $ complete (remainN) 
                                  -- -- $
                                  -- -- (trace "headEq:" $ traceShow headEq $
                                  -- -- trace "remainH: " $ traceShow remainH 
                                  -- remainH --)  
          in -- trace "next: " $  traceShow next $ trace "noNext" $ traceShow noNext $
              trace "sortList" $ sortList `deepseq` trace "sortList berechnet" 
                -- $ traceShow (sortList)
              trace "nexts" $ nexts `deepseq` trace "nexts berechnet" $
              newItems `deepseq` trace "conc ready -- leaving complete" newItems
              
concatPart ::[[Item]] -> [[Item]] -> [Item]
concatPart [] _ = trace "concpart ready" []
concatPart _ [] = trace "concpart ready" []
concatPart nexts heads 
  = let nextHead = (iHead $ head $ head heads)
        nextS = (fromLeft $ fromJust $ nextSymb $ head $ head nexts)
    in trace "nextSymbol: " $ traceShow (head nexts) $ 
       -- nextS `deepseq` trace "nextS berechnet" $ traceShow nextS $
       if  nextHead < nextS 
       then concatPart (nexts) (tail heads) 
       else if nextHead > nextS
            then concatPart (tail nexts) (heads) 
            else (complete' (head nexts) (head heads) nextS)
                  ++ concatPart (tail nexts) (tail heads)
               
-- complete [nextEq] -> [headEq]
complete' :: [Item] -> [Item] -> Token -> [Item]
complete' next noNext symb
  = let list = 
          nub
            [ 
            item{ nextSymb = listToMaybe newRight -- $ tail $ bRight item
            , bRight =  newRight -- tail $ bRight item
            , bLeft = bLeft item ++ [Left symb]
            , stateList = (stateList item) ++ [lState]
            , lastState = lState
                }
            | item <- next
            , item' <- noNext
            , (last $ stateList item) == (head $ stateList item')
            , let newRight  
                    = --if (bRight item == [])
                      --then []
                      --else 
                      tail $ bRight item
                  lState = last $ stateList item'
            ]
    in  -- trace "-------------------" 
        -- $ trace "complete':" 
        -- $ traceShow list 
        -- $ trace "-------------------"
        list
  

predict :: BackwardStar Token (T.Tree (Either Int Token),[Either Int Token]) Int
        -> [[Item]]
        -> [Item]
-- predict all V.empty = V.empty
predict hg !new =  -- trace "predict" $ 
                  if (new == [])
                  then trace "predict ready" []
                  else trace "anzahl new: " $ traceShow (length new) $ 
                    {- let ordList 
                              = L.sortBy 
                                  (O.comparing (nextSymb) ) 
                                  new -}
                       let -- Just (Left next) = nextSymb $ head $ head new
                           -- sortList 
                              -- = groupBy (\item1 item2 
                                          -- -> (nextSymb item1) == (nextSymb item2))
                                        -- new
                           nexts = map (fromLeft . fromJust . nextSymb . head) new
                           newItems = concat $ map2 (map (predict' hg) nexts)  new
                           
                           
                       -- let Just (Left next) = nextSymb $ head new
                           -- (!nextEq, !remain) = partL' next new
                           -- pred = (predict' hg next nextEq)
                           -- newItems 
                             -- = pred `seq` (++) pred $ predict hg remain
 
                       in -- ordList `deepseq` trace "ordList berechnet" 
                          -- sortList `deepseq` trace "sortList berechnet" 
                          -- $ remain `deepseq` trace "remain berechnet"
                          -- $ pred `deepseq` trace "pred berechnet"
                          newItems `deepseq` trace "newItems berechnet"
                          newItems
                     
                       
predict' :: BackwardStar Token (T.Tree (Either Int Token),[Either Int Token]) Int
        -> Token
        -> [Item]
        -> [Item]
-- predict' hg Item{nextSymb = Nothing} = V.empty
-- predict' hg Item{nextSymb = Just (Right a)} = V.empty
predict' hg !next !vec
  = -- trace "in predict'" $ 
    let list = [Item next
                []
                s'
                (ident hgEdge)
                [itemInd]
                (listToMaybe s')
                itemInd
                | hgEdge <- backStar hg $ next
                , itemInd <- S.toList $ S.fromList $ map lastState vec 
                , let s = snd $ label hgEdge 
                , let s' = map (either (Left . ((!!) $ from hgEdge))  
                                       (Right . id)) s
               ]
    in  -- trace "predict: " $  traceShow list $ 
        -- (TIO.writeFile "/home/student/lindal/files/git/testEarley_items.txt" 
            -- $ TIO.pack . show $  V.fromList  list)
        -- `seq`  
        -- trace "old items: " $ traceShow (length vec) $
        -- trace "predict-Items:" $traceShow (length list) $ -- trace "end predict'"  
        -- traceShow list
        -- list
        (next,vec) `deepseq`
        -- trace "-------------------" 
        -- $ trace "predict':" 
        list `deepseq`
        -- traceShow list 
        -- $ trace "-------------------"
        list

     
scan:: WSA Int Token Double -> [Item] -> [Item]
scan wsa new = -- trace "scan" $ 
                  if (new == [])
                  then []
                  else let Just (Right next) = nextSymb $ head new
                           (nextEq, remain) = partR next new
                           scn = (scan' wsa next nextEq)
                           newItems = -- traceShow next $ traceShow (V.length $ nextEq) 
                                    scn `seq` (++) scn $ scan wsa remain  
                       in newItems  
                                    

{-
               trace "scan" $ 
               let newItems = V.concatMap (scan' wsa) new
               in trace "new scanitems: " traceShow (newItems) 
                  -- $ (write  $ return ("vector: " ++  (show $ newItems))::IO())
                      `seq` newItems        
  -}           
                  
scan':: WSA Int Token Double -> Token -> [Item] -> [Item]
scan' wsa a vec -- item@Item{ -- nextSymb = Just (Right a)
             -- , bLeft = left
             -- , bRight = right
             -- , stateList = list
             -- } 
  = let erg = [item{ bLeft = (bLeft item) ++ [Right a]
           , bRight = if (bRight item == [])
                      then []
                      else tail $ bRight item
           , nextSymb = listToMaybe $tail $ bRight item
           , stateList = (stateList item) ++ [state] 
           , lastState = state
           }
           | trans <- transitions wsa
           , item <-  vec
           , transTerminal trans == fromIntegral a
           , last (stateList item) == transStateIn trans
           , let state = transStateOut trans
           ]
    in  -- trace "scan" -- $ traceShow erg $ 
        -- (TIO.writeFile "/home/student/lindal/files/git/testEarley_items.txt" 
            -- $ TIO.pack $  "item: " ++  (show $  V.fromList  erg))
        -- `seq`
        -- traceShow erg erg
        -- trace "-------------------" 
        -- $ trace "scan:" 
        -- $ traceShow erg 
        -- $ trace "-------------------"
        erg
    
create_hg ::[(Hyperedge (Int,Token,Int) 
                        (T.Tree (Either Int Token), [Either Int Token]) 
                        (Int,Int)
            , Double
            )]
          -> Token -- iniNode
          -> WSA Int Token Double
          -> (EdgeList (Int,Token,Int) 
                          (T.Tree (Either Int Token),[Either Int Token]) 
                          (Int,Int)
             , M.Map (Int,Int) Double)
create_hg [] _  _ = error "The given word is not derivable."
create_hg list iniNode wsa 
  = let flist = map fst list
        myZip = map (ident . fst) list
        myMap  = M.fromList $ zip myZip (map snd list) 
    in  trace "createHG"
        $ flist `deepseq` trace "flist berechnet"
        $ myZip `deepseq` trace "myZip berechnet"
        $ (mkHypergraph flist
          ,  myMap) 
    --map ((ident . fst) &&& snd) list )
    
    
          
extractTransitions
  :: BackwardStar Token (T.Tree (Either Int Token),[Either Int Token]) Int
  -> M.Map Int Double
  -> WSA Int Token Double
  -> Token
  -> [Item] 
  -> [(Hyperedge (Int,Token,Int) 
                (T.Tree (Either Int Token), [Either Int Token]) 
                (Int,Int)
      , Double
     )]
extractTransitions hg weights wsa iniNode v
  = let f (vec,n ::  Int) x 
            = let !trans = extract x hg weights wsa iniNode (n)
              in -- if (isJust trans)
                 -- then ((fromJust trans) : vec, n+1)
                 (trans : vec, n+1)
                 -- else (vec,n)
    in fst $ foldl' f ([],0) v
                         
                         
extract :: Item
        -> BackwardStar Token (T.Tree (Either Int Token),[Either Int Token]) Int
        -> M.Map Int Double
        -> WSA Int Token Double
        -> Token
        -> Int
        -> (Hyperedge  (Int,Token,Int) 
                             (T.Tree (Either Int Token), [Either Int Token]) 
                             (Int,Int), Double)
extract item@Item{bRight = []}
        hg
        weights
        wsa
        iniNode
        n
  = let edge = fromJust $ find (\x -> (ident x) == (iEdge item)) $ edgesEL $ toEdgeList hg
        initial_weight 
          = if (iHead item == iniNode)
            then (snd $ fromJust $ L.find 
                                  (\x -> (fst x) == (head $ stateList item)) 
                                 $ initialWeights wsa)
                 * (snd $ fromJust $ L.find 
                                  (\x -> (fst x) == (last $ stateList item)) 
                                 $ finalWeights wsa)
                         else 1
        goal =(mkHyperedge 
                  (head $ stateList item , iHead item , last $ stateList item) 
                  (carryL (snd $ label edge) (stateList item) (from edge))
                  (label edge) 
                  (ident edge, n)
             , ((weights M.! (ident edge)) 
                   * initial_weight 
                   * (L.product [transWeight trans
                               | trans <- transitions wsa
                               , (p,Right a,q) 
                                    <- carryR (snd $ label edge) (stateList item) 
                               , transStateIn trans  == p
                               , transTerminal trans == fromIntegral a
                               , transStateOut trans == q
                               ] ))
             )
    in    trace "extract:" 
        $ edge `deepseq` trace "edge berechnet"
        $ initial_weight `deepseq` trace "initial weight berechnet: " 
        $ goal `deepseq` trace "goal berechnet"
        -- $ edge `deepseq`
          -- trace "edge:" $ traceShow edge
        -- $ trace "goal:" $ traceShow goal
        $ trace "ready" goal
        -- traceShow item 
        -- $ traceShow initial_weight 
        -- traceShow initial_weight `deepseq` 
-- extract _ _ _ _ _ _
  -- = Nothing          
  
vNub l = vNub' l V.empty
  where
    vNub' vec vec'
      = if (vec == V.empty) 
        then V.empty
        else let h = V.head vec
                 t = V.tail vec
             in  if (h `V.elem` t) 
                 then vNub' t vec'
                 else let iter = vNub' t (V.snoc vec' h)
                      in  V.snoc iter h

-- ^ for a list of variables and terminals, 
-- a list of states of a 'WSA' and a list of Nonterminals, 
-- carryL extract tuples (i,A,j) where i and j are the states of the second list 
-- that belong to the nonterminal A of the third list and cooresponds to a
-- variable of the first list.
-- Assumption: the labels are linear in the second component, i.e. there is no 
-- variable that appears twice.
carryL:: (NFData s, NFData b, Num b) => [Either Int b] -> [s] -> [Token] -> [(s,Token,s)]
carryL label states nonterminals 
  = let nons = [0..((length nonterminals)-1)] 
        indVars = map (\x -> fromJust (elemIndex (Left x) label)) nons 
        statesBeforeVars = map ((!!)states) indVars
        statesAfterVars = map (\x -> (!!) states (x+1)) indVars
        erg = zip3 statesBeforeVars nonterminals statesAfterVars
    in  -- trace "carryL" 
        -- $ nons `deepseq` trace "nons berechnet" 
        -- $ indVars `deepseq` trace "indVars berechnet"
        -- $ statesBeforeVars `deepseq` trace "bfVars berechnet"
        -- $ statesAfterVars `deepseq` trace "aVars berechnet"
        -- $ erg `deepseq` trace "leaving carryL" 
        erg
carryR:: (NFData a, NFData b, NFData s, Num b) => [Either a b] -> [s] -> [(s,Either a b,s)]
carryR label states 
  = let rs = trace "carryR" filter isRight label
        indTerms = findIndices isRight label
        statesBeforeTerms = map ((!!)states) indTerms
        statesAfterTerms = map (\x -> (!!) states (x+1)) indTerms
        erg = zip3 statesBeforeTerms rs statesAfterTerms    
    in  -- trace "carryR" 
        -- $ rs `deepseq` trace "rs berechnet" 
        -- $ indTerms `deepseq`  trace "indTerms berechnet"
        -- $ statesBeforeTerms `deepseq` trace "bfTerms berechnet"
        -- $ statesAfterTerms `deepseq`  trace "aTerms berechnet"
        -- $ erg `deepseq` (trace "leaving carryR" erg)
        erg
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False

fromLeft (Left x) = x
fromLeft x = error $ "error while reading " ++ (show x)

concV :: V.Vector a -> V.Vector a -> V.Vector a
concV v1 v2 =  V.fromList $ V.toList v2 ++ V.toList v1
    
conc :: [a] -> [a] -> [a]
conc v1 v2 = if (length v1 < length v2) 
             then v2 ++ v1
             else v1 ++ v2

concL :: [a] -> [a] -> [a]
concL = (++)              

concR :: [a] -> [a] -> [a]
concR v1 v2 = v2 ++ v1             

conc_all :: [[a]] -> [a]
conc_all = foldl' (concR) []

concat':: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = conc x  $ concat' xs 

foldr' f a []     = a
foldr' f a (x:xs) = let a' = f a x in a' `seq` foldl' f a' xs

             
partV :: V.Vector Item -> M.Map Token (V.Vector Item)
partV v = let f = (\v1 v2 -> concV v1 v2)
              next = (\x -> either id id $ fromJust $ nextSymb x)
          in  V.foldl' 
                (\m item -> M.insertWith' f (next item) (V.singleton item) m)
                M.empty 
                v
                
                
partR :: Token -> [Item] -> ([Item], [Item])
partR next new = partition (\x -> nextSymb x == Just (Right next)) new                

partL' :: Token -> [Item] -> ([Item], [Item])
partL' next new = partition (\x -> nextSymb x == Just (Left next)) new      

partL :: (Item -> Token) -> Token -> [Item] -> ([Item], [Item])
partL f next new = partition (\x -> f x == next) new 

partBy::(a -> a-> Bool) -> [a] -> [[a]]
partBy f list 
  = let eq = (\x -> f x $ head list) 
        (a,b) = partition eq list
        in a: (partBy f list)
  -- partBy' $ sortBy f list
  -- where partBy' [] = []
        -- partBy' list = let (a,b) = span f list
                       -- in a : (partBy b)
                       
map2 [] _ = []
map2 _ [] = []
map2 (f:fs) (x:xs) = (f x) : map2 fs xs  
------------------- debugging ---------------------------------------

write:: Show a => IO(a) -> IO()
write term = do term' <- term
                (TIO.writeFile "/home/student/lindal/files/git/testEarley_items.txt" 
                  $ TIO.pack $  "item: " ++  (show $  term'))

-- vector_fromList :: [a] -> V.MVector s a
-- vector_fromList = foldl' (V.snoc) (V.empty :: V.MVector s a)
  
{-
-- | Create a list of all chains (p_0,q_1,p_1)(p_1,q_2,p_2)...(p_k-1,q_k,p_k)
combine :: Int -> Int -> [Int] -> [[Int]]
-- combine i j [] = []
combine i j (x:[]) = [[i,j]]
combine i j (x:xs)
  = concat [map (\b -> i : b) (combine k j xs) | k <- [i..j]]
-}
          
{-
proj_left:: (T.Tree (Either Token Token), [Either Token Token]) -> T.Tree (Either Token Token)
proj_left = fst;

proj_right:: (T.Tree (Either Token Token), [Either Token Token]) -> T.Tree (Either Token Token)
proj_right (left,right) = right;
-}
