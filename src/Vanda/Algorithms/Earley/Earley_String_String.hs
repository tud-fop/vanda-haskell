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

module Vanda.Algorithms.Earley.Earley_String_String  where

-- import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Data.Either
import Data.List as L
import Data.Maybe
import qualified Data.Map as M
import Data.Ord as O
import qualified Data.Set as S
import Debug.Trace

import Vanda.Hypergraph
import Vanda.Token
import Vanda.Algorithms.Earley.WSA

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

instance (Show v, Show t) => Show (Item v t) where
  show item
    = show (iHead item)
      ++ " -> ???"
      ++ " * "
      ++ show (bRight item)
      ++ "\n   "
      ++ show (iEdge item)
      ++ "\n   "
      ++ show (stateList item)
      ++ " # nextSymb: "
      ++ show (nextSymb item)
      ++ "\n"

instance (NFData v, NFData t) => NFData (Item v t) where
  rnf (Item iHead bRight iEdge stateList nextSymb lastState)
    =       rnf iHead
      `seq` rnf stateList
      `seq` rnf nextSymb
      `seq` rnf lastState

instance (NFData v, NFData l)
  => NFData (BackwardStar (Int,v,Int) l (Int, Int)) where
    rnf (BackwardStar nodes f memo) = rnf nodes `seq` rnf memo 
    
instance (NFData k, NFData v) => NFData (M.Map k v) where
  rnf m = rnf (M.toList m)
  
-- Item: [iHead -> ??? * bRight] 
-- statelist contains states of a wsa. 
-- Ignoring the bullet, the first symbol of the righthandside of an item
-- (not bRight) is wrapped by the first and the second state, the second symbol 
-- is wrapped by the second and the third state etc.
data Item v t
  = Item 
    { iHead :: v
    , bRight :: [Either Int t]
    , iEdge :: Int
    , stateList :: [Int]
    , nextSymb :: Maybe (Either Int t)
    , firstState :: Int
    , weight :: Double
    }
  deriving Eq
  
instance Ord (Item v t) where
  i1 `compare` i2
    = (iEdge i1, stateList i1) `compare` (iEdge i2, stateList i2)
  
-- The main procedure. Is it favourable to represent the weights as a vector 
-- instead of a map? The ids of the hyperedges do not start with 0.. (rename?)

earley
  :: Ord v
  => BackwardStar v l Int
  -> (l -> [Either Int t])
  -> WSA Int t Double
  -> (EdgeList (Int, v, Int) l (Int, Int), M.Map (Int, Int) Double)
earley bs component wsa = earley' bs dir wsa 0

earley'
  :: Ord v
  => BackwardStar v l Int
  -> (l -> [Either Int t])
  -> WSA Int t Double
  -> v            -- an initial node of the Hypergraph
  -> (EdgeList (Int, v, Int) l (Int, Int), M.Map (Int,Int) Double)
earley' hg component wsa iniNode 
  = let statelist = [ s | (s, w) <- initialWeights wsa, w > 0 ]
        itemlist
          = iter
              (M.empty, M.empty, M.empty)
              hg
              component
              wsa
              (initState hg component iniNode statelist)
        helist = map (extract hg component wsa iniNode) itemlist
        erg = create_hg helist iniNode
    in  trace "start earley"
        $ statelist `deepseq` trace "statelist berechnet"
        $ itemlist `deepseq` trace "-------itemlist berechnet: -------- " 
        erg

iter 
  :: Ord v
  => ( M.Map v [Item v t]
     , M.Map (Either Token Token) [Item v t]
     , M.Map (Either Token Token) [Item v t]
     ) 
  -> BackwardStar v l Int
  -> (l -> [Either Int t]) 
  -> WSA Int t Double
  -> [Item v t]
  -> [Item v t]
iter (preNoNext, preLeft, preRight) hg component wsa act  
  = let sorted = L.sortBy (O.comparing (nextSymb)) act
        (actNoNext',actNext') = L.span ((Nothing ==) . nextSymb) sorted
        f g list x = not $ elem x $ M.findWithDefault [] (g x) list 
        (actNT',actT') = L.span (not . isRight . fromJust . nextSymb) actNext'
        actNoNext 
          = filter
              (\x -> not $ elem x $ M.findWithDefault [] (iHead x) preNoNext) 
              actNoNext'
        actNT = filter (f (fromJust . nextSymb) preLeft) actNT'
        actT = filter (f (fromJust . nextSymb) preRight) actT'
    in if (null actNoNext && null actNT && null actT) 
       then concat $ map snd $ M.toList preNoNext
       else let preNT = concat $ map snd $ M.toList preLeft
                groupActNT = groupBy
                               ( \item1 item2
                                 -> (nextSymb item1) == (nextSymb item2)
                               )
                               actNT
                newPredict = actNT `deepseq` trace "predict"
                           $ predict hg component groupActNT
                new = trace "new predictitems: "
                    $ traceShow (length newPredict)
                    $ actT `seq` (scan wsa actT ++ newPredict)
                concNew1 = trace "comp1"
                         $ complete (preNT ++ actNT) actNoNext
                concNew2 = trace "comp2"
                         $ complete actNT 
                         $ concat (map snd (M.toList preNoNext))
                concatenate = (new, concNew1, concNew2)
                            `deepseq` trace "completer" 
                            (concNew2 ++ concNew1 ++ new)
                newTuple@(newNoNext, appLeft, appRight) 
                    = ( M.unionWith (++) (M.fromListWith (++)
                        $ map (\x -> (iHead x,[x])) actNoNext) preNoNext
                      , M.unionWith (++) (M.fromListWith (++)
                        $ map (\x -> (fromJust $ nextSymb x,[x])) actNT) preLeft
                      , M.unionWith (++) (M.fromListWith (++)
                        $ map (\x -> (fromJust $ nextSymb x,[x])) actT) preRight
                      )
              in actNoNext' `deepseq` trace "actNoNext' berechnet" $
                 actNext' `deepseq` trace "actNext' berechnet" $
                 actNoNext `deepseq` trace "actNoNext berechnet" $
                 actT `deepseq` trace "actT' berechnet" $
                 actNT `deepseq` trace "actNT' berechnet" $
                 preNT `deepseq` trace "preNT berechnet" $
                 new `deepseq`  trace "new berechnet" $
                 concNew1 `deepseq` trace "concNew1 berechnet" $
                 concNew2 `deepseq` trace "concNew2 berechnet" $
                 concat `deepseq` trace "concat berechnet" $
                 newTuple `deepseq` trace "newTuple berechnet" $
                 (iter newTuple hg component wsa concatenate)

initState
  :: Ord v
  => BackwardStar v l Int
  -> (l -> [Either Int t])
  -> v          -- ^ an initial node of the Hypergraph.
  -> [Int]      -- ^ list of states of a 'WSA' 
  -> [Item v t]
initState hg component iniNode stateList
  = let list = 
          [ Item
              iniNode 
              right
              (ident edge)
              [q]
              (listToMaybe right)
              q
              1
          | edge <- backStar hg iniNode
          , q <- stateList
          , let right = (map (either
              (Left . ((!!) $ from edge))
              (Right . id))
              (component $ label edge))
          ]
    in trace "-------------------" 
       $ trace "init:" 
       $ traceShow list 
       $ trace "-------------------"
       list

complete :: [Item v t] -> [Item v t] -> [Item v t]
complete nextI noNext 
   = if (noNext == [] || nextI == [])
     then trace "complete ready" []
     else let headord = L.sortBy (O.comparing (iHead)) noNext
              headSort = groupBy (\item1 item2 
                           -> (iHead item1) == (iHead item2))
                         headord
              Just (Left next) = nextSymb $ head nextI
              sortList = groupBy (\item1 item2 
                           -> (nextSymb item1) == (nextSymb item2))
                         nextI
              nexts = map (fromLeft . fromJust . nextSymb . head) sortList
              newItems = concatPart sortList headSort
          in trace "sortList" $ sortList `deepseq` trace "sortList berechnet" 
             trace "nexts" $ nexts `deepseq` trace "nexts berechnet" $
             newItems `deepseq` trace "conc ready -- leaving complete" newItems

concatPart :: [[Item v t]] -> [[Item v t]] -> [Item v t]
concatPart [] _ = trace "concpart ready" []
concatPart _ [] = trace "concpart ready" []
concatPart nexts heads 
  = let nextHead = (iHead $ head $ head heads)
        nextS = (fromLeft $ fromJust $ nextSymb $ head $ head nexts)
    in trace "nextSymbol: " $
       if  nextHead < nextS 
       then concatPart (nexts) (tail heads) 
       else if nextHead > nextS
            then concatPart (tail nexts) (heads) 
            else (complete' (head nexts) (head heads) nextS)
                  ++ concatPart (tail nexts) (tail heads)

complete' :: [Item v t] -> [Item v t] -> Int -> [Item v t]
complete' next noNext symb
  = let list = 
          nub
            [ item
                { nextSymb = listToMaybe newRight -- $ tail $ bRight item
                , bRight =  newRight -- tail $ bRight item
                , stateList = lState : stateList item
                }
            | item <- next
            , item' <- noNext
            , head (stateList item) == firstState item'
            , let newRight = tail $ bRight item
                  lState = head $ stateList item'
            ]
    in list

predict
  :: Ord v
  => BackwardStar v l Int
  -> (l -> [Either Int t])
  -> [[Item v t]]
  -> [Item v t]
predict hg component !new
  = if null new
    then trace "predict ready" []
    else trace "anzahl new: " $ traceShow (length new) $ 
         let nexts = map (fromLeft . fromJust . nextSymb . head) new
             newItems = concat $ map2 (map (predict'  hg component) nexts)  new
         in newItems `deepseq` trace "newItems berechnet"
            newItems

predict'
  :: Ord v
  => BackwardStar v l Int
  -> (l -> [Either Int t])
  -> v
  -> [Item v t]
  -> [Item v t]
predict' hg component !next !vec
    let list = [ Item
                   next
                   s'
                   (ident hgEdge)
                   [st]
                   (listToMaybe s')
                   st
                   1
                | hgEdge <- backStar hg $ next
                , st <- S.toList $ S.fromList $ map (head . stateList) vec 
                , let s = component $ label hgEdge 
                , let s' = map (either (Left . ((!!) $ from hgEdge))  
                                       (Right . id)) s
               ]
    in  (next,vec) `deepseq`
        list `deepseq`
        list

     
scan :: WSA Int t Double -> [Item v t] -> [Item v t]
scan wsa new
  = if null new
    then []
    else let Just (Right next) = nextSymb $ head new
             (nextEq, remain) = partR next new
             scn = (scan' wsa next nextEq)
             newItems = -- traceShow next $ traceShow (V.length $ nextEq) 
                      scn `seq` (++) scn $ scan wsa remain  
         in newItems  
                                    

scan' :: WSA Int t Double -> t -> [Item v t] -> [Item v t]
scan' wsa a vec 
  = let erg = [ item
                  { bRight = if (bRight item == [])
                             then []
                             else tail $ bRight item
                  , nextSymb = listToMaybe $tail $ bRight item
                  , stateList = state : stateList item
                  , weight = transWeight trans * weight item
                  }
              | trans <- transitions wsa
              , item <- vec
              , transTerminal trans == fromIntegral a
              , head (stateList item) == transStateIn trans
              , let state = transStateOut trans
              ]
    in erg

create_hg
  :: Ord v
  => [(Hyperedge (Int, v, Int) l Int, Double)]
  -> (EdgeList (Int, v, Int) l (Int, Int), V.Vector Double)
create_hg [] _  _ = error "The given word is not derivable."
create_hg list
  = (map doIt list, V.fromList theList)
  where
    theList = S.toList $ S.fromList $ snd $ unzip list
    theMap = M.fromList (zip theList [0..])
    doIt (he, w) = mapHEi (\ i -> (i, theMap M.! w) ) he

extract
  :: Item v t
  -> BackwardStar v l Int
  -> (l -> [Either Int t])
  -> WSA Int t Double
  -> v
  -> (Hyperedge (Int, v, Int) l Int, Double)
extract item@Item{bRight = []} hg component wsa iniNode
  = let edge = fromJust $ find (\x -> (ident x) == (iEdge item)) $ edgesEL $ toEdgeList hg
        initial_weight 
          = if (iHead item == iniNode) -- START-SEPARATION REQUIRED
            then [ w
                 | (s, w) <- initialWeights wsa
                 , s == firstState item ]
                 ++ [ w
                    | (s, w) <- finalWeights wsa
                    , s == head $ stateList item
                    ]
            else []
        goal
          = ( mkHyperedge 
                (firstState item, iHead item, head $ stateList item) 
                (carryL (component $ label edge) (reverse $ stateList item) (from edge))
                (label edge) 
                (ident edge)
            , weight item * L.product initial_weight
            )
    in    trace "extract:" 
        $ edge `deepseq` trace "edge berechnet"
        $ initial_weight `deepseq` trace "initial weight berechnet: " 
        $ goal `deepseq` trace "goal berechnet"
        $ trace "ready" goal

carryL :: [Either Int t] -> [Int] -> [v] -> [(Int, v, Int)]
carryL lab wsast vert
  = [ trip | (Left _, trip) <- zip lab (zip3 wsast vert (tail wsast)) ]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False

fromLeft (Left x) = x
fromLeft x = error $ "error while reading " ++ (show x)

partR :: Token -> [Item v t] -> ([Item v t], [Item v t])
partR next new = partition (\x -> nextSymb x == Just (Right next)) new                

map2 [] _ = []
map2 _ [] = []
map2 (f:fs) (x:xs) = (f x) : map2 fs xs  

