-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Linda Leuschner 2010
-- License     :  BSD-style
-- Maintainer  :  Linda Leuschner <linda.leuschner@tu-dresden.de>
--
-- Stability   :  unknown
-- Portability :  portable
--
-- This module computes 'Hypergraph' out of a 'Hypergraph' and a 'WSA'.
-- The resulting 'Hypergraph' will only recognize the given word.
-- This implementation uses the Early and the Bar-Hille algorithm.
--
-----------------------------------------------------------------------------


module Vanda.Algorithms.Earley.Earley () where

import Data.Tree as T
import Control.Applicative

import Vanda.Hypergraph


data Item  = Item
    { prodHead :: Int
    , prodLeft :: [Either Int Int]
    , prodRight:: [Either Int Int]
    , edge     :: Hyperedge Int (T.Tree (Either Int Int), [Either Int Int]) Int
    , stateList:: [Int]
    }

data State = State
    {  actual :: Item
     , done   :: [Item]
     , trans  
        :: BackwardStar Int (T.Tree (Either Int Int), [Either Int Int]) Int
    }
    
earley
  ::BackwardStar Int (T.Tree (Either Int Int), [Either Int Int]) Int
  -> Int                      -- an initial node of the Hypergraph  
  -> [Int]                    -- the encoded string
  -> BackwardStar (Int,Int,Int) 
                  (T.Tree (Either Int Int), 
                    [Either Int Int], [(Int,Int,Int)]) 
                  (Int,Int)
earley hg iniNode word 
  = let trans = map (maybe [] pure (extractTransition x n )) 
                $ iter $ initState hg iniNode $ length word
    in create_hg trans;
                 

iter :: State -> State
  -- -> (Item -> Int -> Maybe (Hyperedge  (Int,Int,Int) 
                           -- (T.Tree (Either Int Int),[Either Int Int]) 
                           -- (Int,Int))
     -- )
     --State
  -- -> [Hyperedge  (Int,Int,Int) 
                 -- (T.Tree (Either Int Int), [Either Int Int])
                 -- (Int,Int)
     -- ]
iter (State [] _ _) = [] 
iter s@(State act ready hg)
      = iter s{done = predict act : ready}    
initState
  :: BackwardStar Int (T.Tree (Either Int Int), [Either Int Int]) Int
  -> Int      -- ^ an initial node of the Hypergraph. 
  -> Int      -- ^ the length of the word
  -> State
initState hg iniNode n 
  = State [Item iniNode 
                [] 
                (map (either (Left . id) (Right  . ((!!) $ from edge)) )  
                  (snd $ label edge)) 
                edge 
                list 
          | edge <- (backStar hg iniNode),
            list <- combine 0 n (map (either id id) (snd $ label edge))
          ]
          []
          hg

extractTransition
  :: Item 
  -> Int
  -> Maybe (Hyperedge  (Int,Int,Int) 
                (T.Tree (Either Int Int), [Either Int Int]) 
                (Int,Int))
extractTransition
    (Item {  prodRight = []
           , prodLeft = left
           , prodHead = v
           , edge = hEdge
           , stateList = list 
         })
    n
    = Just $ mkHyperedge v 
                         (zip3 (init list) 
                               (map (either id id) left)
                               (tail list)) 
                         (label hEdge) 
                         (ident hEdge,n)                       
extractTransition 
  _ _= Nothing 
  
complete = undefined

predict :: State -> [Item]
predict State [] _ _ 
    = undefined -- should not happen
predict State actual _ hg 
    = map (\x -> predict' x (backStar hg $ snd $ prodHead x)) actual

predict' 
    :: Item 
    -> [Hyperedge Int (T.Tree (Either Int Int), [Either Int Int]) Int]
    -> [Item]
predict' item bs 
    =[Item (last $ stateList item, snd $ prodHead item,last $ stateList item)
           []
           (from edge)
           edge
           [last $ stateList item]
      ]

create_hg = undefined          


-- | Create a list of all chains (p_0,q_1,p_1)(p_1,q_2,p_2)...(p_k-1,q_k,p_k)
combine :: Int -> Int -> [Int] -> [[Int]]
-- combine i j [] = []
combine i j (x:[]) = [[i,j]]
combine i j (x:xs)
  = concat [map (\b -> i : b) (combine k j xs) | k <- [i..j]]

          
{-
proj_left:: (T.Tree (Either Int Int), [Either Int Int]) -> T.Tree (Either Int Int)
proj_left = fst;

proj_right:: (T.Tree (Either Int Int), [Either Int Int]) -> T.Tree (Either Int Int)
proj_right (left,right) = right;
-}
