module Vanda.Algorithms.MATLearner.TreeAutomaton where
import Vanda.Hypergraph hiding (to,from,label, edges, from1, from2)
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Tree
import Vanda.Algorithms.MATLearner.TreesContexts


data Automaton a = 
  Automaton (EdgeList a String Int) (S.Set a)
  
instance (Show a) => Show (Automaton a) where
  show (Automaton (EdgeList _ edgelist) finalStates) = "Edges:\n" ++ showEdges edgelist ++ "Final States: " ++ show (S.toAscList finalStates)
    where showEdges :: (Show a) => [Hyperedge a String Int] -> String
          showEdges [] = ""
          showEdges ((Hyperedge to from label _) :xs) = show (V.toList from) ++ " -> " ++ show label ++ " " ++ show to ++ "\n" ++ showEdges xs
          showEdges ((Nullary _ _ _ ) : _) = undefined
          showEdges ((Unary _ _ _ _ ) : _) = undefined
          showEdges ((Binary _ _ _ _ _ ) : _) = undefined
 
-- | Computes the state of the automaton after running over the tree. Only works if the automaton is bottom up deterministic and total!
run :: (Eq a) => [Hyperedge a String Int] -> Tree String -> a
run transitions (Node label children) = computeState childrenStates label transitions
  where childrenStates = map (run transitions) children

-- | Selects a Hyperedge from the List of Hyperedges that can be applied to the childrenStates with nodeLabel
computeState :: (Eq a) => [a] -> String -> [Hyperedge a String Int] -> a
computeState _ _ [] = error "Automaton has to be total" -- only happens if the automaton is partial
computeState childrenStates nodeLabel ((Hyperedge to from label _):edges)
  | nodeLabel == label && (V.toList from) == childrenStates = to
  | otherwise = computeState childrenStates nodeLabel edges 
computeState _ _ ((Nullary _ _ _ ) : _) = undefined
computeState _ _ ((Unary _ _ _ _ ) : _) = undefined
computeState _ _ ((Binary _ _ _ _ _ ) : _) = undefined

-- | Computes whether the Automaton accepts a Tree or not
accepts :: (Ord a) => Automaton a -> Tree String -> Bool
accepts (Automaton (EdgeList _ edges) finalStates) tree = S.member rootState finalStates
  where rootState = run edges tree

-- | Returns the complement of an automaton by turning every final state in anon final state and vice versa
complement :: (Ord a) => Automaton a -> Automaton a
complement (Automaton (EdgeList states edges) finalStates) = Automaton (EdgeList states edges) (states `S.difference` finalStates)

-- | Computes the intersection of two automata
intersect :: (Ord a, Ord b) => Automaton a -> Automaton b -> Automaton (a,b)
intersect (Automaton (EdgeList states1 edges1) finalStates1) 
             (Automaton (EdgeList states2 edges2) finalStates2) 
             = Automaton (EdgeList (makePairs states1 states2) 
                                   (makePairEdges edges1 edges2)) 
                         (makePairs finalStates1 finalStates2)

-- | Computes the union of two automata
unite :: (Ord a, Ord b) => Automaton a -> Automaton b -> Automaton (a,b)
unite (Automaton (EdgeList states1 edges1) finalStates1) 
             (Automaton (EdgeList states2 edges2) finalStates2) 
             = Automaton (EdgeList (makePairs states1 states2) 
                                   (makePairEdges edges1 edges2)) 
                         (S.union (makePairs finalStates1 states2) (makePairs states1 finalStates2))


makePairEdges :: [Hyperedge a String Int] -> [Hyperedge b String Int] -> [Hyperedge (a,b) String Int]
makePairEdges l1 l2 = [ Hyperedge (x,y) (V.zip from1 from2) label 0 | Hyperedge x from1 label _ <- l1, Hyperedge y from2 label' _ <- l2, label == label' ]

makePairs :: (Ord a,Ord b) => S.Set a -> S.Set b -> S.Set (a,b)
makePairs set1 set2 = S.fromList $ [(x,y) | x <- (S.toList set1), y <- (S.toList set2)]

getAlphabet :: Automaton a -> [(String, Int)]
getAlphabet (Automaton (EdgeList _ edges) _) = S.toList $ S.fromList [(label,length $ V.toList  from) | Hyperedge _ from label _ <- edges ]


-- | Checks whether the Automaton accepts any Tree and returns such a Tree
isEmpty :: (Ord a) => Automaton a -> Maybe (Tree String)
isEmpty automaton@(Automaton (EdgeList _ edges) finalStates) = go initTrees initTrees
    where
        sigma = getAlphabet automaton
        initTrees = map (\tree -> (tree,run edges tree)) (getAllTrees [] sigma [X])
        
        --go :: Ord t => [(Tree Int, t)] -> [(Tree Int, t)] -> Maybe (Tree Int)
        go []                _          = Nothing
        go ((tree,state):xs) ys
            |S.member state finalStates = Just tree
            |True                       = go (xs ++ newStates) (ys ++ newStates)
                where
                    newTrees = map (\t -> (t,run edges t)) (map (concatTree tree) (getContexts (map fst ys) sigma))
                    newStates = filter (\(_,nstate) -> not $ any (\(_,oldstate) -> oldstate == nstate) ys) newTrees