{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Hypergraph.Recognize
-- Copyright   :  (c) Technische Universität Dresden 2016
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  sebastian.mielke@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Hypergraph.Recognize
  ( recognizeTree
  , totalProbOfTree
  ) where

import qualified Vanda.Hypergraph as H

import           Data.Foldable (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Tree

recognizeTree
  :: forall v l i h. (Ord v, Ord l, H.Hypergraph h) -- Ord v is needed for the initsMap, Ord l for efficient HE nubbing
  => (h v l i, M.Map v i) -- ^ Hypergraph and initial states
  -> Tree l               -- ^ terminal/label tree to be recognized
  -> [H.Derivation v l i] -- ^ list of all possible derivations
recognizeTree (hg, initsMap)
  = downwardPass . upwardPass
  where
    (H.ForwardStar _ nes fs _) = H.toForwardStar hg
    
    -- Given a list of admissible states of each child and the label of a node,
    -- see if that edge matches.
    acceptableEdge :: [[v]] -> l -> H.Hyperedge v l i -> Bool
    acceptableEdge css l he
      = let ruleStates = H.from he
            labelOk = l == H.label he
            lengthOk = length ruleStates == length css
            statesOk = and $ zipWith elem ruleStates css
        in labelOk && lengthOk && statesOk
    
    -- Annotates all Nodes with possibly applicable rules in a bottom-up way.
    -- This way the existence of a rule at some nodes guarantees that we can
    -- complete a derivation going further down the tree, but not up!
    upwardPass :: Tree l -> Tree [H.Hyperedge v l i]
    upwardPass (Node l [])
      = Node (filter ((==l) . H.label) nes) []
    upwardPass (Node l cs)
      = let recognizedChildren = map upwardPass cs
            -- for every child get possible states :: [[v]]
            acceptableChildStates = map (map H.to . rootLabel) recognizedChildren
            -- stupid superset of admissible rules: those that come from any state the first child might have:
            allEdges = nubHEs
                     $ concatMap fs
                     $ head acceptableChildStates
            -- check that they are also admissible for all (other) children
            okEdges = filter (acceptableEdge acceptableChildStates l)
                    $ allEdges
        in Node okEdges recognizedChildren
    
    -- Here we actually build derivations out of all possible rules that were
    -- added to the tree in the upward pass. Every rule at the root of our tree
    -- will yield (at least) one derivation, but we also initially restrict them
    -- to those that start with a legal inital state.
    downwardPass :: Tree [H.Hyperedge v l i] -> [Tree (H.Hyperedge v l i)]
    downwardPass node
      = concatMap (branchIntoDerivations node) $ M.keys initsMap
    branchIntoDerivations :: Tree [H.Hyperedge v l i] -> v -> [Tree (H.Hyperedge v l i)]
    branchIntoDerivations (Node edges cs) v
      = [ Node aEdge newChildren
        | aEdge <- edges
        , H.to aEdge == v
        , newChildren <- sequence $ zipWith branchIntoDerivations cs (H.from aEdge)
        ]

newtype HEwrapper v l i = HEwrapper { getWrappedHE :: H.Hyperedge v l i }

-- Similar to the Hyperedge Eq instance, we will ignore the possibility of
-- different idents for otherwise equal hyperedges for these instances.
instance (Eq v, Eq l) => Eq (HEwrapper v l i) where
  (HEwrapper he1) == (HEwrapper he2) = he1 == he2

instance (Ord v, Ord l) => Ord (HEwrapper v l i) where
  compare (HEwrapper he1) (HEwrapper he2)
    = let unwrap he = (H.from he, H.to he, H.label he)
      in compare (unwrap he1) (unwrap he2)

nubHEs
  :: (Ord v, Ord l)
  => [H.Hyperedge v l i]
  -> [H.Hyperedge v l i]
nubHEs
  = map getWrappedHE
  . S.toAscList
  . S.fromList
  . map HEwrapper

totalProbOfTree
  :: (Ord v, Ord l, H.Hypergraph h)
  => (h v l Double, M.Map v Double) -- ^ Hypergraph and initial states
  -> Tree l                         -- ^ terminal/label tree to be recognized
  -> Double                         -- ^ sum of all possible derivation scores
totalProbOfTree (hg, inits) t
  = sum
  $ map scoreTree
  $ recognizeTree (hg, inits) t
  where
    scoreTree t
      = (*(inits M.! (H.to $ rootLabel t)))
      $ foldl' (\ acc he -> acc * H.ident he) 1 t
