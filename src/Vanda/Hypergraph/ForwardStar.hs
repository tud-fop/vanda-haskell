-- (c) 2012 Matthias Büchse <Matthias.Buechse@tu-dresden.de>
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
-- Maintainer  :  Matthias Buechse
-- Stability   :  unknown
-- Portability :  portable
--
-- This module makes 'ForwardStar' a 'Hypergraph' instance.
module Vanda.Hypergraph.ForwardStar
  ( module Vanda.Hypergraph.Basic
  , edgeCount
  , filterEdges
  , mapLabels
  , mapNodes
  , memoize
  , toEdgeList
  ) where

import Control.Arrow ( (***), (&&&) )
import qualified Data.Array as A
import qualified Data.Ix as Ix
import qualified Data.Map as M
import qualified Data.Vector as V

import Vanda.Hypergraph.Basic

edgeCount (ForwardStar sts lst f _)
  = (length lst+) $ sum $ map (length . f) (Ix.range sts)
filterEdges p (ForwardStar vs lst f _)
  = ForwardStar vs (filter p lst) (filter p . f) False
mapLabels g (ForwardStar vs lst f _)
  = ForwardStar vs (map g lst) (map g . f) False
mapNodes g (ForwardStar vs lst f _)
  = ForwardStar vs' (map (mapHE g) lst) (a A.!) True
  where
    vs' = (g *** g) vs
    a = A.array vs' [ (g v, map (mapHE g) (f v)) | v <- Ix.range vs ]
memoize fs@(ForwardStar vs lst f mem)
  | mem = fs -- idempotent
  | otherwise = ForwardStar vs lst (a A.!) True
  where
    a = A.array vs [ (v, f v) | v <- Ix.range vs ]
toEdgeList (ForwardStar sts lst f _)
  = EdgeList sts $ lst ++ concatMap f (Ix.range sts)


