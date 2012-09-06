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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Vanda.Hypergraph.Binary () where

import Control.Applicative ( (<$>), (<*>) )
import Control.Seq
import Control.DeepSeq ( NFData, force, deepseq )
import qualified Data.Binary as B
import Debug.Trace ( trace )

import Vanda.Hypergraph.Basic
import Vanda.Hypergraph.NFData ()

mkHyperedge' x1 x2 x3 x4 = mkHyperedge x1 (x2 `using` seqList rseq) x3 x4

instance (NFData v, NFData l, NFData i, B.Binary v, B.Binary l, B.Binary i, Ord v)
  => B.Binary (Hyperedge v l i) where
  put e = do
    B.put (to e)
    B.put (from e)
    B.put (label e)
    B.put (ident e)
  -- get = mkHyperedge <$> B.get <*> B.get <*> B.get <*> B.get
  get = do
    x1 <- B.get
    x2 <- x1 `deepseq` B.get
    x3 <- x2 `deepseq` B.get
    x4 <- x3 `deepseq` B.get
    x4 `deepseq` return (mkHyperedge x1 x2 x3 x4)
     

myGet :: (NFData v, NFData l, NFData i, B.Binary v, B.Binary l, B.Binary i, Ord v) => B.Get [Hyperedge v l i]
myGet = do
  -- es1 <- fmap {-(`using` seqList rseq)-} force B.get
  es1 <- B.get
  if null es1
    then return []
    else
      es1 `seq` do
        es2 <- myGet
        return (es1 ++ es2)

myPut
  :: (NFData v, NFData l, NFData i, B.Binary v, B.Binary l, B.Binary i, Ord v) => [Hyperedge v l i] -> B.Put
myPut es@[] = B.put es -- ([] :: [Hyperedge v l i])
myPut es = do
  B.put (take 10000 es)
  myPut (drop 10000 es)

instance (NFData v, NFData l, NFData i, B.Binary v, B.Binary l, B.Binary i, Ord v)
  => B.Binary (EdgeList v l i) where
  put (EdgeList vs es) = do
    B.put vs
    B.put es -- myPut es
  get = EdgeList <$> B.get <*> B.get -- myGet

