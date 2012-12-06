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

module Vanda.Hypergraph.IntBinary () where

import Control.DeepSeq ( NFData, deepseq )
import qualified Data.Binary as B
import qualified Data.Set as S

import Vanda.Hypergraph.IntHypergraph
import qualified Vanda.Hypergraph.Tree as T


instance (NFData l, NFData i, B.Binary l, B.Binary i)
  => B.Binary (Hyperedge l i) where
  put e = do
    B.put (to e)
    B.put (from e)
    B.put (label e)
    B.put (ident e)
  get = do
    x1 <- B.get
    x2 <- x1 `deepseq` B.get
    x3 <- x2 `deepseq` B.get
    x4 <- x3 `deepseq` B.get
    x4 `deepseq` return $! mkHyperedge x1 x2 x3 x4
     

instance (NFData l, NFData i, B.Binary l, B.Binary i)
  => B.Binary (Hypergraph l i) where
  put (Hypergraph vs es) = do
    B.put (S.fromList $ enumFromTo 0 $ vs - 1)
    B.put es -- myPut es
  get = do
    vs <- fmap ((+ 1) . snd . nodesL . S.toList) (B.get :: B.Get (S.Set Int))
    es <- vs `seq` B.get
    es `seq` return (Hypergraph vs es)

instance B.Binary l => B.Binary (T.Tree l) where
  get = do
          x1 <- B.get
          x2 <- B.get
          return $! T.node x1 x2
  put t = B.put (T.rootLabel t) >> B.put (T.subForest t)
