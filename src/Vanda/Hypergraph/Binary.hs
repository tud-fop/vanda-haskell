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

module Vanda.Hypergraph.Binary () where

import Control.Applicative ( (<$>), (<*>) )
import qualified Data.Binary as B
import qualified Data.Vector as V

import Vanda.Hypergraph.Basic

instance (B.Binary v, B.Binary l, B.Binary i)
  => B.Binary (Hyperedge v l i) where
  put (Hyperedge t f l i) = do
    B.put t
    B.put $ V.toList f
    B.put l
    B.put i
  get = mkHyperedge <$> B.get <*> B.get <*> B.get <*> B.get

instance (B.Binary v, B.Binary l, B.Binary i)
  => B.Binary (EdgeList v l i) where
  put (EdgeList vs es) = do
    B.put vs
    B.put es
  get = EdgeList <$> B.get <*> B.get

