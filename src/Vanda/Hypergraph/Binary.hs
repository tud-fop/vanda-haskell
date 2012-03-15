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

import Vanda.Hypergraph.Basic

instance (B.Binary v, B.Binary l, B.Binary i)
  => B.Binary (Hyperedge v l i) where
  put e = do
    B.put (to e)
    B.put (from e)
    B.put (label e)
    B.put (ident e)
  get = mkHyperedge <$> B.get <*> B.get <*> B.get <*> B.get

myGet :: (B.Binary v, B.Binary l, B.Binary i) => B.Get [Hyperedge v l i]
myGet = do
  es1 <- B.get
  if null es1
    then return []
    else
      do
        es2 <- myGet
        es1 `seq` return (es1 ++ es2)

myPut
  :: (B.Binary v, B.Binary l, B.Binary i) => [Hyperedge v l i] -> B.Put
myPut es@[] = B.put es -- ([] :: [Hyperedge v l i])
myPut es = do
  B.put (take 10000 es)
  myPut (drop 10000 es)

instance (B.Binary v, B.Binary l, B.Binary i)
  => B.Binary (EdgeList v l i) where
  put (EdgeList vs es) = do
    B.put vs
    myPut es
  get = EdgeList <$> B.get <*> myGet

