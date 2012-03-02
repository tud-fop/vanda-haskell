-- (c) 2012 Linda Leuschner <Leuschner.Linda@mailbox.tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}  -- for 'Stream'

module Vanda.Hypergraph.Text where

import Control.Arrow ( (&&&), first )
import Data.Int ( Int8 )
import qualified Data.Ix as Ix
import qualified Data.List as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Vanda.Hypergraph.Basic --( Hyperedge, mkHyperedge, EdgeList )

import Debug.Trace

-- mytrace x = trace (show x) x
mytrace = id

parseHypergraph
  :: (Ix.Ix v, Read v, Read l) => T.Text -> EdgeList v l Double 
parseHypergraph
  = uncurry EdgeList
  . (nodesL &&& id)
  . map (he . words . T.unpack)
  . T.lines
  
he (x1:(x2:(x3:xs))) 
  = mkHyperedge 
    (read x1) 
    from'
    (read x3)
    wgt
  where (from', wgt) = g xs []

g [] _ = error "empty list"
g (x:[]) _ = error "just one element"
g [x1, x2] ys = (reverse ys, read x2)
g (x1:xs) ys
  = let y = read x1
    in y `seq` g xs (y:ys) -- first ((read $ T.unpack x1):) (g xs)
      
  
  
  
  
  
{-  
  
  =
       (\list 
      -> mkHyperedge 
         (read $ mytrace $ T.unpack $ list !! 0) 
         (map (read . mytrace . T.unpack) $ init $ init $ drop 3 list)
         (read $ mytrace $ T.unpack $ list !! 2)
         (read $ mytrace $ T.unpack $ L.last list)
     )
     
  -}
