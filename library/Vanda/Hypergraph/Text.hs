-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Linda Leuschner 2012
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Hypergraph.Text ( parseHypergraph ) where

import Control.Arrow ( (&&&) )
import qualified Data.Ix as Ix
import qualified Data.Text.Lazy as T

import Vanda.Hypergraph.Basic --( Hyperedge, mkHyperedge, EdgeList )

parseHypergraph
  :: (Ix.Ix v, Read v, Read l) => T.Text -> EdgeList v l Double 
parseHypergraph
  = uncurry EdgeList
  . (nodesL &&& id)
  . map (he . words . T.unpack)
  . T.lines
  where
    he (x1 : (_ : (x3 : xs))) 
      = mkHyperedge (read x1) from' (read x3) wgt
      where (from', wgt) = g xs []
    he _ = undefined
    g [] _ = error "empty list"
    g (_ : []) _ = error "just one element"
    g [_, x2] ys = (reverse ys, read x2)
    g (x1 : xs) ys
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
