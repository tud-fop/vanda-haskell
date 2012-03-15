-- | Makes Hyperedge an instance of 'NFData' for strict evaluation.
module Vanda.Hypergraph.NFData () where

import Control.DeepSeq
import qualified Data.Vector as V

import Vanda.Hypergraph.Basic

instance (NFData v, NFData l, NFData i) => NFData (Hyperedge v l i) where
  rnf (Nullary t l i) = rnf t `seq` rnf l `seq` rnf i
  rnf (Unary t f1 l i) = rnf t `seq` rnf f1 `seq` rnf l `seq` rnf i
  rnf (Binary t f1 f2 l i)
    = rnf t `seq` rnf f1 `seq` rnf f2 `seq` rnf l `seq` rnf i
  rnf (Hyperedge t f l i)
    = rnf t `seq` V.map rnf f `seq` rnf l `seq` rnf i

instance (NFData v, NFData l, NFData i) => NFData (EdgeList v l i) where
  rnf (EdgeList vs es) = rnf vs `seq` rnf es
