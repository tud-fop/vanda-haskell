module Data.HyperGraph
  ( HyperGraph(), hyperGraph
  , HyperEdge(), hyperEdge
  , vertices, verticesS
  , edges, edgesM
  , eHead, eTail, eLabel, eWeight
  , drawHyperGraph, drawHyperEdge
  ) where


import qualified Data.Map as M
import qualified Data.Set as S


data HyperEdge v l s = HyperEdge
  { eHead   :: v
  , eTail   :: [v]
  , eLabel  :: l
  , eWeight :: s
  }

data HyperGraph v l s = HyperGraph
  { verticesS :: S.Set v
  , edgesM    :: M.Map v [HyperEdge v l s]
  }


hyperGraph es
  = HyperGraph
      (S.fromList . concatMap (\ e ->  eHead e : eTail e) $ es)
      (M.fromListWith (++) . map (\ e -> (eHead e, [e]))  $ es)


hyperEdge = HyperEdge


vertices :: HyperGraph v l s -> [v]
vertices = S.toList . verticesS


edges :: HyperGraph v l s -> [HyperEdge v l s]
edges = concat . M.elems . edgesM


drawHyperEdge (HyperEdge h t l w)
  = show h ++ " -- " ++ show t ++ " | " ++ show l ++ " | " ++ show w


drawHyperGraph g
  = unlines . map drawHyperEdge . edges $ g