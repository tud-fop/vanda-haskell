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


data HyperEdge v l w = HyperEdge
  { eHead   :: v
  , eTail   :: [v]
  , eLabel  :: l
  , eWeight :: w
  }

data HyperGraph v l w = HyperGraph
  { verticesS :: S.Set v
  , edgesM    :: M.Map v [HyperEdge v l w]
  }


hyperGraph es
  = HyperGraph
      (S.fromList . concatMap (\ e ->  eHead e : eTail e) $ es)
      (M.fromListWith (++) . map (\ e -> (eHead e, [e]))  $ es)


hyperEdge = HyperEdge


vertices :: HyperGraph v l w -> [v]
vertices = S.toList . verticesS


edges :: HyperGraph v l w -> [HyperEdge v l w]
edges = concat . M.elems . edgesM


drawHyperEdge (HyperEdge h t l w)
  = show h ++ " -- " ++ show t ++ " | " ++ show l ++ " | " ++ show w


drawHyperGraph g
  = unlines . map drawHyperEdge . edges $ g