-- Copyright (c) 2010, Toni Dietze

module RuleExtraction where

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.Tree as T


extract forest = M.fromListWith (+) . map (flip (,) 1) . extractF $ forest

extractF forest = concatMap extractT forest

extractT (T.Node {T.rootLabel = r, T.subForest = f@(_:_)})
  = (r, map T.rootLabel f):(extractF f)
extractT _ = []