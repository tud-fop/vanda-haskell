-- Copyright (c) 2010, Toni Dietze

module RuleExtraction where

import qualified Data.WTA as WTA

import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.Tree as T

extractWTA forest
  = WTA.properize
  . flip WTA.create []
  . map (\((x, xs), c) -> WTA.Transition x x xs c)
  . M.toList
  . extract
  $ forest

extract forest = M.fromListWith (+) . map (flip (,) 1) . extractF $ forest

extractF forest = concatMap extractT forest

extractT (T.Node {T.rootLabel = r, T.subForest = f@(_:_)})
  = (r, map T.rootLabel f):(extractF f)
extractT _ = []
