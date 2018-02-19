-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Util.Histogram
-- Copyright   :  (c) Technische Universität Dresden 2014
-- License     :  BSD-style
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Vanda.Util.Histogram where


import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)


histogram :: Ord k => [k] -> Map k Int
histogram = M.fromListWith (+) . map (\ k -> (k, 1))
