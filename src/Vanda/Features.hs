-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Features
-- Copyright   :  (c) Technische Universität Dresden 2012
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Matthias.Buechse@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Defines the Feature type, feature evaluation on derivation trees,
-- and composition (product) for features. Note that one Feature in our
-- setting here can compute several features (the type is closed under
-- product).
--
-----------------------------------------------------------------------------

module Vanda.Features (
  -- * Feature type
    Feature (..)
  -- * Evaluation
  , processDerivation
  , evalDerivation
  , inner
  -- * Best derivations
  , Candidate
  , BestArray
  , topCC
  -- * Composition
  , (+++)
  , projLeft
  , projRight
  , product
  ) where

import Prelude hiding ( product )
import Control.Arrow ( (***), (&&&) )
import qualified Data.Array as A
import qualified Data.Tree as T
import qualified Data.Vector as V

import Vanda.Hypergraph.Basic

-- | A Feature with label type @l@, id type @i@, and data type @x@;
-- the data type is for intermediate calculations when processing
-- a derivation tree using @processNode@. It is converted into a
-- 'Vector' of 'Double' at the root using @finalize@.
data Feature l i x = Feature
  { -- | Processes a node of a derivation tree using the (label, id) pair
    -- of that node and a list of intermediate results for the successors.
    processNode :: (l, i) -> [x] -> x
    -- | Converts the intermediate result at the root of a derivation tree
    -- into a 'Vector' of 'Double'.
  , finalize :: x -> V.Vector Double
  }

-- | Composes two features. The resulting feature computes both input features
-- and returns the concatenation of the respective vectors at the root.
(+++) :: Feature l i x1 -> Feature l i x2 -> Feature l i (x1,x2) 
Feature pN1 f1 +++ Feature pN2 f2
  = Feature
    (\ li -> (pN1 li *** pN2 li) . unzip)
    (uncurry (V.++) . (f1 *** f2))

-- | Lifts a feature with id type @i@ to @(i, i')@.
projLeft :: Feature l i x -> Feature l (i, i') x
projLeft (Feature pN f) = Feature (pN . (fst &&& fst . snd)) f

-- | Lifts a feature with id type @i'@ to @(i, i')@.
projRight :: Feature l i' x -> Feature l (i, i') x
projRight (Feature pN f) = Feature (pN . (fst &&& snd . snd)) f

-- | Composes two features with id types @i@ and @i'@, respectively.
-- That is, the features are lifted to @(i,i')@ via 'projLeft' and
-- 'projRight', respectively, and then composed via '+++'.
product :: Feature l i1 x1 -> Feature l i2 x2 -> Feature l (i1,i2) (x1,x2)
product = curry $ (uncurry (+++)) . (projLeft *** projRight)

{-- | Processes a hyperedge with a given feature.
processEdge :: Feature l i x -> Hyperedge v l i -> [x] -> x
processEdge (Feature pN f) (Hyperedge _ _ l i) = pN (l, i) --}

-- | Processes a derivation tree with a given feature.
processDerivation :: Feature l i x -> Derivation v l i -> x
processDerivation feat (T.Node (Hyperedge _ _ l i) ds)
  = processNode feat (l, i) $ map (processDerivation feat) ds

-- | Evaluates a derivation tree with a given feature. That is,
-- it processes the tree via 'processTree' and then applies the
-- 'finalize' mapping of the feature.
evalDerivation :: Feature l i x -> Derivation v l i -> V.Vector Double
evalDerivation = (uncurry (.)) . (finalize &&& processDerivation)
--evalDerivation feat = finalize feat . processDerivation feat

-- | Computes the inner product of two 'Vector's of 'Double's.
inner :: V.Vector Double -> V.Vector Double -> Double
inner = curry $ V.foldl (+) 0 . (uncurry $ V.zipWith (*))

-- | A candidate derivation, consisting of its alleged weight (recall that
-- we may not be at the root yet), the derivation itself, and its feature
-- data value.
type Candidate v l i x = (Double, (Derivation v l i, x))

-- | An array of best derivations for each node.
type BestArray v l i x = A.Array v [Candidate v l i x]

-- | Top concatenation for derivation candidates.
topCC
  :: Feature l i x        -- ^ 'Feature' mapping
  -> V.Vector Double      -- ^ weight vector
  -> Hyperedge v l i      -- ^ 'Hyperedge' for top concatenation
  -> [Candidate v l i x]  -- ^ successor candidates
  -> Candidate v l i x    -- ^ resulting candidate
topCC feat wV e
  = (inner wV . finalize feat . snd &&& id)
  . (T.Node e . map (fst . snd)
    &&& processNode feat (label e, i e) . map (snd . snd))
