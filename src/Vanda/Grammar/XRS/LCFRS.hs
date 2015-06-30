module Vanda.Grammar.XRS.LCFRS where

import qualified Data.Vector as V

import           Data.NTT
import           Vanda.Hypergraph.IntHypergraph


type Rule = ((Int, [Int]), [[NTT]])

data MIRTG -- Mono-IRTG! I should not be allowed to name things.
  = MIRTG
    { rtg :: Hypergraph Int Int
      -- Int as identification for the homomorphism (label)
      -- and for the rule weights (ident)
    , initial :: [Int] -- these are nodes of the hypergraph (NTs)
    , h :: V.Vector (V.Vector (V.Vector NTT))
      -- Outer vector lets me map rules (using their Int-label) to the
      -- Middle vector, which represents the components of the lhs-NT,
      -- its length is that NTs fan-out
      -- Inner vector represents the concatenation of Ts and variables
      -- (linearly adressable, thus Int)
      -- The NTTs Ts are indeed the Ts, the NTs however are the variables
      -- (zero-indexed)
    }

data MXRS
  = MXRS
    { irtg :: MIRTG
    , weights :: V.Vector Double
    }

instance Show MXRS where
  show (MXRS (MIRTG hg _ h') w)
    = unlines
    . map (\ he -> (cut 2 . show . to $ he)
                ++ " <- "
                ++ (cut 10 . show . from $ he)
                ++ " # "
                ++ (cut 5 . show . (V.!) w . ident $ he)
                ++ " || "
                ++ (show . (V.!) h' . label $ he)
          )
    . edges
    $ hg

cut :: Int -> [Char] -> [Char]
cut n = take n . (++ repeat ' ')

getMIRTGFromRules
  :: [Rule]
  -> [Int] -- ^ initials
  -> MIRTG
getMIRTGFromRules rules initials =
  let myHyperedges = map (\(((lhs, rhs), _), i) -> mkHyperedge lhs rhs i i)
                   $ zip rules [0..]
      myH = V.fromList $ map (V.fromList . map V.fromList . snd) rules
  in MIRTG (mkHypergraph myHyperedges) initials myH

getMXRSFromProbabilisticRules
  :: [(Rule, Double)] -- ^ rules and their probabilities
  -> [Int] -- ^ initial NTs
  -> MXRS
getMXRSFromProbabilisticRules rs initials =
  MXRS (getMIRTGFromRules (map fst rs) initials) (V.fromList $ map snd rs)

toProbabilisticRules
  :: MXRS
  -> [(Rule, Double)]
toProbabilisticRules (MXRS (MIRTG hg _ h') ws)
  = map worker
  $ zip3 (edges hg) -- assuming edges are sorted by ident!
         (V.toList ws)
         (V.toList $ fmap (V.toList . fmap V.toList) h')
  where
    worker (he, d, h'') = (((to he, from he), h''), d)
