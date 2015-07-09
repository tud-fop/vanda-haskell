module Vanda.Grammar.XRS.LCFRS
( Rule
, getRk
, getFo
, getRhs
, PLCFRS
-- The following instances are just there to integrate with other vanda things,
-- for example the stuff in LCFRS.Evaluation
, MIRTG(..)
, MXRS(..)
-- , getMXRSFromProbabilisticRules
-- , toProbabilisticRules
, niceStatictics
) where

import qualified Data.Array as A
import           Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import           Text.Printf (printf)

import           Data.NTT
import           Vanda.Hypergraph.IntHypergraph


type Rule = ((Int, [Int]), [[NTT]])

-- These are kept vague to allow ProtoRules in Binarize!
getRk, getFo :: (((l, [a]), [[NTT]]), Double) -> Int
getRk (((_, rhs),  _), _) = length rhs
getFo (((_, _  ), h'), _) = length h'

getRhs :: (((l, [a]), [[NTT]]), Double) -> [a]
getRhs (((_, rhs), _), _) = rhs

-- | Intial NTs, a map from each NT to a list of possible (intified)
-- rules with their probabilities and a NT and T dictionary.
type PLCFRS = ([Int], [(Rule, Double)], (A.Array Int String, A.Array Int String))

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
    where cut n = take n . (++ repeat ' ')

getMIRTGFromRules
  :: [Int] -- ^ initials
  -> [Rule]
  -> MIRTG
getMIRTGFromRules initials rules =
  let myHyperedges = map (\(((lhs, rhs), _), i) -> mkHyperedge lhs rhs i i)
                   $ zip rules [0..]
      myH = V.fromList $ map (V.fromList . map V.fromList . snd) rules
  in MIRTG (mkHypergraph myHyperedges) initials myH

getMXRSFromProbabilisticRules
  :: [Int] -- ^ initial NTs
  -> [(Rule, Double)] -- ^ rules and their probabilities
  -> MXRS
getMXRSFromProbabilisticRules initials rs =
  MXRS (getMIRTGFromRules initials (map fst rs)) (V.fromList $ map snd rs)

toProbabilisticRules
  :: MXRS
  -> ([Int], [(Rule, Double)])
toProbabilisticRules (MXRS (MIRTG hg inits h') ws)
  = (,) inits
  $ map worker
  $ zip3 (edges hg) -- assuming edges are sorted by ident...
         (V.toList ws) -- so that we can just zip this...
         (V.toList $ fmap (V.toList . fmap V.toList) h') -- ...and this.
  where
    worker (he, d, h'') = (((to he, from he), h''), d)

niceStatictics
  :: PLCFRS
  -> String
niceStatictics (initials, rulesAndProbs, (a_nt, _)) =
  "\n"
  ++ (printf "%7d initial NTs\n" $ length initials)
  ++ (printf "%7d NTs total\n" $ length (A.indices a_nt))
  ++ (printf "%7d rules\n" $ length rulesAndProbs)
  ++ "\n"
  ++ "Ranks:\n"
  ++ (unlines (map (uncurry (printf "%2d: %7d")) rkCounts))
  ++ "Fanouts:\n"
  ++ (unlines (map (uncurry (printf "%2d: %7d")) foCounts))
  where
    counter f = foldl' (\m r -> M.insertWith (+) (f r) 1 m)
                       (M.empty :: M.Map Int Int)
                       rulesAndProbs
    populateHoles m = foldl' (\m k -> M.insertWith (+) k 0 m)
                             m
                             [0.. (fst $ M.findMax m)]
    rkCounts = M.assocs $ populateHoles $ counter getRk
    foCounts = M.assocs $ populateHoles $ counter getFo
