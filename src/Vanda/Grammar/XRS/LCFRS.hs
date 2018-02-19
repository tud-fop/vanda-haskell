-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Sebastian Mielke 2015
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Grammar.XRS.LCFRS
  ( Rule
  , PLCFRS
    -- * IRTG view on LCFRS
    -- The following instances are just there to integrate with other vanda things,
    -- for example the stuff in LCFRS.Evaluation
  , MIRTG(..)
  , MXRS(..)
    -- * Identifier
  , NTIdent
  , TIdent
  , CompFuncEntry
  , Fanout
    -- * getter
  , getRk
  , getFo
  , getRhs
    -- * conversion
  , fromProbabilisticRules
  , toProbabilisticRules
    -- * pretty printing
  , showPLCFRS
  , niceStatictics
    -- * examples
  , exampleMIRTG
  , exampleMXRS
  , examplePLCFRS
    -- * plumbing
  , getNonterminalFanout
  ) where

import qualified Data.Array as A
import           Data.Foldable (foldl')
import           Data.List (intercalate)
import qualified Data.Map.Strict as M
import           Data.Maybe (listToMaybe)
import qualified Data.Vector as V
import           Text.Printf (printf)

import           Data.NTT
import           Vanda.Hypergraph.IntHypergraph


type NTIdent = Int
type TIdent = Int
type CompFuncEntry = NTT
type Fanout = Int
type Rule = ((NTIdent, [NTIdent]), [[CompFuncEntry]])

-- These are kept vague to allow ProtoRules in Binarize!
getRk :: (((l, [a]), [[CompFuncEntry]]), Double) -> Int
getRk (((_, rhs),  _), _) = length rhs
getFo :: (((l, [a]), [[CompFuncEntry]]), Double) -> Fanout
getFo (((_, _  ), h'), _) = length h'

getRhs :: (((l, [a]), [[CompFuncEntry]]), Double) -> [a]
getRhs (((_, rhs), _), _) = rhs

-- | Initial NTs, a map from each NT to a list of possible (intified)
-- rules with their probabilities and a NT and T dictionary.
type PLCFRS = ([NTIdent], [(Rule, Double)], (A.Array NTIdent String, A.Array TIdent String))

data MIRTG -- Mono-IRTG! I should not be allowed to name things.
  = MIRTG
    { rtg :: Hypergraph Int Int
      -- Int as identification for the homomorphism (label)
      -- and for the rule weights (ident)
    , initial :: [NTIdent] -- these are nodes of the hypergraph (NTs)
    , h :: V.Vector (V.Vector (V.Vector CompFuncEntry))
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

fromRules
  :: [NTIdent] -- ^ initials
  -> [Rule]
  -> MIRTG
fromRules initials rules =
  let myHyperedges = map (\(((lhs, rhs), _), i) -> mkHyperedge lhs rhs i i)
                   $ zip rules [0..]
      myH = V.fromList $ map (V.fromList . map V.fromList . snd) rules
  in MIRTG (mkHypergraph myHyperedges) initials myH

fromProbabilisticRules
  :: [NTIdent] -- ^ initial NTs
  -> [(Rule, Double)] -- ^ rules and their probabilities
  -> MXRS
fromProbabilisticRules initials rs =
  MXRS (fromRules initials (map fst rs)) (V.fromList $ map snd rs)

toProbabilisticRules
  :: MXRS
  -> ([NTIdent], [(Rule, Double)])
toProbabilisticRules (MXRS (MIRTG hg inits h') ws)
  = (,) inits
  $ map worker
  $ zip3 (edges hg) -- assuming edges are sorted by ident...
         (V.toList ws) -- so that we can just zip this...
         (V.toList $ fmap (V.toList . fmap V.toList) h') -- ...and this.
  where
    worker (he, d, h'') = (((to he, from he), h''), d)

-- pretty printing

retranslateProbRule
  :: A.Array NTIdent String
  -> A.Array TIdent String
  -> (Rule, Double)
  -> String
retranslateProbRule a_nt a_t (((lhs, rhs), hom_f), p)
  =  (A.!) a_nt lhs
  ++ " -> ⟨"
  ++ intercalate (',' : thinspace)
                 (map (intercalate thinspace . map retHomComponent) hom_f)
  ++ "⟩( "
  ++ unwords (map ((A.!) a_nt) rhs)
  ++ " ) "
  ++ show p
    where
      retHomComponent (T t) = (A.!) a_t t
      retHomComponent (NT v) = show v
      thinspace = [' ']

showPLCFRS :: PLCFRS -> String
showPLCFRS (initials, rules, (a_nt, a_t))
  =  "Initial NTs:\n"
  ++ intercalate ", " (map ((A.!) a_nt) initials)
  ++ "\n\nRules (LHS -> ⟨comp.fct.⟩( RHS ) probability):\n"
  ++ unlines (map (retranslateProbRule a_nt a_t) rules)

niceStatictics
  :: PLCFRS
  -> String
niceStatictics (initials, rulesAndProbs, (a_nt, _)) =
  "\n"
  ++ printf "%7d initial non-terminals\n" (length initials)
  ++ printf "%7d non-terminals\n" (length (A.indices a_nt))
  ++ printf "%7d rules\n" (length rulesAndProbs)
  ++ "\n"
  ++ "Histograms:\n"
  ++ "  Ranks:\n"
  ++ unlines (map (uncurry (printf "    %2d: %7d")) rkCounts)
  ++ "  Fanouts:\n"
  ++ unlines (map (uncurry (printf "    %2d: %7d")) foCounts)
  where
    counter f = foldl' (\m r -> M.insertWith (+) (f r) 1 m)
                       (M.empty :: M.Map Int Int)
                       rulesAndProbs
    rkCounts = M.assocs $ counter getRk
    foCounts = M.assocs $ counter getFo

-- examples
exampleRules :: [Rule]
exampleRules
  = [ ((0, [1, 2]), [[NT 0, NT 2, NT 1, NT 3]]) -- S → [x₁y₁x₂y₂](A,B) # 1.0
    , ((1, [1]), [[T 0, NT 0], [T 2, NT 1]])    -- A → [ax₁, cx₂](A)   # 0.7
    , ((1, []), [[], []])                       -- A → [ε, ε]()        # 0.3
    , ((2, [2]), [[T 1, NT 0], [T 3, NT 1]])    -- B → [bx₁, dx₂](B)   # 0.4
    , ((2, []), [[], []])                       -- B → [ε, ε]()        # 0.6
    ]

exampleMIRTG :: MIRTG
exampleMIRTG
  = fromRules [1] exampleRules

exampleMXRS :: MXRS
exampleMXRS
  = MXRS exampleMIRTG (V.fromList [1, 0.7, 0.3, 0.4, 0.6])

examplePLCFRS :: PLCFRS
examplePLCFRS
  = ( [1]
    , zip exampleRules [1, 0.7, 0.3, 0.4, 0.6]
    , ( A.listArray (0, 2) ["S", "A", "B"]
      , A.listArray (0, 3) ["a", "b", "c", "d"]
      )
    )

-- | Returns the fanout of the given nonterminal symbol in the given 'PLCFRS'
getNonterminalFanout :: PLCFRS -> NTIdent -> Maybe Int
getNonterminalFanout (_, rs, _) i
  = fmap snd
    . listToMaybe
    . filter ((== i) . fst)
    $ map (\(((nt', _), f), _) -> (nt', length f)) rs
