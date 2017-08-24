-- (c) 2016 Tobias Denkinger <Tobias.Denkinger@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

module Vanda.Grammar.PMCFG.Functions
  ( fromPLCFRS
  , extractFromNegra
  , extractFromNegraAndBinarize
  ) where

import           Control.Arrow                           (first)
import           Data.Array                              ((!), listArray, bounds, indices)
import           Data.Maybe                              (fromJust)
import           Data.NTT                                (NTT (..))
import           Vanda.Corpus.Negra                      (Negra)
import           Vanda.Grammar.PMCFG                     (WPMCFG, fromWeightedRules, Rule (..))
import qualified Vanda.Grammar.PMCFG                as M (VarT (..))
import           Vanda.Grammar.XRS.LCFRS                 (PLCFRS, getNonterminalFanout, NTIdent)
import           Vanda.Grammar.XRS.LCFRS.Binarize        (Binarizer, binarizeUsing)
import           Vanda.Grammar.XRS.LCFRS.Extraction      (extractPLCFRSFromNegra)

fromPLCFRS :: PLCFRS -> WPMCFG String Double String
fromPLCFRS g@(is, rs, (mnt, mt))
  = fromWeightedRules (map (mnt !) is)
  $ map (first convertRule) rs
    where
      convertRule ((nt, nts), vartss)
        = Rule ((mnt ! nt, map (mnt !) nts), map (map $ convertComposition (map (fanouts !) nts)) vartss)
      fanouts 
        = listArray (bounds mnt) [ fromJust (getNonterminalFanout g n) | n <- indices mnt ]
      convertComposition _ (T t)
        = M.T (mt ! t)
      convertComposition f (NT j)
        = uncurry M.Var $ calculateIndices f 0 j
      calculateIndices [] _ _
        = error "variable index too high"
      calculateIndices (ix:ixs) i j
        | j < ix    = (i, j)
        | otherwise = calculateIndices ixs (i+1) (j-ix)

extractFromNegra :: Negra -> WPMCFG String Double String
extractFromNegra = fromPLCFRS . extractPLCFRSFromNegra

extractFromNegraAndBinarize
  :: Binarizer
  -> Negra
  -> WPMCFG String Double String
extractFromNegraAndBinarize strategy = fromPLCFRS . binarizeUsing strategy . extractPLCFRSFromNegra
