{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Grammar.XRS.LCFRS.IncrementalParser
  ( testParse,
--    parse,
    exampleGrammar,
    Container
  ) where

import Data.Hashable (Hashable(hashWithSalt))
import Data.Range
import Vanda.Grammar.PMCFG

import qualified Data.MultiHashMap             as MMap
import qualified Data.IntMap                   as IMap
import qualified Data.HashSet                  as Set
import qualified Vanda.Grammar.XRS.LCFRS.Chart as C

testParse :: String
testParse = "File Connected"


exampleGrammar :: String
exampleGrammar = prettyPrintWPMCFG prettyShowString prettyShowString exampleWPMCFG

-- From executable/PMCFG.hs
prettyShowString :: (Show w) => w -> String
prettyShowString s = '\"' : concatMap g (show s) ++ "\"" where
  g c    = [c]

-- From Active Parser, Only Difference in Kal10 between Item in act. and inc. Parser is Ri, which should not be a Problem when using a map
data Item nt t wt = Active (Rule nt t) wt [Range] (Function t) (IMap.IntMap Rangevector) wt  -- Might need to add passive Item for adding passive Items in Update Rule, so that I can use C.parseTrees
instance (Eq nt, Eq t) => Eq (Item nt t wt) where
  (Active r _ rs fs completions _) == (Active r' _ rs' fs' completions' _) 
    =  r           == r' 
    && rs          == rs' 
    && completions == completions'
    && fs          == fs'


instance (Hashable nt, Hashable t) => Hashable (Item nt t wt) where
  salt `hashWithSalt` (Active r _ rhos _ _ _) 
    = salt `hashWithSalt` r `hashWithSalt` rhos


instance (Show nt, Show t) => Show (Item nt t wt) where
  show (Active r _ rv f _ _)
    = "[Active] " ++ show r ++ "\n" 
    ++ "current status: " ++ show (reverse rv) ++ " • " ++ prettyPrintComposition show f

-- From active Parser
type Container nt t wt = ( C.Chart nt t wt -- Passive Items
                         , MMap.MultiMap nt (Item nt t wt) --Map NT onto List of Active Items, that need NT in the next Step
                         , Set.HashSet nt -- All NTs, which are not init. right now
                         )


-- From active Parser
{-parse :: forall nt t wt.(Hashable nt, Hashable t, Eq t, Ord wt, Weight wt, Ord nt, Converging wt) 
      => WPMCFG nt wt t -- Grammar
      -> Int -- Beam Width
      -> Int -- Max. Amount of Parse Trees
      -> [t] -- Word
      -> MMap.MultiMap nt (Item nt t wt)-} -- Map of Active Items for Testing Rules, -> [Tree (Rule nt t)] Later
-- parse g bw tops w = parse' (prepare g w) bw tops w


