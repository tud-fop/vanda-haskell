module Vanda.Grammar.PMCFG.Chart where

import Vanda.Grammar.PMCFG (Rule(Rule))
import Vanda.Grammar.PMCFG.Range (Rangevector)
import Data.Semiring
import Data.Hashable (Hashable)
import Data.Tree (Tree(Node))
import Data.Maybe (maybeToList)

import qualified Data.HashMap.Lazy as Map

data Backtrace nt t wt = Backtrace (Rule nt t) wt [Rangevector]

instance (Eq nt, Eq t) => Eq (Backtrace nt t wt) where
  (Backtrace r _ rv) == (Backtrace r' _ rv') = r == r' && rv == rv'

type Chart nt t wt = Map.HashMap nt (Map.HashMap Rangevector ([Backtrace nt t wt], wt))

empty :: Chart nt t wt
empty = Map.empty

lookupWith :: (Eq nt, Hashable nt)
           => (nt -> Rangevector -> Backtrace nt t wt -> wt -> it nt t wt) -> Chart nt t wt -> nt -> [it nt t wt]
lookupWith con passives nta = [ con nta rv bt w
                              | rvmap <- maybeToList $ Map.lookup nta passives
                              , (rv, (bt:_, w)) <- Map.toList rvmap
                              ]

insert :: (Semiring wt, Hashable nt, Eq nt) 
       => Chart nt t wt -> nt -> Rangevector -> Backtrace nt t wt -> wt -> (Chart nt t wt, Bool)
insert passives a rho bt iw = case  Map.lookup a passives of
                                    Nothing -> (Map.insert a (Map.singleton rho ([bt], iw)) passives, True)
                                    Just pa -> case Map.lookup rho pa of
                                                    Nothing -> (Map.adjust (Map.insert rho ([bt], iw)) a passives, True)
                                                    Just (bts, w) -> (Map.adjust (Map.insert rho (bt:bts, w <+> iw)) a passives, False)


readoff :: (Eq nt, Hashable nt) => [nt] -> Rangevector -> Chart nt t wt -> [Tree (Rule nt t)]
readoff ss rv passive = [ Node r children
                        | a <- ss
                        , (Backtrace r@(Rule ((_, as), _)) _ rvs) <- fst $ (passive Map.! a) Map.! rv
                        , let childbt = zip ((:[]) <$> as) rvs
                        , children <- sequence $ uncurry readoff <$> childbt <*> [passive]
                        ]

{-
data ChartRule it ct = ChartRule Bool (it -> ct -> [it])

buildChart :: ct -> (ct -> it -> (ct, Bool)) -> (ct -> Chart nt t wt) -> [ChartRule it ct] -> Int -> Chart nt t wt
buildChart container update fromContainer rules beam = fromContainer 
                                                        $ snd 
                                                        $ execState (chartIteration rs' update) 
                                                                    (Q.fromList b inits, container)
  where
    inits = rs >>= applyWithoutAntecedents
    
    applyWithoutAntecedents (DeductiveRule 0 _ app) = app container []
    applyWithoutAntecedents _ = []

    rs' = filter (\ (DeductiveRule antecedents _ _) -> antecedents > 0) rs
-}