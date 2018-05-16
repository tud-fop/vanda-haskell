{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Grammar.XRS.LCFRS.IncrementalParser
  ( testParse,
    parse,
    exampleGrammar,
    Container
  ) where

import Data.Hashable (Hashable(hashWithSalt))
import Data.Converging (Converging)
import Data.Maybe (mapMaybe, fromJust)
import Data.Range
import Data.Semiring
import Data.Tree (Tree)
import Data.Weight
import Vanda.Grammar.PMCFG
import Debug.Trace(trace)

import qualified Data.HashMap.Lazy             as Map

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
{- update :: Container nt t wt -> Item nt t wt -> (Container nt t wt, Bool)
update (p, a, n) item@(Active (Rule ((_, as),_)) _ _ ((Var i _:_):_) _ _)
 = ((p, MMap.insert (as !! i) item a, (as !! i) `Set.delete` n), True)
update (p, a, n) _ = ((p, a, n), True) TODO Hier rein, wann ein actives Item in Chart kommt-} 


-- From active Parser
parse :: forall nt t wt.(Show nt, Show t, Show wt, Hashable nt, Hashable t, Eq t, Ord wt, Weight wt, Ord nt, Converging wt) 
      => WPMCFG nt wt t -- Grammar
      -> Int -- Beam Width
      -> Int -- Max. Amount of Parse Trees
      -> [t] -- Word
      -> [Tree (Rule nt t)]
parse g bw tops w = parse' (prepare g w) bw tops w

parse' :: forall nt t wt.(Show nt, Show t, Show wt, Hashable nt, Hashable t, Eq t, Ord wt, Weight wt, Ord nt, Converging wt) 
       => (MMap.MultiMap nt (Rule nt t, wt), Map.HashMap nt (wt,wt), [nt]) -- prepare Result (RuleMap NT-Rules, IO-Weights NTs, Reachable Items
       -> Int -- Beam Width
       -> Int -- Max. Amount of Parse Trees
       -> [t] -- Word
       -> [Tree (Rule nt t)]
parse' (rmap, iow, s') bw tops w
  = C.parseTrees tops (trace ("\ns':" ++ show s') s')
    (singleton $ entire w) -- Goal Item
  $ (\ (e, _, _) -> e) -- parse Trees just needs passive Items from Container
  $ (\chart -> (trace ("\nchart" ++ ( show chart)) chart))
  $ C.chartify (C.empty, MMap.empty, nset) update rules bw tops
    where
      nset = Set.fromList $ filter (not . (`elem` s')) $ Map.keys rmap
      
      rules = [initialPrediction w (s' >>= (`MMap.lookup` rmap)) iow]

-- | Prediction rule for rules of initial nonterminals.
initialPrediction :: forall nt t wt. (Hashable nt, Eq nt, Semiring wt, Eq t) 
                  => [t]
                  -> [(Rule nt t, wt)]
                  -> Map.HashMap nt (wt, wt)
                  -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
initialPrediction word srules ios 
  = Left 
      [ (Active r w rho' f' IMap.empty inside, inside) 
      | (r@(Rule ((_, as), f)), w) <- (trace "\nI'm here" srules)
      , (rho', f') <- completeKnownTokens word IMap.empty [Epsilon] f
      , let inside = w <.> foldl (<.>) one (map (fst . (ios Map.!)) as)
      ]

completeKnownTokens :: (Eq t)
                    => [t] 
                    -> IMap.IntMap Rangevector 
                    -> [Range] 
                    -> Function t 
                    -> [([Range], Function t)]
completeKnownTokens _ _ rs [[]] = [(rs, [])]
completeKnownTokens w m rs ([]:fs) = completeKnownTokens w m (Epsilon:rs) fs
completeKnownTokens w m (r:rs) ((T t:fs):fss) 
  = [ (r':rs, fs:fss)
    | r' <- mapMaybe (safeConc r) $ singletons t w
    ] >>= uncurry (completeKnownTokens w m)
completeKnownTokens w m (r:rs) ((Var i j:fs):fss) 
  = case i `IMap.lookup` m of
         Just rv -> case safeConc r (rv ! j) of
                         Just r' -> completeKnownTokens w m (r':rs) (fs:fss)
                         Nothing -> []
         Nothing -> [(r:rs, (Var i j:fs):fss)]
completeKnownTokens _ _ _ _ = []


update :: (Show nt, Show t, Show wt, Eq nt, Hashable nt) => Container nt t wt -> Item nt t wt -> (Container nt t wt, Bool)
update (p, a, n) (Active rule@(Rule ((nt, _),_)) wt ranges ([]:_) _ _)
    = case C.insert p nt (fromJust $ fromList ranges) (C.Backtrace rule wt ([fromJust $ fromList ranges])) wt of
        (p', isnew) -> trace "\nworks1" ((p', a, n), isnew)
update (p, a, n) item@(Active (Rule ((_, as),_)) _ _ ((Var i _:_):_) _ _)
    = trace ("\nworks2" ++ (show item)) ((p, MMap.insert (as !! i) item a, (as !! i) `Set.delete` n), True)
update (p, a, n) _ = trace "\nworks3"((p, a, n), True)
-- TODO Schau, dass init Pred das macht, was es soll


{-convert :: (Item nt t wt, wt) -> Maybe (Item nt t wt, wt)
convert (Active r w rs [] completions inside, heuristic)
  = case fromList $ reverse rs of
         Nothing -> Nothing
         Just rv -> let rvs = IMap.elems completions
                        (Rule ((a, _), _)) = r
                    in Just (Passive a rv (C.Backtrace r w rvs) inside, heuristic)
convert i@(Active _ _ rs _ _ _, _)
  | isNonOverlapping rs = Just i
  | otherwise = Nothing
convert _ = Nothing

-- Wenn Item direkt passive ist, dann wird letztes Schritt über aktive Item nicht mehr in Map aufgenommen, vor passive Item weggelassen, da ich das nicht brauche. Falls nicht Konvertiert werden kann, dann einfach dieses Item zurückgeben
predictionRule :: forall nt t wt. (Weight wt, Eq nt, Hashable nt, Eq t) 
               => [t] -- Word
               -> MMap.MultiMap nt (Rule nt t, wt) -- Rules from prepare: NT->Rules Map
               -> Map.HashMap nt (wt, wt) -- IO-Weigths from prepare
               -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
predicitionRule word rs ios = Right app
    where
        app :: Item nt t wt -- Trigger Item
            -> Container nt t wt
            -> [(Item nt t wt, wt)] -- New Items from using Trigger with Container by using prediction Rule
        app (Active (Rule ((_, as), _)) w _ ((Var i _:_):_) _ _) (_, _, inits) --Var i -> Betrachte i-te Eingangsvektor Xi
        = catMaybes [ (Active r' w rho'' f'' IMap.empty inside, inside <.> outside) 
        | let a = as !! i -- Nimm den i-ten Einfangsvektor Xi
        , (r'@(Rule ((a', as'), f')), w') <- MMap.lookup a rs -- Baue für jede Regel, die  Xi -> ... ist, neues Item auf
        , (rho'', f'') <- com
        ] 

-- Nimmt gefundene Ranges für ein NT + Aktuelle Ranges + Regel -> Spuckt neue Ranges und Funktionen aus, in denen alles, was möglich ist, eingesetzt wurde
completeKnownTokens :: (Eq t)
                    => [t]  -- Word
                    -> IMap.IntMap Rangevector  -- Map von Ranges von NTs, welche ich schon ersetzt habe A1->...
                    -> [Range] -- Ranges, welche ich hinzufügen will?
                    -> Function t -- Ersetzungfunktion
                    -> [([Range], Function t)] -- Neue Range und Funktion
completeKnownTokens _ _ rs [[]] = [(rs, [])]
completeKnownTokens w m rs ([]:fs) = completeKnownTokens w m (Epsilon:rs) fs
completeKnownTokens w m (r:rs) ((T t:fs):fss) 
  = [ (r':rs, fs:fss)
    | r' <- mapMaybe (safeConc r) $ singletons t w
    ] >>= uncurry (completeKnownTokens w m)
completeKnownTokens w m (r:rs) ((Var i j:fs):fss) 
  = case i `IMap.lookup` m of
         Just rv -> case safeConc r (rv ! j) of
                         Just r' -> completeKnownTokens w m (r':rs) (fs:fss)
                         Nothing -> []
         Nothing -> [(r:rs, (Var i j:fs):fss)]
completeKnownTokens _ _ _ _ = []

-}


-- TODO convert active Item noch überall rein, sobald ich weiß, dass mein aktive Item richtig ist und ich daraus passive Item ableiten kann
--


