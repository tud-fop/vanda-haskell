{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Grammar.XRS.LCFRS.IncrementalParser
  ( testParse,
    parse,
    exampleGrammar,
    Container
  ) where

import Data.Hashable (Hashable(hashWithSalt))
import Data.Converging (Converging)
import Data.Maybe (mapMaybe)
import Data.Range
import Data.Semiring
import Control.Monad(join)
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

data Item nt t wt = Active (Rule nt t) wt (IMap.IntMap Range) Int Range [VarT t] (Function t) (IMap.IntMap (IMap.IntMap Range)) wt
-- erste IMap sind fertige Ranges, Int ist Ri, Range ist jetzige Range, die schon fertig ist, [VarT t] ist das, was bei Ri gerade noch nicht fertig ist, zweite IMap ist quasi x_i,j , wobei äußere IMAp i darstellt, innere das j

instance (Eq nt, Eq t) => Eq (Item nt t wt) where
  (Active r _ rhos ri left right fs completions _) == (Active r' _ rhos' ri' left' right' fs' completions' _) 
    =  r           == r' 
    && rhos        == rhos' 
    && ri          == ri'
    && left        == left'
    && right       == right'
    && completions == completions'
    && fs          == fs'


instance (Hashable nt, Hashable t) => Hashable (Item nt t wt) where
  salt `hashWithSalt` (Active r _ _ _ left _ _ _ _) 
    = salt `hashWithSalt` r `hashWithSalt` left


instance (Show nt, Show t) => Show (Item nt t wt) where
  show (Active r _ _ ri left right _ _ _)
    = "[Active] " ++ show r ++ "\n" 
    ++ "current status: Ri:" ++ show(ri)++  ", " ++ show (left) ++ " • " ++ prettyPrintComposition show [right] -- TODO Ausführlicher

-- From active Parser
type Container nt t wt = ( C.Chart nt t wt -- Passive Items
                         , MMap.MultiMap (nt, Int) (Item nt t wt) --Map Variablen onto List of Active Items, that need NT in the next Step
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
    (singleton $ entire w) -- Goal Item TODO Falsches GOal item?
  $ (\ (e, _, _) -> (trace ("\nChart:" ++ (show e))  e)) -- parse Trees just needs passive Items from Container
  $ (\chart -> (trace ("\nchart" ++ ( show chart)) chart))
  $ C.chartify (C.empty, MMap.empty, nset) update rules bw tops
    where
      nset = Set.fromList $ filter (not . (`elem` s')) $ Map.keys rmap
      
      rules = [initialPrediction w (s' >>= (`MMap.lookup` rmap)) iow]

-- | Prediction rule for rules of initial nonterminals.
-- Predicted alles, bei dem Terminale am Anfang stehen
initialPrediction :: forall nt t wt. (Hashable nt, Eq nt, Semiring wt, Eq t, Show nt, Show t, Show wt) 
                  => [t]
                  -> [(Rule nt t, wt)]
                  -> Map.HashMap nt (wt, wt)
                  -> C.ChartRule (Item nt t wt) wt (Container nt t wt)
initialPrediction word srules ios 
  = Left 
      (trace' [ (Active r w IMap.empty ri left right [] IMap.empty inside, inside)  --TODO Darf fs theoretisch nicht durch [] ersetzen, aber da es sowieso bald wegkommt, ist das egal
      | (r@(Rule ((_, as), fs)), w) <- (trace ("\nI'm here" ++ (show srules)) srules) -- TODO, was ist, wenn Rule nicht f:fs ist, sondern nur 1 ELement ist. Geht das überhaupt?
      , (left, right,ri) <- trace' (completeKnownTokensWithRI word fs) "completed" -- Jede Funktion einmal komplete known Tokens übegeben -> 1 Item für jedes Ri
      , let inside = w <.> foldl (<.>) one (map (fst . (ios Map.!)) as)
      ] "initPred" )

trace' :: (Show s) => s -> String -> s
trace' s prefix= trace ("\n" ++ prefix ++": "++ (show s) ++ "\n") s

-- 
completeKnownTokensWithRI  :: (Eq t)
                    => [t] 
                    -> Function t -- alle Funktionen
                    -> [(Range, [VarT t], Int)] -- Zusätzliches Int, da ich Ri mit übergebe, Erste Liste für Ranges, innere für singletons mehrere Ausgaben
completeKnownTokensWithRI _ [] = []
completeKnownTokensWithRI word (f:fs) = case (completeKnownTokens word IMap.empty Epsilon f) of
  ([], _) -> completeKnownTokensWithRI word fs
  (lefts, right) -> [(left, right, ri)| left <- lefts] ++ (completeKnownTokensWithRI word fs) -- TODO ++ weg
  where ri = length (f:fs) -- Deshalb erstes Ri = R2, nicht R0
 -- Betrachte immer nur die erste Range, 
completeKnownTokens :: (Eq t)
                    => [t] 
                    -> IMap.IntMap (IMap.IntMap Range) -- Variablen, deren Ranges ich kenne
                    -- -> IMap.IntMap Range -- Fertige Ranges, brauche ich nicht
                    -> Range -- aktuelle Left,
                    -> [VarT t]-- aktuelle Right
--                    -> Function t -- Weitere Fkten., aktuelle right steht ganz vorne dran Brauch ich nicht
                    -> ([Range], [VarT t]) -- Danach bekoannte, mit allen Funktionen außer Epsilon.  -- Maybe, da, falls saveConc failed, ich das gar nicht mehr nutze, da irgendetwas falsch predicted
-- Original aus akt. Parser geht auch über mehrere Funktionen, aber das wird wahrscheinlich zu umständlich, da ich da wieder Unteritems erzeugen würde, je nachdem, welches Ri ich als nächstes besuchen würde
completeKnownTokens _ _ left [] = ([left], [])
completeKnownTokens w m left (T t:rights)
 -- = mapMaybe (\ left' -> completeKnownTokens w m left' rights) $ mapMaybe (safeConc left) $ singletons t w -- TODO Weiter Hier kommen schon mehere raus
  = (\ts -> (join $ map fst ts, snd $ head ts)) $ map (\left'' ->  completeKnownTokens w m left'' rights) 
    [ left'
    | left' <- mapMaybe (safeConc left) $ singletons t w
    ]
    -- TODO fs am Endealle gleich? oder mach ich da etwas falsch?
    
completeKnownTokens w m left (Var i j:rights)
  = case i `IMap.lookup` m of
         Just xi -> case j `IMap.lookup` xi of
            Just r -> case safeConc left r of -- Range ist vorhanden
                         Just left' -> completeKnownTokens w m left' rights
                         Nothing -> ([], rights)
            Nothing -> ([left], (Var i j):rights)
         Nothing -> ([left], (Var i j):rights)
--completeKnownTokens _ _ _ fs = ([], fs) -- Kann nichts machen

-- TODO  nitVerstehen
{-completeKnownTokens :: (Eq t)
                    => [t] 
                    -> IMap.IntMap Rangevector 
                    -> [(Int, Range)]
                    -> Function t 
                    -> [([(Int, Range)], Function t)]
completeKnownTokens _ _ rs [[]] = [(rs, [])]
completeKnownTokens w m rs ([]:fs) = completeKnownTokens w m (Epsilon:rs) fs
completeKnownTokens w m ((ii:r),rs) ((T t:fs):fss)  -- ii = Ri
  = [ (((ii,r):rs), fs:fss) 
    | r' <- mapMaybe (safeConc r) $ singletons t w
    ] >>= uncurry (completeKnownTokens w m)
completeKnownTokens w m ((ii,r):rs) ((Var i j:fs):fss) 
  = case i `IMap.lookup` m of
         Just rv -> case safeConc r (rv ! j) of
                         Just r' -> completeKnownTokens w m ((ii,r'):rs) (fs:fss)
                         Nothing -> []
         Nothing -> [(((ii,r):rs), (Var i j:fs):fss)]
completeKnownTokens _ _ _ _ = [] -}


-- True beudetet "Ist schon da gewesen"
update :: (Show nt, Show t, Show wt, Eq nt, Hashable nt) => Container nt t wt -> Item nt t wt -> (Container nt t wt, Bool)
update (p, a, n) (Active rule@(Rule ((nt, _), _)) iw _ _ left [] [] _ inside) = -- Sind alle Ris berechnet? -> Item fertig, also in p Chart aufnehmen
                case C.insert p nt {-rv aus rhos berechnen-} (singleton left) (C.Backtrace rule iw []) inside of -- TODO Backtrace + Rangevector neu
                    (p', isnew) -> trace ("\np':" ++ show p') ((p', a, n), isnew) -- TODO Auch in aktives Board mit aufnehmen? Oder nicht mehr nötig?
update (p, a, n) item@(Active (Rule ((_, as), _)) _ _ _ _ (Var i j: _) _ _ _) = ((p, MMap.insert ((as !! i), j) item a, (as !! i) `Set.delete` n), True) -- Schmeiß aus neuen Items raus, packe in aktive Items
update (p, a, n) _ = ((p,a,n), True) -- Nicht neu
--    = case C.insert p nt (fromJust $ fromList [r]) (C.Backtrace rule wt ([fromJust $ fromList [r])) wt of --2x fromList r falsch, aber erstmal egal
--        (p', isnew) -> trace "\nworks1" ((p', a, n), isnew)

--update (p, a, n) item@(Active (Rule ((_, as),_)) _ _ ((Var i _:_):_) _ _)
 --   = trace ("\nworks2" ++ (show item)) ((p, MMap.insert (as !! i) item a, (as !! i) `Set.delete` n), True)
--update (p, a, n) _ = trace "\nworks3"((p, a, n), True)
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


