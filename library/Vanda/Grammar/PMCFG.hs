{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Tobias Denkinger 2016
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Grammar.PMCFG
  ( VarT (Var, T)
  , Rule (Rule)
  , lhs
  , antecedents
  , isT
  , fromT
  , isVar
  , fromVar
  , printAsLcfrsRule
  -- * Parallel multiple context-free grammars
  , PMCFG (PMCFG)
  , fromRules
  , yield
  -- * Weighted parallel multiple context-free grammars
  , WPMCFG (WPMCFG)
  , fromWeightedRules
  , integerize
  , deintegerize
  , ioWeights
  , prepare
  -- * derivation trees
  , Derivation(Derivation)
  , node
  , pos
  , posTab
  -- * ranges with variables
  , Function
  , InstantiatedFunction
  , instantiate
  , concVarRange
  , toRange
  -- * pretty printing
  , prettyPrintRule
  , prettyPrintComposition
  , prettyPrintWPMCFG
  , prettyPrintInstantiatedFunction
  -- * examples
  , examplePMCFG
  , exampleWPMCFG
  , exampleDerivation
  , exampleRules
  , exampleCompositions
  , exampleWeights
  , instantiableRules
  ) where

import Control.Arrow (first)
import Control.DeepSeq (NFData)
import Control.Monad.Trans.State.Lazy (State, get, put, runState)
import Data.Array (Array, (!))
import Data.Converging (Converging(converged))
import Data.Hashable
import Data.Interner (Interner, internList, intern, emptyInterner, internListPreserveOrder, internerToArray)
import Data.List (intercalate)
import Data.Maybe (listToMaybe, mapMaybe, fromJust)
import Data.Range (Range(Epsilon), safeConc, singletons)
import Data.Semiring
import Data.Tree
import GHC.Generics (Generic)
import Vanda.Hypergraph (EdgeList(EdgeList), ident, mkHyperedge)
import Vanda.Algorithms.InsideOutsideWeights (insideOutside')

import qualified Control.Error
import qualified Data.Binary          as B
import qualified Data.HashMap.Strict  as Map
import qualified Data.Map             as M
import qualified Data.Vector          as V
import qualified Data.Set             as S
import qualified Data.MultiHashMap    as MMap

errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.Grammar.PMCFG"

data VarT t = T t | Var !Int !Int deriving (Eq, Ord, Show, Generic, NFData)

instance (Hashable t) => Hashable (VarT t) where
  salt `hashWithSalt` (T t) = salt `hashWithSalt` t
  salt `hashWithSalt` (Var i j) = salt `hashWithSalt` i `hashWithSalt` j

isT :: VarT t -> Bool
isT (T _) = True
isT _     = False

fromT :: VarT t -> Maybe t
fromT (T x) = Just x
fromT _     = Nothing

isVar :: VarT t -> Bool
isVar (Var _ _) = True
isVar _         = False

fromVar :: VarT t -> Maybe (Int, Int)
fromVar (Var i j) = Just (i,j)
fromVar _         = Nothing

instance Functor VarT where
  fmap f (T t)     = T (f t)
  fmap _ (Var i j) = Var i j

instance B.Binary t => B.Binary (VarT t) where
  get = do x <- B.getWord8
           case x of
             0 -> do t <- B.get
                     return $ T t
             1 -> do i <- B.get
                     j <- B.get
                     return $ Var i j
             _ -> errorHere "get" $ "unexpected word" ++ show x
  put (T t) = B.putWord8 0 >> B.put t
  put (Var i j) = B.putWord8 1 >> B.put i >> B.put j


-- | 'Rule' ((A, [A₁, …, Aₖ]), f) ~ A → f(A₁, …, Aₖ).
newtype Rule nt t = Rule ((nt, [nt]), [[VarT t]]) deriving (Eq, Ord, Show, Generic, NFData)

lhs :: (Rule nt t, wt) -> nt
lhs (Rule ((a, _), _), _) = a

antecedents :: (Rule nt t, wt) -> [nt]
antecedents (Rule ((_, as),_), _) = as

composition :: (Rule nt t, wt) -> Function t
composition (Rule (_, f), _) = f

instance (Hashable nt, Hashable t) => Hashable (Rule nt t) where
  salt `hashWithSalt` (Rule tup) = salt `hashWithSalt` tup

instance Functor (Rule nt) where
  fmap f (Rule (nts, varts)) = Rule (nts, map (map (fmap f)) varts)

instance (B.Binary nt, B.Binary t) => B.Binary (Rule nt t) where
  get = Rule <$> B.get
  put (Rule x) = B.put x

data PMCFG nt t = PMCFG [nt] [Rule nt t] deriving (Show, Generic, NFData)

instance Functor (PMCFG nt) where
  fmap f (PMCFG ints rs) = PMCFG ints $ map (fmap f) rs

instance (B.Binary nt, B.Binary t) => B.Binary (PMCFG nt t) where
  get = do is <- B.get
           rs <- B.get
           return $ PMCFG is rs
  put (PMCFG is rs) = B.put is >> B.put rs

fromRules
  :: [nt] -- ^ initial non-terminals
  -> [Rule nt t] -- ^ rules
  -> PMCFG nt t
fromRules = PMCFG


data WPMCFG nt w t = WPMCFG [nt] [(Rule nt t, w)] deriving (Show, Generic, NFData)

instance Functor (WPMCFG nt w) where
  fmap f (WPMCFG ints rs) = WPMCFG ints $ map (first (fmap f)) rs

instance (B.Binary nt, B.Binary w, B.Binary t) => B.Binary (WPMCFG nt w t) where
  get = do is <- B.get
           wrs <- B.get
           return $ WPMCFG is wrs
  put (WPMCFG is wrs) = B.put is >> B.put wrs

fromWeightedRules
  :: [nt] -- ^ initial non-terminals
  -> [(Rule nt t, w)] -- ^ weighted rules
  -> WPMCFG nt w t
fromWeightedRules = WPMCFG


-- | A composition function.
type Function t = [[VarT t]]


-- | An instantiated composition function. 
-- Terminals are substituted by their corresponding ranges in the word.
type InstantiatedFunction = Function Range


-- | A derivation tree.
newtype Derivation nt t = Derivation (Tree (Rule nt t)) deriving (Eq, Show)


-- | Wraps Node constructor of Tree for easy use of Derivation.
node :: Rule nt t -> [Derivation nt t] -> Derivation nt t
node r ds = Derivation $ Node r ts
  where
    ts = map (\ (Derivation t) -> t) ds


instance (Ord nt, Ord t) => Ord (Derivation nt t) where
  compare (Derivation (Node x xs)) (Derivation (Node y ys))
    | x < y = LT
    | x > y = GT
    | otherwise = map Derivation xs `compare` map Derivation ys


instance (Hashable t, Hashable nt) => Hashable (Derivation nt t) where
  salt `hashWithSalt` (Derivation (Node x xs)) = salt `hashWithSalt` x `hashWithSalt` map Derivation xs



-- | Tries to concatenate ranges in an instantiated function component.
-- Variables are left as they are, so they result does not need to be one range.
concVarRange :: [VarT Range] -> Maybe [VarT Range]
concVarRange (T r1 : T r2 : is) = case safeConc r1 r2 of
                                       Just r -> concVarRange $ T r : is
                                       Nothing -> Nothing
concVarRange (T Epsilon : (i : is)) = concVarRange $ i:is
concVarRange (i : (T Epsilon : is)) = concVarRange $ i:is
concVarRange (i:is) = (i:) <$> concVarRange is
concVarRange [] = Just []


-- | Tries to unpack a concatenated range vector.
toRange :: [VarT Range] -> Maybe Range
toRange [T r] = Just r
toRange _ = Nothing


-- | Returns a list of all possible instances of a composition function for a word.
instantiate :: (Eq t)
            => [t]                    -- ^ the word
            -> Function t             -- ^ the function to instantiate
            -> [InstantiatedFunction] -- ^ all possible combinations of instances with valid concatenated ranges
instantiate w' = mapM (mapMaybe concVarRange . sequence . instantiateComponent w')
  where
    instantiateComponent :: (Eq t) => [t] -> [VarT t] -> [[VarT Range]]
    instantiateComponent _ []         = [[ T Epsilon ]]
    instantiateComponent w fs         = map (instantiateCharacter w) fs
    
    instantiateCharacter :: (Eq t) => [t] -> VarT t -> [VarT Range]
    instantiateCharacter _ (Var i j)  = [Var i j]
    instantiateCharacter w (T c)      = map T $ singletons c w


prettyPrintInstantiatedFunction :: InstantiatedFunction -> String
prettyPrintInstantiatedFunction fs = show $ map go fs
  where
    go :: [VarT Range] -> String
    go [] = ""
    go (T r : f) = "(" ++ show r ++ ")" ++ go f
    go (Var i j : f) = "x[" ++ show i ++ ":" ++ show j ++ "]" ++ go f


-- | Substitutes all terminals and nonterminals with integers and saves substitutions.
integerize :: (Eq t, Eq nt, Hashable t, Hashable nt) => WPMCFG nt wt t -> (WPMCFG Int wt Int, Interner nt, Interner t)
integerize (WPMCFG s rs) = (WPMCFG s' rs', ntInterner', tInterner)
  where
    (rs', (ntInterner, tInterner)) = runState (mapM integerize' rs) (emptyInterner, emptyInterner)
    (ntInterner', s') = internList ntInterner s
    
    integerize' :: (Eq nt, Hashable nt, Eq t, Hashable t) => (Rule nt t, wt) -> State (Interner nt, Interner t) (Rule Int Int, wt)
    integerize' (Rule ((a, as), f), wt) = do (nti, ti) <- get
                                             let (nti', a') = intern nti a
                                             let (nti'', as') = internListPreserveOrder nti' as
                                             let (f', ti') = runState (mapM (mapM internVar) f) ti
                                             put (nti'', ti')
                                             return (Rule ((a', as'), f'), wt)
      where
        internVar :: (Eq t, Hashable t) => VarT t -> State (Interner t) (VarT Int)
        internVar (T t) = do i <- get
                             let (i', t') = intern i t
                             put i'
                             return (T t')
        internVar (Var i j) = return (Var i j)

-- | Re-substitutes old terminals and nonterminals into integetrized rules.
deintegerize :: (Interner nt, Interner t) -> Tree (Rule Int Int) -> Tree (Rule nt t)
deintegerize (nti, ti) = fmap (deintegerize' ta nta) 
  where
    nta = internerToArray nti
    ta = internerToArray ti
    
    deintegerize' :: Array Int t -> Array Int nt -> Rule Int Int -> Rule nt t
    deintegerize' ta' nta' (Rule ((a, as), f)) = Rule ((a', as'), f')
      where
        a' = nta' ! a
        as' = map (nta' !) as
        f' = map (map (fmap (ta' !))) f

yield :: Tree (Rule nt t) -> Maybe [t]
yield = fmap head . evaluate . fmap (\(Rule (_, f)) -> f)

evaluate :: Tree [[VarT t]] -> Maybe [[t]]
evaluate (Node f ts) = do
  tups <- mapM evaluate ts
  let lookUp (Var i j) = listToMaybe (drop i tups) >>= (listToMaybe . drop j)
      lookUp (T t)     = Just [t]
  mapM (fmap concat . mapM lookUp) f


prettyPrintRule :: (nt -> String) -> (t -> String) -> Rule nt t -> String
prettyPrintRule ntPrint tPrint (Rule ((a, bs), f))
  = ntPrint a ++ " → " ++ prettyPrintComposition tPrint f ++ " (" ++ intercalate ", " (map ntPrint bs) ++ ")"

prettyPrintWeightedRule :: Show w => (nt -> String) -> (t -> String) -> (Rule nt t, w) -> String
prettyPrintWeightedRule ntPrint tPrint (r, w) = prettyPrintRule ntPrint tPrint r ++ "\t# " ++ show w

prettyPrintList :: (t -> String) -> [t] -> String
prettyPrintList f l = '[' : intercalate ", " (map f l) ++ "]"

prettyPrintComposition :: (t -> String) -> [[VarT t]] -> String
prettyPrintComposition tPrint = prettyPrintList (prettyPrintList g)
  where g (Var i j) = "Var " ++ show i ++ " " ++ show j
        g (T t)     = "T " ++ tPrint t

prettyPrintWPMCFG :: Show w => (nt -> String) -> (t -> String) -> WPMCFG nt w t -> String
prettyPrintWPMCFG ntPrint tPrint (WPMCFG is rs)
  = "initial: " ++ prettyPrintList ntPrint is ++ "\n\n"
    ++ unlines (map (prettyPrintWeightedRule ntPrint tPrint) rs)

exampleCompositions :: [[[VarT Char]]]
exampleCompositions = [ [[Var 0 0, Var 1 0, Var 0 1, Var 1 1]]
                      , [[T 'a', Var 0 0], [T 'c', Var 0 1]]
                      , [[T 'b', Var 0 0], [T 'd', Var 0 1]]
                      , [[], []]
                      ]

exampleRules :: [Rule Int Char]
exampleRules = [ Rule ((0, [1,2]), exampleCompositions !! 0)
               , Rule ((1, [1])  , exampleCompositions !! 1)
               , Rule ((1, [])   , exampleCompositions !! 3)
               , Rule ((2, [2])  , exampleCompositions !! 2)
               , Rule ((2, [])   , exampleCompositions !! 3)
               ]

exampleDerivation :: Tree (Rule Int Char)
exampleDerivation = Node (exampleRules !! 0)
                    [ Node (exampleRules !! 1)
                      [ Node (exampleRules !! 1)
                        [ Node (exampleRules !! 2) [] ]
                      ]
                    , Node (exampleRules !! 3)
                      [ Node (exampleRules !! 4) [] ]
                    ]

exampleWeights :: [Double]
exampleWeights = [1, 0.6, 0.4, 0.3, 0.7]

examplePMCFG :: PMCFG Int Char
examplePMCFG = fromRules [0] exampleRules

exampleWPMCFG :: WPMCFG Int Double Char
exampleWPMCFG = fromWeightedRules [0] $ zip exampleRules exampleWeights


prepare :: (Converging wt, Semiring wt, Hashable nt, Ord nt, Eq t, Ord wt)
        => WPMCFG nt wt t -> [t] -> (MMap.MultiMap nt (Rule nt t, wt), Map.HashMap nt (wt,wt), [nt])
prepare (WPMCFG s rs) w = let frs = filter (not . null . instantiate w . composition) rs
                              iow = ioWeights s frs
                              heuristic (Rule ((a,as),_),weight) 
                                = snd (Map.lookupDefault (zero, zero) a iow) 
                               <.> weight
                               <.> foldl (<.>) one ((fst . flip (Map.lookupDefault (zero, zero)) iow) <$> as)
                              rmap = MMap.fromList 
                                   $ (\ r -> (lhs r, r))
                                  <$> filter ((< zero) . heuristic) frs
                              s' = filter (\ nt -> (< zero) $ fst $ Map.lookupDefault (zero, zero) nt iow) s
                          in (((,,) $! rmap) $! iow) $! s'

-- | Calculates inside and outside weights for a given grammar with semiring weights.
ioWeights :: (Converging wt, Semiring wt, Hashable nt, Ord nt)
          => [nt]
          -> [(Rule nt t, wt)]
          -> Map.HashMap nt (wt, wt)
ioWeights ss rs
  = toHashMap
  $ insideOutside' converged
                   ((V.!) wV . ident)
                   Nothing
                   (EdgeList (S.fromList (map (Just . lhs) rs)) es)
  where
    (es, wV)
      = first (V.toList . V.imap (flip ($)))
      $ V.unzip
      $ V.fromList
        (
          [ (mkHyperedge Nothing [Just s] (Nothing, [s]), one)
          | s <- ss
          ]
        ++
          [ (mkHyperedge (Just a) (map Just as) (Just a, as), weight)
          | (Rule ((a, as), _), weight) <- rs
          ]
        )

    toHashMap m = Map.fromList [(a, io) | (Just a, io) <- M.toList m]


instantiableRules :: (Eq t, Eq nt, Hashable nt)
                  => [t] -> [(Rule nt t, wt)] -> MMap.MultiMap nt (Rule nt t, wt)
instantiableRules w rs = let nrs = (\ r -> (lhs r, r))
                                <$> filter (not . null . instantiate w . composition) rs
                         in MMap.fromList nrs


pos :: Tree (Rule nt t) -> Maybe [(t, nt)]
pos = yield . fmap topostag
  where
    -- annotate a pos rule's terminal with its pos tag;
    -- if inner rule expect only variables, so terminals in those compositions
    -- are undefined
    topostag :: Rule nt t -> Rule nt (t, nt)
    topostag (Rule ((a, []), [[T token]])) = Rule ((a, []), [[T (token, a)]])
    topostag (Rule ((a, as), fss)) = Rule ((a, as), ((const undefined <$>) <$>) <$> fss)
    
posTab :: [[(t, nt)]] -> [(t, [nt])]
posTab (pos:poss) = let tokens = fst <$> pos
                        nts = (snd <$>) <$> (pos:poss)
                        nts' = foldr (zipWith (:)) (repeat []) nts
                    in zip tokens nts'
posTab [] = []

printAsLcfrsRule :: Rule String String -> String
printAsLcfrsRule (Rule ((a, bs), composition))
  = a ++ " -> ⟨" 
      ++ intercalate (',' : thinspace)
                     (map (intercalate thinspace . map ppCompEle) composition)
      ++ "⟩(" ++ unwords bs ++ ")"
  where
    ppCompEle v@(Var _ _) = show $ fromJust $ S.lookupIndex v orderedVariables
    ppCompEle (T s) = s
    orderedVariables = S.fromList $ filter isVar $ concat composition
    thinspace = [' ']