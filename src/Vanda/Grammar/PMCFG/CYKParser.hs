----------------------------------------------------
-- | NaiveParser
--
-----------------------------------------------------

module Vanda.Grammar.PMCFG.CYKParser
    ( parse
    , weightedParse
    , instantiate
    -- * Ranges
    , Range
    , Rangevector
    , prettyPrintRangevector
    , isNonOverlapping
    -- * Ranges with variables
    , Function
    , InstantiatedFunction
    , prettyPrintInstantiatedFunction
    , concVarRange
    , toRange
    ) where

import Vanda.Grammar.PMCFG.DeductiveSolver (solve, DeductiveSolver(DeductiveSolver), DeductiveRule(DeductiveRule))
import Vanda.Grammar.PMCFG
import Data.Tree (Tree(Node))
import Data.Hashable (Hashable, hashWithSalt)
import Data.List (elemIndices, sort)
import Data.Maybe (mapMaybe)

-- | A range in a word. Nothing is an empty range.
type Range = Maybe (Int, Int)
type Rangevector = [Range]

-- | Item of naive parsing deduction. 
-- Tuple of nonterminal, spanning range vector, derivation tree.
type DeductiveItem nt t = (nt, Rangevector, Tree (Rule nt t))

-- | Adds weight to item.
type WeightedDeductiveItem nt t wt = (DeductiveItem nt t, wt)

-- | A composition function.
type Function t = [[VarT t]]

-- | An instantiated composition function. 
-- Terminals are substituted by thir corresponding ranges in the word.
type InstantiatedFunction = Function Range

instance (Ord t) => Ord (Tree t) where
  compare (Node x xs) (Node y ys)
    | x < y = LT
    | x > y = GT
    | otherwise = compare xs ys

instance (Hashable t) => Hashable (Tree t) where
  salt `hashWithSalt` (Node x xs) = salt `hashWithSalt` x `hashWithSalt` xs

-- | Top-level function to parse a word using a grammar.
parse :: (Ord t, Ord nt, Hashable t, Hashable nt)
      => PMCFG nt t                               -- ^ the grammar
      -> [t]                                      -- ^ the word
      -> [Tree (Rule nt t)]                       -- ^ list of derivation trees 
parse (PMCFG s rules) w = map (\ (_, _, t) -> t) 
                          $ filter (\ (a, rho, _) -> (a `elem` s) && (rho == [expectedRange])) 
                          $ solve ds
  where
    ds = DeductiveSolver (makeRules w rules) id
    expectedRange = if not $ null w then Just (0, length w) else Nothing

-- | Top-level function to parse a word using a weighted grammar.
weightedParse :: (Ord t, Ord nt, Hashable t, Hashable nt, Num wt, Ord wt, Hashable wt)
              => WPMCFG nt wt t             -- ^ weighted grammar
              -> [t]                        -- ^ word
              -> [Tree (Rule nt t)]   -- ^ parse trees and resulting weights
weightedParse (WPMCFG s rs) w = map (\ ((_, _, t), _) -> t) 
                                $ filter (\ ((a, rho, _), _) -> (a `elem` s) && (rho == [expectedRange])) 
                                $ solve ds
  where
    ds = DeductiveSolver (makeWeightedRules w rs) sort
    expectedRange = if not $ null w then Just (0, length w) else Nothing

-- | Constructs deduction rules using a weighted grammar.
-- Weights are stored in antecedent items and apllication functions of rules.
makeWeightedRules :: (Eq nt, Num wt, Eq t, Ord wt) 
                  => [t]                                      -- ^ word 
                  -> [(Rule nt t, wt)]                        -- ^ weighted grammar rules
                  -> [DeductiveRule (DeductiveItem nt t, wt)] -- ^ weighted deduction rules 
makeWeightedRules w rs = rs >>= makeRule w
  where
    makeRule :: (Eq nt, Num wt, Eq t, Ord wt) => [t] -> (Rule nt t, wt) -> [DeductiveRule (DeductiveItem nt t, wt)]
    makeRule w r@(Rule ((a, as), f), weight) =  [ DeductiveRule (map itemFilter as) (application r inst) 
                                                | inst <- instantiate w f 
                                                ]
    
    itemFilter :: (Eq nt, Num wt, Ord wt) => nt -> (DeductiveItem nt t, wt) -> Bool
    itemFilter a ((a', _, _), weight) = a == a' && weight > 0
    
    application :: (Num wt, Ord wt) => (Rule nt t, wt) -> InstantiatedFunction -> [(DeductiveItem nt t, wt)] -> Maybe (DeductiveItem nt t, wt)
    application (r@(Rule ((a, as), f)), weight) fs is = case  insert rvs fs of
                                                              Just rv ->  if isNonOverlapping rv && newWeight > 0
                                                                          then Just ((a, rv, Node r ts), newWeight)
                                                                          else Nothing
                                                              Nothing -> Nothing
      where
        (antecedents, weights) = unzip is
        (rvs, ts) = case  unzip3 antecedents of
                          (_, rvs, ts) -> (rvs, ts)
        newWeight = weight * product weights

-- | Constructs deduction rules for a grammar to parse a word.
makeRules :: (Eq t, Eq nt) 
          => [t]                                  -- ^ the word, functions are instantiated to it
          -> [Rule nt t]                          -- ^ grammar rules
          -> [DeductiveRule (DeductiveItem nt t)] -- ^ resulting deduction rules with corresponding item type
makeRules w rs = rs >>= makeRule w
  where
    makeRule :: (Eq t, Eq nt) => [t] -> Rule nt t -> [DeductiveRule (DeductiveItem nt t)]
    makeRule w r@(Rule ((_, as), f)) =  [ DeductiveRule (map itemFilter as) (application r inst) 
                                        | inst <- instantiate w f 
                                        ]
    
    itemFilter :: (Eq nt) =>  nt -> DeductiveItem nt t -> Bool
    itemFilter a (a', _, _) = a == a'
    
    application :: Rule nt t -> InstantiatedFunction -> [DeductiveItem nt t] -> Maybe (DeductiveItem nt t)
    application r@(Rule ((a, _), _)) fs is = case insert rvs fs of
                                                  Just rv ->  if isNonOverlapping rv 
                                                              then Just (a, rv, Node r ts)
                                                              else Nothing
                                                  Nothing -> Nothing
      where
        (rvs, ts) = case  unzip3 is of
                          (_, rvs, ts) -> (rvs, ts)

-- | Returns a list of all possible instantiations of a composition function for a word.
instantiate :: (Eq t)
            => [t]                    -- ^ the word
            -> Function t             -- ^ the function to instantiate
            -> [InstantiatedFunction] -- ^ all possible combinations of instances with valid concatenated ranges
instantiate w = mapM (mapMaybe concVarRange . sequence . instantiateComponent w)
  where
    instantiateComponent :: (Eq t) => [t] -> [VarT t] -> [[VarT Range]]
    instantiateComponent _ []         = [[ T Nothing ]]
    instantiateComponent w fs         = map (instantiateCharacter w) fs
    instantiateCharacter :: (Eq t) => [t] -> VarT t -> [VarT Range]
    instantiateCharacter _ (Var i j)  = [Var i j]
    instantiateCharacter w (T c)      = map (T . Just . (\ i -> (i, i+1))) $ elemIndices c w

-- | Checks for overlapping components in a range vector.
isNonOverlapping :: Rangevector -> Bool
isNonOverlapping = isNonOverlapping' []
  where
    isNonOverlapping' :: [(Int, Int)] -> Rangevector -> Bool
    isNonOverlapping' _ [] = True
    isNonOverlapping' cache (Just (i,j) : rs)
      | any (\ (k,l) -> (j > k && j < l) || (i > k && i < l)) cache = False
      | otherwise = isNonOverlapping' ((i, j):cache) rs
    isNonOverlapping' cache (Nothing : rs) = isNonOverlapping' cache rs

-- | Replaces variables with corresponding ranges. Fails if ranges do not fit in their context.
insert :: [Rangevector] -> InstantiatedFunction -> Maybe Rangevector
insert rvs = mapM ((>>= toRange) . concVarRange . map (insert' rvs))
  where
    insert' :: [Rangevector] -> VarT Range -> VarT Range
    insert' rvs (Var x y)  = T $ rvs !! x !! y
    insert' _ r = r

-- | Tries to concatenate ranges in an instantiated function component.
-- Variables are left as they are, so they result does not need to be one range.
concVarRange :: [VarT Range] -> Maybe [VarT Range]
concVarRange (T (Just (i, j)) : T (Just (k, l)) : is)
  | j == k = concVarRange $ (T $ Just (i, l)) : is
  | otherwise = Nothing
concVarRange (T Nothing : (i : is)) = concVarRange $ i:is
concVarRange (i : (T Nothing : is)) = concVarRange $ i:is
concVarRange (i:is) = case concVarRange is of
                            Nothing -> Nothing
                            Just is' -> Just $ i:is'
concVarRange [] = Just []

-- | Tries to unpack a concatenated range vector.
toRange :: [VarT Range] -> Maybe Range
toRange [T r] = Just r
toRange _ = Nothing

prettyPrintInstantiatedFunction :: InstantiatedFunction -> String
prettyPrintInstantiatedFunction fs = show $ map go fs
  where
    go :: [VarT Range] -> String
    go [] = ""
    go (T (Just (i,j)) : f) = "(" ++ show i ++ "," ++ show j ++ ")" ++ go f
    go (T Nothing : f) = "()" ++ go f
    go (Var i j : f) = "x[" ++ show i ++ ":" ++ show j ++ "]" ++ go f

prettyPrintRangevector :: Rangevector -> String
prettyPrintRangevector rs = "<" ++ unwords (map go rs) ++  ">"
  where
    go (Just r) = show r
    go Nothing = "()"

prettyPrintDeductiveItem :: (Show nt, Show t) => DeductiveItem nt t -> String
prettyPrintDeductiveItem (n, rv, t) = show n ++ ", " ++ prettyPrintRangevector rv ++ ", " ++ show t