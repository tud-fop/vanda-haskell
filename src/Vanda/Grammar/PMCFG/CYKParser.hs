----------------------------------------------------
-- | NaiveParser
--
-----------------------------------------------------
module Vanda.Grammar.PMCFG.CYKParser
    ( parse
    , weightedParse
    , instantiate
    -- * derivation trees
    , Derivation(Derivation)
    , node
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

import Vanda.Grammar.PMCFG.WeightedDeductiveSolver (solve, WeightedDeductiveSolver(..), DeductiveRule(..), Cost(..))
import Vanda.Grammar.PMCFG
import Data.Tree (Tree(Node))
import Data.Hashable (Hashable, hashWithSalt)
import Data.List (elemIndices, sort)
import Data.Maybe (mapMaybe)

-- | A range in a word. Nothing is an empty range.
type Range = Maybe (Int, Int)
type Rangevector = [Range]

-- | Item of naive parsing deduction. 
-- Tuple of non-terminal, spanning range vector, derivation tree.
type DeductiveItem nt t = (nt, Rangevector, Derivation nt t)

-- | A composition function.
type Function t = [[VarT t]]

-- | An instantiated composition function. 
-- Terminals are substituted by their corresponding ranges in the word.
type InstantiatedFunction = Function Range

-- | A derivation tree.
newtype Derivation nt t = Derivation (Tree (Rule nt t)) deriving (Eq, Show)

-- | Wraps Node constructor of Tree to easy use of Derivation.
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

-- | Top-level function to parse a word using a grammar.
parse :: (Ord t, Ord nt)
      => PMCFG nt t                               -- ^ the grammar
      -> [t]                                      -- ^ the word
      -> [Tree (Rule nt t)]                       -- ^ list of derivation trees 
parse (PMCFG s rules) = weightedParse $ WPMCFG s $ zip rules $ repeat (Cost 1 :: Cost Int)

-- | Top-level function to parse a word using a weighted grammar.
weightedParse :: (Ord t, Ord nt, Monoid wt, Ord wt)
              => WPMCFG nt wt t             -- ^ weighted grammar
              -> [t]                        -- ^ word
              -> [Tree (Rule nt t)]   -- ^ parse trees and resulting weights
weightedParse (WPMCFG s rs) w = map (\ (_, _, Derivation t) -> t) 
                                $ filter (\ (a, rho, _) -> (a `elem` s) && (rho == [expectedRange])) 
                                $ solve ds
  where
    ds = WeightedDeductiveSolver (makeWeightedRules w rs) sort
    expectedRange = if not $ null w then Just (0, length w) else Nothing

-- | Constructs deduction rules using a weighted grammar.
-- Weights are stored in antecedent items and application functions of rules.
makeWeightedRules :: (Eq nt, Monoid wt, Eq t, Ord wt) 
                  => [t]                                      -- ^ word 
                  -> [(Rule nt t, wt)]                        -- ^ weighted grammar rules
                  -> [(DeductiveRule (DeductiveItem nt t), wt)] -- ^ weighted deduction rules 
makeWeightedRules w rs =  [ (DeductiveRule (map itemFilter as) (application r $ instantiate w f), weight)
                          | (r@(Rule ((_, as), f)), weight) <- rs
                          ]
  where
    itemFilter :: (Eq nt) => nt -> DeductiveItem nt t -> Bool
    itemFilter a (a', _, _)= a == a'
    
    application :: (Rule nt t) -> [InstantiatedFunction] -> [DeductiveItem nt t] -> [DeductiveItem nt t]
    application r@(Rule ((a', _), _)) fs is = [ (a', rv, node r ts) 
                                              | rv <- mapMaybe (insert rvs) fs
                                              , isNonOverlapping rv 
                                              ]
      where
        (rvs, ts) = case  unzip3 is of
                          (_, rvs', ts') -> (rvs', ts')


-- | Returns a list of all possible instances of a composition function for a word.
instantiate :: (Eq t)
            => [t]                    -- ^ the word
            -> Function t             -- ^ the function to instantiate
            -> [InstantiatedFunction] -- ^ all possible combinations of instances with valid concatenated ranges
instantiate w' = mapM (mapMaybe concVarRange . sequence . instantiateComponent w')
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
    insert' rvs' (Var x y)  = T $ rvs' !! x !! y
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