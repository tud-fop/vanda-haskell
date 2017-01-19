module Vanda.Grammar.PMCFG.NaiveParser
    (parse) where

import Vanda.Grammar.PMCFG.DeductiveSolver (solve, DeductiveRule(DeductiveRule), DeductiveSolver(DeductiveSolver))
import Vanda.Grammar.PMCFG.CYKParser (InstantiatedFunction, Range, Rangevector, Function, concVarRange, toRange, isNonOverlapping, instantiate)
import Vanda.Grammar.PMCFG (Rule(Rule), PMCFG(PMCFG), WPMCFG(WPMCFG), VarT(Var, T), prettyPrintRule)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Tree (Tree(Node))

data Item nt t = ActiveItem (Rule nt t, [nt], Int, [Tree (Rule nt t)], InstantiatedFunction)
                    | PassiveItem (nt, Rangevector, Tree (Rule nt t)) deriving (Eq, Ord)

instance (Hashable nt, Hashable t) => Hashable (Item nt t) where
    salt `hashWithSalt` (ActiveItem tup) = salt `hashWithSalt` tup
    salt `hashWithSalt` (PassiveItem tup) = salt `hashWithSalt` tup

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

instance (Show nt, Show t) => Show (Item nt t) where
    show (ActiveItem (r, as, i, _, fs)) = "[active] " ++ prettyPrintRule r ++ " " ++ show as ++ "+" ++ show i ++ " " ++ prettyPrintInstantiatedFunction fs
    show (PassiveItem (a, rv, _)) = "[passive] " ++ show a ++ " " ++ prettyPrintRangevector rv 

parse :: (Eq t, Eq nt, Ord t, Ord nt, Hashable t, Hashable nt) => PMCFG nt t -> [t] -> [Tree (Rule nt t)]
parse (PMCFG s rs) w = map (\ (PassiveItem (_, _, t)) -> t) 
                        $ filter resultfilter
                        $ solve ds
    where
        ds = DeductiveSolver (rs >>= makeRule w) id
        targetrange = [ if null w then Nothing else Just (0, length w) ]
        --resultfilter :: (Eq nt) => Item nt t -> Bool
        resultfilter (PassiveItem (a, rho, _)) = a `elem` s && rho == targetrange
        resultfilter _ = False 

makeRule :: (Eq nt, Eq t) => [t] -> Rule nt t -> [DeductiveRule (Item nt t)]
makeRule w r@(Rule ((a, as), f)) = DeductiveRule [filterConversion r] convert
                                    : [ DeductiveRule [] (\ [] -> Just $ ActiveItem (r, as, 0, [], inst)) | inst <- instantiate w f ]
                                    ++ [ DeductiveRule [filterCompletePassive a', filterCompleteActive r] complete | a' <- as ]
    where
        filterCompletePassive :: (Eq nt) => nt -> Item nt t -> Bool
        filterCompletePassive a (PassiveItem (a', _, _)) = a == a'
        filterCompletePassive _ _ = False

        filterCompleteActive :: (Eq nt, Eq t) => Rule nt t -> Item nt t -> Bool
        filterCompleteActive r (ActiveItem (r', _, _, _, _)) = r == r'
        filterCompleteActive _ _ = False

        complete :: [Item nt t] -> Maybe (Item nt t)
        complete [ PassiveItem (a', rv, t)
                    , ActiveItem ( r@(Rule ((a, as), f))
                                , _:as', offset, ts, fs) ] = case mapM concVarRange $ insert offset rv fs of
                                                                Just fs' -> Just (ActiveItem (r, as', offset+1, t:ts, fs'))
                                                                Nothing -> Nothing
        complete _ = Nothing

        filterConversion :: (Eq t, Eq nt) => Rule nt t -> Item nt t -> Bool
        filterConversion r (ActiveItem (r', [], _, _, _)) = r == r'
        filterConversion _ _ = False

        convert :: [Item nt t] -> Maybe (Item nt t)
        convert [ActiveItem (r@(Rule ((a, _),_))
                , [], _, ts, fs)]               = case mapM ((>>= toRange) . concVarRange) fs of
                                                        Just rv ->  if isNonOverlapping rv
                                                                    then Just (PassiveItem (a, rv, Node r $ reverse ts))
                                                                    else Nothing
                                                        Nothing -> Nothing

insert :: Int -> Rangevector -> InstantiatedFunction -> InstantiatedFunction
insert off rv = map (map (substitute off rv))
    where
        substitute :: Int -> Rangevector -> VarT Range -> VarT Range
        substitute i rv (Var i' j)
            | i' == i =  T $ rv !! j
            | otherwise = Var i' j
        substitute _ _ r = r