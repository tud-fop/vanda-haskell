{-|
Module:      Vanda.Grammar.AutomataStorage.PushdownStorage
Description: storage type for /pushdowns/
Copyright:   Ⓒ Tobias Denkinger, 2015
Maintainer:  Tobias.Denkinger@tu-dresden.de
Stability:   experimental

This module contains a data structure for the storage type /pushdown/ together with some predicates and functions on pushdowns.
-}
module Vanda.Grammar.AutomataStorage.PushdownStorage
  ( Pushdown
-- * construction
  , emptyPushdown
-- * predicates
  , isEmptyPushdown
  , isNonemptyPushdown
  , checkPushdown
-- * functions
  , pushPushdown
  , popPushdown
  ) where


-- | Data type for pushdowns.
type Pushdown a = [a]


-- | Constructs an empty 'Pushdown'.
emptyPushdown :: Pushdown a
emptyPushdown = []


-- | Checks if the 'Pushdown' is empty.
isEmptyPushdown :: Pushdown a -> Bool
isEmptyPushdown = null


-- | Checks if the 'Pushdown' is not empty.
isNonemptyPushdown :: Pushdown a -> Bool
isNonemptyPushdown = not . null


-- | Checks a predicate on the top-most pushdown symbol.
checkPushdown :: (a -> Bool) -> Pushdown a -> Bool
checkPushdown p = (&&) <$> not . null <*> p . head


-- | Pushes a value on top of a 'Pushdown'.
pushPushdown :: a -> Pushdown a -> [Pushdown a]
pushPushdown x = return . (x :)


-- | Pops the top-most value from the 'Pushdown'.
popPushdown :: Pushdown a -> [Pushdown a]
popPushdown = return . tail
