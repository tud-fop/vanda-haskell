module Data.NTT (NTT (..), Var (..), tt, nt, var) where

import qualified Data.Vector as V

data NTT = NT !Int | T !Int deriving (Eq, Ord, Show)

data Var l = Var Int | NV l deriving (Eq, Ord, Show)

ts = V.fromList [ T i | i <- [0 .. 129999] ]
nts = V.fromList [ NT i | i <- [0 .. 9] ]

tt i = if i < 130000 then ts V.! i else T i
nt i = if i < 10 then nts V.! i else NT i

vars = V.fromList [ Var i | i <- [0 .. 9] ]
var i = if i < 10 then vars V.! i else Var i



