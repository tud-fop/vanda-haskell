module Data.NTT (NTT (..), Var (..), tt, nt, var) where

import qualified Data.Vector as V

import Control.Seq
import Data.Hashable ( Hashable (..) )

data NTT = NT !Int | T !Int deriving (Eq, Ord, Show)

instance Hashable NTT where
  hashWithSalt salt (NT i) = hashWithSalt salt (Left i :: Either Int Int)
  hashWithSalt salt (T i) = hashWithSalt salt (Right i :: Either Int Int)

data Var l = Var Int | NV l deriving (Eq, Ord, Show)

ts :: V.Vector NTT
ts = let ts' = V.fromList [ T i | i <- [0 .. 129999] ]
     in (V.toList ts' `using` seqList rseq) `seq` ts'

nts :: V.Vector NTT
nts = let nts' = V.fromList [ NT i | i <- [0 .. 59] ]
      in (V.toList nts' `using` seqList rseq) `seq` nts'

tt :: Int -> NTT
tt i = if 0 <= i && i < 130000 then ts V.! i else T i

nt :: Int -> NTT
nt i = if i < 60 then nts V.! i else NT i

vars :: V.Vector (Var l)
vars = V.fromList [ Var i | i <- [0 .. 59] ]

var :: Int -> Var l
var i = if i < 60 then vars V.! i else Var i



