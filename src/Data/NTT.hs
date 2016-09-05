{-# LANGUAGE FlexibleInstances #-}

module Data.NTT (NTT (..), Var (..), tt, nt, var) where

import qualified Data.Vector as V

import Control.DeepSeq (NFData, rnf)
import qualified Control.Error
import Control.Seq
import qualified Data.Binary as B
import Data.Hashable ( Hashable (..) )

errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Data.NTT"

data NTT = NT !Int | T !Int deriving (Eq, Ord, Show, Read)

instance NFData NTT where
  rnf a = a `seq` ()

instance B.Binary NTT where
  {- get = B.getWord8 >>= \ x ->
        case x of
          0 -> NT <$> B.get
          1 -> T <$> B.get
          _ -> error "corrupt NTT data stream" -}
  get = do
          x <- B.getWord8
          y <- B.get :: B.Get Int
          return $! case x of { 0 -> nt y ; 1 -> tt y ; _ -> errorHere "get" $ "unexpected word " ++ show x }
  put (NT x) = B.putWord8 0 >> B.put x
  put (T x) = B.putWord8 1 >> B.put x

instance Hashable NTT where
  hashWithSalt salt (NT i) = hashWithSalt salt (Left i :: Either Int Int)
  hashWithSalt salt (T i) = hashWithSalt salt (Right i :: Either Int Int)

instance Hashable (V.Vector NTT) where
  hashWithSalt salt = hashWithSalt salt . V.toList

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



