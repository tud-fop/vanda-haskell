{-# LANGUAGE BangPatterns, RecordWildCards #-}

module Vanda.Grammar.XRS.IRTG
 ( StrictIntPair (..)
 , IRTG (..)
 , XRS (..)
 ) where

import Control.DeepSeq ( NFData, rnf )
import qualified Data.Binary as B
import Data.NTT
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Vanda.Hypergraph.IntHypergraph
import qualified Vanda.Hypergraph.Tree as T


data StrictIntPair
  = SIP
    { _fst :: !Int
    , _snd :: !Int
    } deriving (Eq, Ord, Show)

instance NFData StrictIntPair where
  rnf a = a `seq` ()

instance B.Binary StrictIntPair where
  get = SIP <$> B.get <*> B.get
  put (SIP a b) = B.put a >> B.put b


data IRTG i
  = IRTG
    { rtg :: Hypergraph StrictIntPair i
    , initial :: Int
    , h1 :: V.Vector (T.Tree NTT)
    , h2 :: V.Vector (V.Vector NTT)
    }

instance (B.Binary i, NFData i) => B.Binary (IRTG i) where
  get = do
          !rtg <- B.get
          !initial <- B.get
          !h1 <- fmap V.fromList B.get
          !h2 <- fmap (V.fromList . map V.fromList) B.get
          return $! IRTG { .. }
  put IRTG{ .. } = B.put rtg >> B.put initial >> B.put (V.toList h1) >> B.put (map V.toList $ V.toList h2)


data XRS
  = XRS
    { irtg :: IRTG Int
    , weights :: VU.Vector Double
    }

instance Show XRS where
  show (XRS (IRTG hg _ h1 h2) w)
    = unlines
    . map (\ he -> (cut 2 . show . to $ he)
                ++ " <- "
                ++ (cut 10 . show . from $ he)
                ++ " # "
                ++ (cut 5 . show . (VU.!) w . ident $ he)
                ++ " || "
                ++ (cut 30 . show . (V.!) h1 . _fst . label $ he)
                ++ " | "
                ++ (show . (V.!) h2 . _snd . label $ he)
          )
    . edges
    $ hg

cut :: Int -> [Char] -> [Char]
cut n = take n . (++ repeat ' ')
