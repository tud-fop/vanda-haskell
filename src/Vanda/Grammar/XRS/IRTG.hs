module Vanda.Grammar.XRS.IRTG where

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

data IRTG i
  = IRTG
    { rtg :: Hypergraph StrictIntPair i
    , initial :: Int
    , h1 :: V.Vector (T.Tree NTT)
    , h2 :: V.Vector [NTT]
    }

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
cut n s = take n . flip (++) (repeat ' ') $ s