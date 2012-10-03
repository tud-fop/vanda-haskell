{-# LANGUAGE RecordWildCards #-}
module Vanda.Grammar.XRS.Functions where

import Codec.Compression.GZip ( decompress )
import Control.DeepSeq ( NFData )

import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe ( listToMaybe )
import Data.NTT
import qualified Data.Set as S
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V

import Vanda.Algorithms.IntEarley
import qualified Vanda.Algorithms.Earley.WSA as WSA
import Vanda.Grammar.XRS.Binary ()
import Vanda.Grammar.XRS.IRTG
import Vanda.Hypergraph.IntHypergraph
import qualified Vanda.Hypergraph.Tree as T
import Vanda.Token

instance NFData StrictIntPair

toWSAmap :: TokenMap -> String -> WSA.WSA Int Token Double 
toWSAmap tm = WSA.fromList 1.0 . map (getToken tm) . L.words

loadIRTG :: String -> IO (IRTG Int)
loadIRTG = fmap (B.decode . decompress) . B.readFile

loadTokenMap :: String -> IO TokenMap
loadTokenMap = fmap fromText . TIO.readFile

loadTokenArray :: String -> IO TokenArray
loadTokenArray = fmap fromText . TIO.readFile

loadWeights :: String -> IO (V.Vector Double)
loadWeights = fmap (V.fromList . B.decode . decompress) . B.readFile

getTerminals :: Ord t => WSA.WSA Int t Double -> S.Set t
getTerminals = S.fromList . map WSA.transTerminal . WSA.transitions

prune :: (l -> [NTT]) -> S.Set Int -> Hypergraph l i -> Hypergraph l i
prune comp s hg
  = Hypergraph (nodes hg) (filter p $ edges hg)
  where
    p e = arity e <= 2 && L.foldl' p' True (comp (label e))
    p' b (NT _) = b
    p' b (T i) = b && i `S.member` s



inputProduct :: IRTG Int -> WSA.WSA Int Int Double -> IRTG Int
inputProduct irtg@IRTG{ .. } wsa
  = let comp = ((h2 V.!) . _snd)
        rrtg = dropNonproducing $ prune comp (getTerminals wsa) rtg
        (mm, ip, _) = earley rrtg comp wsa fst initial
    in irtg{ rtg = ip, initial = mm M.!
                           (0, initial, fst . head . WSA.finalWeights $ wsa)
           }

bestDeriv
  :: IRTG Int -> V.Vector Double -> Maybe (Candidate StrictIntPair Int)
bestDeriv IRTG{ .. } ws
  = let feat _ i xs = (if i < 0 then 1 else ws V.! i) * product xs
        ba = knuth rtg feat
    in listToMaybe (ba A.! initial)

getOutputTree
 :: IRTG Int -> Maybe (Candidate StrictIntPair Int) -> Maybe (T.Tree Int)
getOutputTree irtg = fmap (getOutputTree' irtg . deriv)

getOutputTree' :: IRTG Int -> Derivation StrictIntPair Int -> T.Tree Int
getOutputTree' irtg@IRTG{ .. } n
  = subst $ (h1 V.!) $ _fst $ label $ T.rootLabel n
  where
    ts = map (getOutputTree' irtg) $ T.subForest n
    subst (T.Nullary (NT i)) = ts !! i
    subst t = case T.rootLabel t of
                T i -> T.node i (map subst (T.subForest t))

toString :: TokenArray -> Maybe (T.Tree Int) -> String
toString _ Nothing = "(no parse)"
toString ta (Just t) = toString' ta t

toString' :: TokenArray -> T.Tree Int -> String
toString' ta = go
  where
    gs i = if i < 0 then "@" else getString ta i
    go (T.Nullary i) = gs i
    go (T.Unary i t) = "(" ++ gs i ++ " " ++ go t ++ ")"
    go (T.Binary i t1 t2)
      = "(" ++ gs i ++ " " ++ go t1 ++ " "  ++ go t2 ++ ")"
    go (T.Node i sF)
      = "(" ++ gs i ++ " " ++ (unwords (map go sF)) ++ ")"

--      print $ map (fmap (nttToString em em) . derivToTree ((h1 V.!) . _fst) . deriv)

