{-# LANGUAGE RecordWildCards #-}
module Vanda.Grammar.XRS.Functions where

import Codec.Compression.GZip ( decompress )

import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import qualified Data.List as L
import qualified Data.Map as M
import Data.NTT
import qualified Data.Set as S
import qualified Data.Text.Lazy as TS
-- import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy.IO as TSIO
import qualified Data.Vector as V

import Vanda.Algorithms.IntEarley
import qualified Vanda.Algorithms.Earley.WSA as WSA
import Vanda.Grammar.XRS.Binary ()
import Vanda.Grammar.XRS.IRTG
import Vanda.Hypergraph.IntHypergraph
import qualified Vanda.Hypergraph.Tree as T
import Vanda.Token


loadText :: String -> IO String
loadText file = fmap (TS.unpack . head . TS.lines) $ TIO.readFile file

saveText :: String -> String -> IO ()
saveText text file = TIO.writeFile file (TS.pack text)


toWSAmap :: TokenMap -> TS.Text -> WSA.WSA Int Token Double 
toWSAmap tm = WSA.fromList 1.0 . map (getToken tm) . TS.words

toWSAMap :: TS.Text -> TokenMap -> WSA.WSA Int Token Double
toWSAMap = flip toWSAmap

loadIRTG :: String -> IO (IRTG Int)
loadIRTG = fmap (B.decode . decompress) . B.readFile

loadTokenMap :: String -> IO TokenMap
loadTokenMap = fmap fromText . TSIO.readFile

loadTokenArray :: String -> IO TokenArray
loadTokenArray = fmap fromText . TSIO.readFile

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


prepareInputProduct :: IRTG Int -> (IRTG Int, Int -> Trie StrictIntPair Int)
prepareInputProduct irtg@IRTG{ .. }
  = (irtg{ rtg = rtg' }, toBackwardStar rtg' comp)
  where
    rtg' = {-dropNonproducing-} rtg{ edges = es }
    es = filter ((<= 2) . arity) (edges rtg)
    comp = ((h2 V.!) . _snd)


doInputProduct
  :: WSA.WSA Int Int Double
  -> (IRTG Int, Int -> Trie StrictIntPair Int)
  -> IRTG Int
doInputProduct wsa (irtg@IRTG{ .. }, bs)
  = let comp = ((h2 V.!) . _snd)
        (mm, ip, _) = earley bs comp wsa fst initial
        initial' = mm M.! (0, initial, fst . head . WSA.finalWeights $ wsa)
    in initial' `seq` irtg{ rtg = ip, initial = initial' }


inputProduct :: WSA.WSA Int Int Double -> IRTG Int -> IRTG Int
inputProduct wsa irtg@IRTG{ .. }
  = let comp = ((h2 V.!) . _snd)
        -- rrtg = dropNonproducing $ prune comp (getTerminals wsa) rtg
        (mm, ip, _) = earley (toBackwardStar rtg comp) comp wsa fst initial
        initial' = mm M.! (0, initial, fst . head . WSA.finalWeights $ wsa)
    in initial' `seq` irtg{ rtg = ip, initial = initial' }

bestDeriv :: IRTG Int -> V.Vector Double -> [Candidate StrictIntPair Int]
bestDeriv IRTG{ .. } ws
  = let feat _ i xs = (if i < 0 then 1 else ws V.! i) * product xs
        ba = knuth rtg feat
    in ba A.! initial

getOutputTree :: IRTG Int -> [Candidate StrictIntPair Int] -> [T.Tree Int]
getOutputTree irtg = map (getTree' ((h1 irtg V.!) . _fst) . deriv)

getTree'
  :: (StrictIntPair -> T.Tree NTT)
  -> Derivation StrictIntPair Int
  -> T.Tree Int
getTree' comp n
  = subst $ comp $ label $ T.rootLabel n
  where
    ts = map (getTree' comp) $ T.subForest n
    subst (T.Nullary (NT i)) = ts !! i
    subst t = case T.rootLabel t of
                T i -> T.node i (map subst (T.subForest t))

toString :: TokenArray -> [T.Tree Int] -> TS.Text
toString _ [] = TS.pack "(no parse)"
toString ta (t : _) = toString' ta t

att :: TS.Text
att = TS.singleton '@'

lrb :: TS.Text
lrb = TS.singleton '('

rrb :: TS.Text
rrb = TS.singleton ')'

spa :: TS.Text
spa = TS.singleton ' '

toString' :: TokenArray -> T.Tree Int -> TS.Text
toString' ta = go
  where
    gs i = if i < 0 then att else getString ta i
    go (T.Nullary i) = gs i
    go (T.Unary i t) = TS.concat [lrb, gs i, spa, go t, rrb]
    go (T.Binary i t1 t2)
      = TS.concat [lrb, gs i, spa, go t1, spa, go t2, rrb]
    go (T.Node i sF)
      = TS.concat [lrb, gs i, spa, TS.unwords (map go sF), rrb]

