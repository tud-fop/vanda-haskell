-- (c) 2012 Matthias Büchse <Matthias.Buechse@mailbox.tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

-- |
-- Maintainer  :  Matthias Büchse
-- Stability   :  unbekannt
-- Portability :  portable

module Vanda.Algorithms.IntEarley ( earley, NTT (..) ) where

import Control.Monad ( unless, liftM3, forM_ )
import Control.Monad.ST
import Control.Seq
import qualified Data.Array as A ( array, elems )
import Data.List ( foldl' )
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import Data.NTT
import qualified Data.Set as S
import Data.STRef
import qualified Data.Queue as Q
import qualified Data.Vector.Unboxed as VU

-- import Debug.Trace

import Vanda.Hypergraph.IntHypergraph hiding ( weight )
import qualified Vanda.Algorithms.Earley.WSA as WSA
import Vanda.Util


data Trie l i
  = Trie
    { subTries :: MS.Map NTT (Trie l i)
    , bin :: [Hyperedge l i]
    }
    deriving Show

emptyTrie :: Trie l i
emptyTrie = Trie MS.empty []

addRule :: Hyperedge l i -> [NTT] -> Trie l i -> Trie l i
addRule e [] t@Trie{ bin = b } = t{ bin = (e : b) }
addRule e (ntt : ntts) t@Trie{ subTries = sub }
  = let st = MS.alter f ntt' sub in st `seq` t{ subTries = st }
  where
    f Nothing = Just $ addRule e ntts emptyTrie
    f (Just t') = Just $ addRule e ntts t'
    ntt' = case ntt of
      T _  -> ntt
      NT i -> NT (e `deref` i)

data Item l i p
  = Item 
    { iHead :: !Int
    , trie :: Trie l i
    , stateList :: ![p]
    , firstState :: !p
    , weight :: !Double
    }
    deriving Show

{-instance (Show i, Show p) => Show (Item l i p) where
  show item
    = show (iHead item)
      ++ " -> ???"
      ++ " * "
      ++ show (bRight item)
      ++ "\n   "
      ++ show (iEdge item)
      ++ "\n   "
      ++ show (stateList item)
      ++ "\n"
-}
{-instance (Eq i, Eq p) => Eq (Item l i p) where
  i1 == i2 = (iEdge i1, stateList i1) == (iEdge $ i2, stateList i2)

instance (Ord i, Ord p) => Ord (Item l i p) where
  i1 `compare` i2
    = (iEdge i1, stateList i1) `compare` (iEdge i2, stateList i2)-}


seqHyperedge
  :: Strategy Int -> Strategy l -> Strategy i -> Strategy (Hyperedge l i)
seqHyperedge sv sl si he
  = case he of
      Nullary t l i -> sv t `seq` sl l `seq` si i
      Unary t f l i -> sv t `seq` sv f `seq` sl l `seq` si i
      Binary t f1 f2 l i -> sv t `seq` sv f1 `seq` sv f2 `seq` sl l `seq` si i
      Hyperedge t f l i -> sv t `seq` seqList sv (VU.toList f)
                           `seq` sl l `seq` si i

second3 :: (b -> d) -> (a, b, c) -> (a, d, c)
second3 f (a, b, c) = (a, f b, c)


toBackwardStar :: Hypergraph l i -> (l -> [NTT]) -> Int -> Trie l i
toBackwardStar (Hypergraph _ es) f = flip (MS.findWithDefault emptyTrie) a
  where
    lst = [ (v, e) | e <- es, let v = to e ]
    a = foldl' (\m (v, e) -> MS.alter (prep e) v m) M.empty lst
    prep e Nothing = Just $ addRule e (f (label e)) emptyTrie
    prep e (Just t) = Just $ addRule e (f (label e)) t



earley
  :: (Ord p, Ord i, Show p, Show i, Show l)
  => Hypergraph l i
  -> (l -> [NTT])
  -> WSA.WSA p Int Double
  -> ((i, Int) -> i')
  -> Int            -- an initial node of the Hypergraph
  -> (M.Map (p, Int, p) Int, Hypergraph l i', VU.Vector Double)
earley hg comp wsa mki' v0
  = second3 mkHypergraph
  $ iter (toBackwardStar hg comp) (comp . label) wsa mki' v0


iter
  :: forall l i p i'. (Ord p, Ord i, Show p, Show i, Show l)
  => (Int -> Trie l i) 
  -> (Hyperedge l i -> [NTT])
  -> WSA.WSA p Int Double
  -> ((i, Int) -> i')
  -> Int            -- an initial node of the Hypergraph
  -> (M.Map (p, Int, p) Int, [Hyperedge l i'], VU.Vector Double)
iter back comp wsa mki' v0
  = runST $ do
      iq <- newSTRef Q.empty
      pvs <- newSTRef (S.fromList pv0)
      cm <- newSTRef M.empty
      ls <- newSTRef ([] :: [Hyperedge l i'])
      ws <- newSTRef (M.empty :: M.Map Double Int)
      cn <- newSTRef (0 :: Int)
      cn2 <- newSTRef (0 :: Int)
      s2n <- newSTRef (M.empty :: M.Map (p, Int, p) Int) 
      let enqueue is
            = (is `using` seqList rseq) `seq` modifySTRef' iq $ Q.enqList is
          modifycm = modifySTRef' cm
          predscan it@Item{ stateList = p : _ } (ntt, subtrie)
            = case ntt of
                NT v -> let pv = (p, v) in do
                  b <- readSTRefWith (S.member pv) pvs
                  unless b $ do
                    modifySTRef' pvs $ S.insert pv
                    enqueue $ [predItem pv]
                T t -> enqueue $ scanItem it (p, t) subtrie
          predscan _ _ = undefined -- can not happen
          complete1 it@Item{ stateList = p' : _ } (ntt, subtrie)
            = case ntt of
                NT v -> let pv = (p', v) in do
                  mb <- readSTRefWith (M.lookup pv) cm
                  case mb of
                    Nothing -> modifycm $ MS.insert pv (S.empty, [it])
                    Just (ps, _) -> do
                      enqueue $ map (compItem1 subtrie it) (S.toList ps)
                      modifycm $ MS.adjust (second' (it :)) pv
                _ -> return ()
          complete1 _ _ = undefined -- can not happen
          complete2 Item{ stateList = p':_, iHead = v, firstState = p }
            = let pv = (p, v) in do
                mb <- readSTRefWith (M.lookup pv) cm
                case mb of
                  Nothing -> modifycm $ MS.insert pv (S.singleton p', [])
                  Just (ps, is) -> unless (p' `S.member` ps) $ do
                    enqueue $ map (compItem2 (NT v) p') is
                    modifycm $ MS.adjust (first' (S.insert p')) pv
          complete2 _ = undefined -- can not happen
          mapw w = do
            mb <- readSTRefWith (M.lookup w) ws
            case mb of
              Nothing -> do
                i <- readSTRef cn
                modifySTRef' cn (+ 1)
                modifySTRef' ws $ M.insert w i
                return i
              Just i -> return i
          register pip' = do
            mb <- readSTRefWith (M.lookup pip') s2n
            case mb of
              Nothing -> do
                i <- readSTRef cn2
                modifySTRef' cn2 (+ 1)
                modifySTRef' s2n $ M.insert pip' i
                return i
              Just i -> return i
          doExtract it e = case it of
            Item{ firstState = p, iHead = q, stateList = statl@(p' : _)
                , weight = w }
              -> let pqp' = (p, q, p') in do
                i <- mapw w
                pqpi <- register pqp'
                s2n' <- readSTRef s2n
                let e' = mkHyperedge pqpi
                          (map (s2n' M.!) (carryL (yesyes (comp e)
                                            (reverse statl)) (from e)))
                          (label e)
                          (mki' (ident e, i))
                         `using`
                         (seqHyperedge rseq r0 rseq {-(seqTuple2 rseq r0)-})
                e' `seq` modifySTRef' ls (e' :)
            _ -> return ()
          go2 = do
            viewSTRef' iq Q.deqMaybe
              (readSTRef cn >>= \ cn' -> liftM3 (,,)
                (readSTRef s2n)
                (readSTRef ls)
                (fmap (VU.fromList . A.elems . A.array (0, cn') 
                . map (\(x,y) -> (y,x)) . M.toList) (readSTRef ws)))
              $ \ i@Item{ trie = t } -> do
                forM_ (M.assocs (subTries t)) $
                  \x -> predscan i x >> complete1 i x
                unless (null (bin t)) (complete2 i)
                mapM_ (doExtract i) (bin t)
                go2
      mapM_ (enqueue . (: []) . predItem) pv0
      go2
  where
    pv0 = [ (p, v0) | (p, _) <- WSA.initialWeights wsa ]
    predItem (p, v) = let t = back v in t `seq` Item v t [p] p 1
    scanItem it@Item{ stateList = ps, weight = w } pt subtrie
      = [ it{ trie = subtrie, stateList = p : ps, weight = w' * w }
        | (p, w') <- M.findWithDefault [] pt scanMap ]
    compItem1 subtrie i@Item{ stateList = ps } p'
      = i{ stateList = p' : ps, trie = subtrie }
    compItem2 v p' i@Item{ stateList = ps, trie = t }
      = let t' = subTries t M.! v
        in t' `seq` i{ stateList = p' : ps, trie = t' } 
    scanMap
      = M.fromListWith (++)
          [ ( (WSA.transStateIn t, WSA.transTerminal t)
            , [(WSA.transStateOut t, WSA.transWeight t)]
            )
          | t <- WSA.transitions wsa
          ]
    carryL yesbaby vert
      = [ (l, v, r)
        | (i, v) <- zip [0..] vert
        , let (l, _, r) = yesbaby M.! i
        ]
    yesyes lab wsast
      = M.fromList
          [ (i, x)
          | x@(_, NT i, _) <- zip3 wsast lab (tail wsast)
          ]

