-------------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Matthias Büchse 2012
-- Maintainer  :  Matthias Büchse <matthias.buechse@tu-dresden.de>
-- Stability   :  unbekannt
-- Portability :  portable
--
-- Left : nonterminals
-- Right: terminals
-------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Vanda.Algorithms.Earley ( earley ) where

import qualified Control.Error
import Control.Monad ( unless, liftM3 )
import Control.Monad.ST
import Control.Seq
import qualified Data.Array as A ( array, elems )
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import qualified Data.Set as S
import qualified Data.Queue as Q
import qualified Data.Vector as VV
import qualified Data.Vector.Unboxed as V

-- import Debug.Trace

import Vanda.Hypergraph
import qualified Vanda.Algorithms.Earley.WSA as WSA
import Vanda.Util


errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.Algorithms.Earley"


-- Item: [iHead -> ??? * bRight] 
-- statelist contains states of a wsa. 
-- Ignoring the bullet, the first symbol of the right-hand side of an item
-- (not bRight) is wrapped by the first and the second state, the second
-- symbol is wrapped by the second and the third state etc.
data Item v l i t p
  = Item 
    { iHead :: !v
    , bRight :: ![Either Int t]
    , edge :: Hyperedge v l i
    , stateList :: ![p]
    , firstState :: !p
    , weight :: !Double
    }

iEdge :: Item v l i t p -> i
iEdge = ident . edge

instance (Show v, Show i, Show p, Show t) => Show (Item v l i t p) where
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

instance (Eq i, Eq p) => Eq (Item v l i t p) where
  i1 == i2 = (iEdge i1, stateList i1) == (iEdge $ i2, stateList i2)

instance (Ord i, Ord p) => Ord (Item v l i t p) where
  i1 `compare` i2
    = (iEdge i1, stateList i1) `compare` (iEdge i2, stateList i2)


seqHyperedge
  :: Strategy v -> Strategy l -> Strategy i -> Strategy (Hyperedge v l i)
seqHyperedge sv sl si he
  = case he of
      Nullary t l i -> sv t `seq` sl l `seq` si i
      Unary t f l i -> sv t `seq` sv f `seq` sl l `seq` si i
      Binary t f1 f2 l i -> sv t `seq` sv f1 `seq` sv f2 `seq` sl l `seq` si i
      Hyperedge t f l i -> sv t `seq` seqList sv (VV.toList f)
                           `seq` sl l `seq` si i

second3 :: (b -> d) -> (a, b, c) -> (a, d, c)
second3 f (a, b, c) = (a, f b, c)


-- | Intersect a context-free and a regular language.
--
-- The context-free laguage is represented by a 'Hypergraph', an initial
-- vertex, and a composition operation for each 'Hyperedge'. A composition
-- operation defines for a 'Hyperedge', how terminal strings from
-- subderivations are combined to a new terminal string (of type @[t]@). The
-- 'Right's introduce new terminal symbols to the string and the 'Left's are
-- replaced by the string of the subderivation referenced the 'Int'. The
-- subderivations correspond to the 'from' vertices of the 'Hyperedge'; their
-- counting is @0@-based. Every subderivation must be referenced exactly once.
--
-- The regular language is represented by a 'WSA.WSA'.
--
-- This function is a combination of the construction of Bar-Hillel, Perles,
-- Shamir (1961) and the parsing strategy of Earley (1970).
-- The result is a triple of
--
-- * a bijection between the constructed vertices and their 'Int'
--   representation in the resulting 'Hypergraph',
-- * a 'Hypergraph' representing the intersection, and
-- * a 'V.Vector' of weigths of the constructed 'Hyperedge's, assigned by the
--   second component of the 'Hyperedge'’s 'ident's.
--
-- The first component of each 'Hyperedge'’s 'ident' is the 'ident' from the
-- original 'Hyperedge' it was constructed from.
earley
  :: (Ord p, Ord t, Show i, Hypergraph h)
  => h Int l i              -- ^ 'Hypergraph'
  -> (l -> [Either Int t])  -- ^ composition operations for the 'Hyperedge's
  -> WSA.WSA p t Double
  -> Int                    -- ^ initial vertex
  -> (M.Map (p, Int, p) Int, EdgeList Int l (i, Int), V.Vector Double)
earley hg comp wsa v0
  = second3 mkHypergraph
  $ iter (backStar $ toBackwardStar hg) (comp . label) wsa v0 extract
  {- = (mkHypergraph $ map doIt trans, V.fromList theList)
  where
    trans = iter (backStar $ toBackwardStar hg) (comp . label) wsa v0 extract
    theList = S.toList $ S.fromList $ snd $ unzip trans
    theMap = M.fromList (zip theList [0..])
    doIt (he, w) = mapHEi (\ i -> (i, theMap M.! w)) he -}


extract
  :: (Hyperedge v l i -> [Either Int t])
  -> Item v l i t p
  -> Maybe (Hyperedge (p, v, p) l i, Double)
extract
  comp
  Item
    { bRight = []
    , firstState = p
    , iHead = q
    , stateList = statl@(p' : _)
    , edge = e
    , weight = w
    }
  = (`using` strategy)
  $ Just
  $ ( mkHyperedge
        (p, q, p')
        (carryL (yesyes (comp e) (reverse statl)) (from e))
        (label e)
        (ident e)
    , w -- L.product initial_weight
    )
  where
    -- strategy = r0
    strategy
      = seqMaybe
         (seqTuple2
            (seqHyperedge (seqTuple3 rseq rseq rseq) rseq rseq)
            r0
         )
    carryL yesbaby vert
      = [ (l, v, r)
        | (i, v) <- zip [0..] vert
        , let (l, _, r) = yesbaby M.! i
        ]
    yesyes lab wsast
      = M.fromList
          [ (i, x)
          | x@(_, Left i, _) <- zip3 wsast lab (tail wsast)
          ]
extract _ _ = Nothing


iter
  :: forall a i l p t
  .  (Ord p, Ord t, Show i)
  => (Int -> [Hyperedge Int l i]) 
  -> (Hyperedge Int l i -> [Either Int t])
  -> WSA.WSA p t Double
  -> Int            -- an initial node of the Hypergraph
  -> ((Hyperedge Int l i -> [Either Int t]) -> Item Int l i t p -> Maybe a)
  -> (M.Map (p, Int, p) Int, [Hyperedge Int l (i, Int)], V.Vector Double) -- a
iter back comp wsa v0 _
  = runST iterST
  where
    iterST :: forall s. ST s ( M.Map (p, Int, p) Int
                             , [Hyperedge Int l (i, Int)]
                             , V.Vector Double )
    iterST = do
      iq <- newSTRef Q.empty
      pvs <- newSTRef (S.fromList pv0)
      cm <- newSTRef M.empty
      ls <- newSTRef ([] :: [Hyperedge Int l (i, Int)])
      ws <- newSTRef (M.empty :: M.Map Double Int)
      cn <- newSTRef (0 :: Int)
      cn2 <- newSTRef (0 :: Int)
      s2n <- newSTRef (M.empty :: M.Map (p, Int, p) Int) 

      let enqueue :: [Item Int l i t p] -> ST s ()
          enqueue is
            = (is `using` seqList rseq) `seq` modifySTRef' iq $ Q.enqList is

          modifycm
            :: (  M.Map (p, Int) (S.Set p, [Item Int l i t p])
               -> M.Map (p, Int) (S.Set p, [Item Int l i t p]) )
            -> ST s ()
          modifycm = modifySTRef' cm

          predscan :: Item Int l i t p -> ST s ()
          predscan it@Item{ stateList = p : _ }
            = case bRight it of
                Left i : _ -> let pv = (p, edge it `deref` i) in do
                  b <- readSTRefWith (S.member pv) pvs
                  unless b $ do
                    modifySTRef' pvs $ S.insert pv
                    enqueue $ predItem pv
                Right t : _ -> enqueue $ scanItem it (p, t)
                _ -> return ()
          predscan Item{ stateList = [] }
            = errorHere "iter.iterST.predscan" "stateList = []"

          complete :: Item Int l i t p -> ST s ()
          complete it@Item{ stateList = p':_, iHead = v, firstState = p }
            = case bRight it of
                Left i : _ -> let pv = (p', edge it `deref` i) in do
                  mb <- readSTRefWith (M.lookup pv) cm
                  case mb of
                    Nothing -> modifycm $ MS.insert pv (S.empty, [it])
                    Just (ps, _) -> do
                      enqueue $ map (flip compItem it) (S.toList ps)
                      modifycm $ MS.adjust (second' (it :)) pv
                [] -> let pv = (p, v) in do
                  mb <- readSTRefWith (M.lookup pv) cm
                  case mb of
                    Nothing -> modifycm $ MS.insert pv (S.singleton p', [])
                    Just (ps, is) -> unless (p' `S.member` ps) $ do
                      enqueue $ map (compItem p') is
                      modifycm $ MS.adjust (first' (S.insert p')) pv
                _ -> return ()
          complete Item{ stateList = [] }
            = errorHere "iter.iterST.complete" "stateList = []"

          mapw :: Double -> ST s Int
          mapw w = do
            mb <- readSTRefWith (M.lookup w) ws
            case mb of
              Nothing -> do
                i <- readSTRef cn
                modifySTRef' cn (+ 1)
                modifySTRef' ws $ M.insert w i
                return i
              Just i -> return i

          register :: (p, Int, p) -> ST s ()
          register pip' = do
            mb <- readSTRefWith (M.lookup pip') s2n
            case mb of
              Nothing -> do
                i <- readSTRef cn2
                modifySTRef' cn2 (+ 1)
                modifySTRef' s2n $ M.insert pip' i
              Just _ -> return ()

          doExtract :: Item Int l i t p -> ST s ()
          doExtract it = case extract comp it of
            Nothing -> return ()
            Just (e, w) -> do
              i <- mapw w
              register (to e)
              s2n' <- readSTRef s2n
              let ide = ident e
                  e' = (mapHE (s2n' M.!) e){ ident = (ide, i) }
              ide `seq` e' `seq` modifySTRef' ls (e' :)

          go2 :: ST s ( M.Map (p, Int, p) Int
                      , [Hyperedge Int l (i, Int)]
                      , V.Vector Double )
          go2 = do
            viewSTRef' iq Q.deqMaybe
              (readSTRef cn >>= \ cn' -> liftM3 (,,)
                (readSTRef s2n)
                (readSTRef ls)
                (fmap (V.fromList . A.elems . A.array (0, cn' - 1)
                . map (\(x,y) -> (y,x)) . M.toList) (readSTRef ws)))
              $ \ i -> do -- traceShow i $ do
                predscan i
                complete i
                doExtract i
                -- modifySTRef' ls (maybe id (:) (extract comp i))
                go2

      mapM_ (enqueue . predItem) pv0
      go2

    pv0 :: [(p, Int)]
    pv0 = [ (p, v0) | (p, _) <- WSA.initialWeights wsa ]

    predItem :: (p0, Int) -> [Item Int l i t p0]
    predItem (p, v) = [ Item v (comp e) e [p] p 1 | e <- back v ]

    scanItem :: Item v l i t p -> (p, t) -> [Item v l i t p]
    scanItem it@Item{ bRight = _ : bs, stateList = ps, weight = w } pt
      = [ it{ bRight = bs, stateList = p : ps, weight = w' * w }
        | (p, w') <- M.findWithDefault [] pt scanMap ]
    scanItem Item{ bRight = [] } _
      = errorHere "iter.scanItem" "bRight = []"

    compItem :: p -> Item v l i t p -> Item v l i t p
    compItem p' i@Item{ stateList = ps, bRight = _:bright' }
      = i{ stateList = p' : ps, bRight = bright' }
    compItem _ Item{ bRight = [] }
      = errorHere "iter.compItem" "bRight = []"

    scanMap :: MS.Map (p, t) [(p, Double)]
    scanMap
      = M.fromListWith (++)
          [ ( (WSA.transStateIn t, WSA.transTerminal t)
            , [(WSA.transStateOut t, WSA.transWeight t)]
            )
          | t <- WSA.transitions wsa
          ]

