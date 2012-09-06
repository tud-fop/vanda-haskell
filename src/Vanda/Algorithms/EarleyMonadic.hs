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

-- Left : nonterminals
-- Right: terminals

module Vanda.Algorithms.EarleyMonadic ( earley ) where

import Control.Arrow ( first, second )
import Control.Monad ( unless )
import Control.Monad.ST
import Control.Seq
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import qualified Data.Set as S
import Data.STRef
import qualified Data.Queue as Q
import qualified Data.Vector as VV
import qualified Data.Vector.Unboxed as V
import Debug.Trace

import Vanda.Hypergraph
import qualified Vanda.Algorithms.Earley.WSA as WSA

-- Item: [iHead -> ??? * bRight] 
-- statelist contains states of a wsa. 
-- Ignoring the bullet, the first symbol of the righthandside of an item
-- (not bRight) is wrapped by the first and the second state, the second symbol 
-- is wrapped by the second and the third state etc.
data Item v l i t p
  = Item 
    { iHead :: !v
    , bRight :: ![Either Int t]
    , edge :: Hyperedge v l i
    , stateList :: ![p]
    , firstState :: !p
    , weight :: !Double
    }

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


data Const v l i t p
  = Const
    { back :: !(v -> [Hyperedge v l i])
    -- , byId :: !(i -> Hyperedge v l i)
    , comp :: !(Hyperedge v l i -> [Either Int t])
    , v0 :: !v
    , smap :: !(M.Map (p, t) [(p, Double)])
      -- ^ Another representation of 'WSA' transitions. An incoming state
      -- and a terminal are mapped to a list of outcoming states and weights.
    }

data State v l i t p s
  = State
    { itemq :: STRef s (Q.Queue (Item v l i t p))
    , pset :: STRef s (S.Set (p, v))   -- ^ remember what has already been predicted
    , cmap :: STRef s (M.Map (p, v) (S.Set p, [Item v l i t p]))
    }


seqHyperedge
  :: Strategy v -> Strategy l -> Strategy i -> Strategy (Hyperedge v l i)
seqHyperedge sv sl si he
  = case he of
      Nullary t l i -> sv t `seq` sl l `seq` si i
      Unary t f l i -> sv t `seq` sv f `seq` sl l `seq` si i
      Binary t f1 f2 l i -> sv t `seq` sv f1 `seq` sv f2 `seq` sl l `seq` si i
      Hyperedge t f l i -> sv t `seq` seqList sv (VV.toList f)
                           `seq` sl l `seq` si i


seqMaybe :: Strategy a -> Strategy (Maybe a)
seqMaybe _ Nothing = ()
seqMaybe s (Just a) = s a `seq` ()


seqEither :: Strategy a -> Strategy b -> Strategy (Either a b)
seqEither sa _ (Left a)  = sa a
seqEither _ sb (Right b) = sb b


earley
  :: (Ord p, Ord v, Ord i, Ord t, Show p, Show v, Show i, Show t, Hypergraph h)
  => h v l i
  -> (l -> [Either Int t])
  -> WSA.WSA p t Double
  -> v            -- an initial node of the Hypergraph
  -> (EdgeList (p, v, p) l (i, Int), V.Vector Double)
earley hg component wsa v0
  = (mkHypergraph $ map doIt trans, V.fromList (snd (unzip theList)))
  where
    bs = toBackwardStar hg
    constt = initConst bs component wsa v0
    trans = iter extract wsa constt -- (initState wsa constt)
    theList = S.toList $ S.fromList $ snd $ unzip trans
    theMap = M.fromList (zip theList [0..])
    doIt (he, w) = mapHEi (\ i -> (i, theMap M.! w)) he


initConst
  :: (Ord p, Ord v, Ord t, Ord i, Show v, Show i, Show p, Show t)
  => BackwardStar v l i
  -> (l -> [Either Int t])
  -> WSA.WSA p t Double
  -> v            -- an initial node of the Hypergraph
  -> Const v l i t p
initConst hg component wsa v00
  = state
  where
    state
      = Const
        { back = backStar hg
        -- , byId = ((M.fromList idList) M.!)
        , comp = component . label
        , v0 = v00
        , smap = initScanMap (WSA.transitions wsa)
        }
    initScanMap ts
      = M.fromListWith (++)
          [ ( (WSA.transStateIn t, WSA.transTerminal t)
            , [(WSA.transStateOut t, WSA.transWeight t)]
            )
          | t <- ts
          ]
    {- idList
      = [ (ident e, e)
        | v <- S.toList (nodes hg)
        , e <- backStar hg v
        ] -}


extract
  :: (Show v, Show p, Show i, Show t)
  => Item v l i t p
  -> Const v l i t p
  -> Maybe (Hyperedge (p, v, p) l i, ([p], Double))
extract
  Item
    { bRight = []
    , firstState = p
    , iHead = q
    , stateList = statl
    , edge = edge
    , weight = w
    }
  Const{ {-byId = byid,-} comp = compo }
  = (`using` strategy)
  $ Just
  $ ( mkHyperedge 
        (p, q, head statl)
        (carryL (yesyes (compo edge) (reverse statl)) (from edge))
        (label edge)
        (ident edge)
    , ( statl
      , w -- * L.product initial_weight
      )
    )
  where
    -- strategy = r0
    strategy
      = seqMaybe
         (seqTuple2
            (seqHyperedge (seqTuple3 rseq rseq rseq) rseq rseq)
            (seqTuple2 r0 r0)
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

-- | Strict version of 'modifySTRef' 
modifySTRef' :: STRef s a -> (a -> a) -> ST s () 
modifySTRef' ref f = do 
  x <- readSTRef ref 
  let x' = f x 
  x' `seq` writeSTRef ref x' 

iter
  :: forall v l i t p a. (Ord p, Ord v, Ord t, Ord i, Show p, Show v, Show t, Show i)
  => (Item v l i t p -> Const v l i t p -> Maybe a)
  -> WSA.WSA p t Double
  -> Const v l i t p
  -> [(Hyperedge (p, v, p) l i, ([p], Double))] -- a
iter _ wsa constt@Const{ v0 = v00 }
  = runST $ do
      iq <- newSTRef Q.empty
      ps <- newSTRef S.empty
      cm <- newSTRef M.empty
      ls <- newSTRef ([] :: [(Hyperedge (p, v, p) l i, ([p], Double))])
      let s = State iq ps cm
      go1 s [ (p, v00) | (p, _) <- WSA.initialWeights wsa ]
      go2 s ls
  where
    go1 :: State v l i t p s -> [(p, v)] -> ST s ()
    go1 s [] = return ()
    go1 s (x:xs) = predict' constt x s >> go1 s xs
    go2 :: State v l i t p s -> STRef s [(Hyperedge (p, v, p) l i, ([p], Double))] -> ST s [(Hyperedge (p, v, p) l i, ([p], Double))]
    go2 s ls = do
      iq <- readSTRef (itemq s)
      case Q.deqMaybe iq of
        Nothing -> readSTRef ls
        Just (i, itemq')
          -> do
            writeSTRef (itemq s) itemq'
            predict constt i s
            scan constt i s
            complete i s
            modifySTRef' ls (maybe id (:) (extract i constt))
            go2 s ls


predict
  :: (Ord p, Ord v, Ord t, Ord i, Show v, Show i, Show p, Show t)
  => Const v l i t p
  -> Item v l i t p
  -> State v l i t p s
  -> ST s ()
predict constt item
  = {-# SCC predict #-} case {-# SCC predicta #-} bRight item of
      Left i:_ -> {-# SCC predict0 #-} predict' constt ({-# SCC predictb #-} (head (stateList item), edge item `deref` i))
      _ -> const $ return ()

predict'
  :: (Ord p, Ord v, Ord t, Ord i, Show v, Show i, Show p, Show t)
  => Const v l i t p
  -> (p, v)
  -> State v l i t p s
  -> ST s ()
predict' c pv@(p, v) s
  = do
      ps <- readSTRef (pset s)
      unless (S.member pv ps) $ do
        let ps' = S.insert pv ps in ps' `seq` writeSTRef (pset s) ps'
        itemlist `seq` modifySTRef' (itemq s) (Q.enqList itemlist)
  where
    itemlist
      = [ Item v (comp c edge) edge [p] p 1
        | edge <- back c v
        -- , let conv = (either (Left . (deref edge)) (Right . id))
        {-, let vec = case edge of
                      Nullary{} -> VV.empty
                      Unary{ from1 = x1 } -> VV.singleton x1
                      Binary{ from1 = x1, from2 = x2 } -> VV.fromList [x1, x2]
                      Hyperedge{ _from = xs } -> xs-}
        ]
        `using` seqList rseq


scan
  :: (Ord p, Ord v, Ord t, Ord i, Show v, Show i, Show p, Show t)
  => Const v l i t p
  -> Item v l i t p
  -> State v l i t p s
  -> ST s ()
scan c
     item@Item{ bRight = Right t : bright'
              , stateList = slist
              , weight = weyght }
     s
  = modifySTRef' (itemq s) (Q.enqList itemlist)
  where
    itemlist
      = [ id -- mytrace
        $ item
            { bRight = bright'
            , stateList = p:slist
            , weight = w * weyght
            }
        | (p, w) <- M.findWithDefault [] (head $ slist, t) (smap c)
        ]
scan _ _ s = return ()


first' f p = case p of
               (x, y) -> let fx = f x in fx `seq` (fx, y)

second' f p = case p of
               (x, y) -> let fy = f y in fy `seq` (x, fy)

complete
  :: (Ord p, Ord v, Ord t, Ord i, Show v, Show i, Show p, Show t)
  => Item v l i t p
  -> State v l i t p s
  -> ST s ()
complete it@Item{ stateList = p:_, bRight = Left i:_ } s@State{ cmap = zmap }
  = {-# SCC complete_1 #-}
    do
      cm <- readSTRef zmap
      let v = edge it `deref` i
      let ps' = maybe [] (S.toList . fst) (M.lookup (p, v) cm)
      let is' = map (flip completeItem it) ps' `using` seqList rseq
      is' `seq` modifySTRef' (itemq s) (Q.enqList is')
        -- (Q.enqListWith (flip completeItem i) ps')
      let cm' = MS.alter
                  (Just . maybe (S.empty, [it]) (second' (it:)))
                  -- (\x -> Just $ (`using` seqTuple2 rseq)
                  --             $ maybe (S.empty, [it]) (second (it:)) x)
                  (p, v)
                  cm
          in writeSTRef zmap cm'
complete Item{ iHead = v, stateList = p':_, firstState = p, bRight = [] } s@State{ cmap = zmap }
  = {-# SCC complete_2 #-}
    do
      cm <- readSTRef zmap
      unless (maybe False (S.member p' . fst) (M.lookup (p, v) cm)) $
        do
          let cm' = MS.alter
                    (Just . maybe (S.singleton p', []) (first' (S.insert p')))
                    (p, v)
                    cm
              in cm' `seq` writeSTRef zmap cm'
          let is = maybe [] snd (M.lookup (p, v) cm)
          let is' = map (completeItem p') is `using` seqList rseq
          is' `seq` modifySTRef' (itemq s) (Q.enqList is')
complete _ s
  = {-# SCC complete_3 #-} return () -- error "Earley.complete"

completeItem
  :: (Show v, Show i, Show p, Show t) => p -> Item v l i t p -> Item v l i t p
completeItem p' i@Item{ stateList = slist, bRight = _:bright' }
  = {- mytrace -} i{ stateList = p' : slist, bRight = bright' }

