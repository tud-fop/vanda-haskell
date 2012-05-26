-- (c) 2010 Linda Leuschner <Leuschner.Linda@mailbox.tu-dresden.de>
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
-- Maintainer  :  Linda Leuschner
-- Stability   :  unbekannt
-- Portability :  portable
--
-- This module computes 'Hypergraph' out of a 'Hypergraph' and a 'WSA'. 
-- The resulting 'Hypergraph' will only recognize the given word.
-- This implementation uses the Early and the Bar-Hille algorithm.

-- The input 'Hypergraph' represents a synchronous contet-free grammar.
-- Variables in a production should start with 0. 
-- The list of nonterminals belonging to the variables is ordered by the index 
-- of the variables.

-- Left : nonterminals
-- Right: terminals

module Vanda.Algorithms.EarleyCFG where

-- import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Data.Either
import Data.List as L
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Queue as Q
import qualified Data.Vector.Unboxed as V
import Debug.Trace

import Vanda.Hypergraph
import Vanda.Token
import qualified Vanda.Algorithms.Earley.WSA as WSA

{-instance (Show v, Show l, Show i) => Show (Hyperedge v l i) where
  show e
    = show (to e)
      ++ " -> "
      ++ show (from e)
      ++ "\n<"
      ++ show (label e)
      ++ ">\n "
      -- ++ unwords (Prelude.map show $ from e)
      ++ " # "
      ++ show (ident e)
      ++ "\n\n"-}

instance (Show v, Show i, Show p, Show t) => Show (Item v i t p) where
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

{-instance (NFData v, NFData t) => NFData (Item v t) where
  rnf (Item iHead bRight iEdge stateList nextSymb lastState)
    =       rnf iHead
      `seq` rnf stateList
      `seq` rnf nextSymb
      `seq` rnf lastState

instance (NFData v, NFData l)
  => NFData (BackwardStar (Int,v,Int) l (Int, Int)) where
    rnf (BackwardStar nodes f memo) = rnf nodes `seq` rnf memo 
    
instance (NFData k, NFData v) => NFData (M.Map k v) where
  rnf m = rnf (M.toList m)
-}

-- Item: [iHead -> ??? * bRight] 
-- statelist contains states of a wsa. 
-- Ignoring the bullet, the first symbol of the righthandside of an item
-- (not bRight) is wrapped by the first and the second state, the second symbol 
-- is wrapped by the second and the third state etc.
data Item v i t p
  = Item 
    { iHead :: v
    , bRight :: [Either v t]
    , iEdge :: i
    , stateList :: [p]
    , firstState :: p
    , weight :: Double
    }

instance (Eq i, Eq p) => Eq (Item v i t p) where
  i1 == i2 = (iEdge i1, stateList i1) == (iEdge i2, stateList i2)

instance (Ord i, Ord p) => Ord (Item v i t p) where
  i1 `compare` i2
    = (iEdge i1, stateList i1) `compare` (iEdge i2, stateList i2)

data State v l i t p = State
    { back :: v -> [Hyperedge v l i]
    , byId :: i -> Hyperedge v l i
    , comp :: Hyperedge v l i -> [Either Int t]
    , v0 :: v
    , itemq :: Q.Queue (Item v i t p)
    , pset :: S.Set (p, v)   -- ^ remember what has already been predicted
    , smap :: M.Map (p, t) [(p, Double)]
      -- ^ Another representation of 'WSA' transitions. An incoming state
      -- and a terminal are mapped to a list of outcoming states and weights.
    , cmap :: M.Map (p, v) (S.Set p, [Item v i t p])
    }


-- The main procedure. Is it favourable to represent the weights as a vector 
-- instead of a map? The ids of the hyperedges do not start with 0.. (rename?)

{-earley'
  :: (Ord p, Ord v, Ord i, Ord t)
  => BackwardStar v l i
  -> (l -> [Either Int t])
  -> WSA.WSA p t Double
  -> (EdgeList (p, v, p) l (i, Int), V.Vector Double)
earley' bs component wsa = earley bs component wsa (S.findMin $ nodes bs) -}

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
    trans = iter extract (initState bs component wsa v0) []
    theList = S.toList $ S.fromList $ snd $ unzip trans
    theMap = M.fromList (zip theList [0..])
    doIt (he, w) = mapHEi (\ i -> (i, theMap M.! w)) he
    -- doIt (he, w) = mapHEi (\ i -> (i, 0)) he


initState
  :: (Ord p, Ord v, Ord t, Ord i)
  => BackwardStar v l i
  -> (l -> [Either Int t])
  -> WSA.WSA p t Double
  -> v            -- an initial node of the Hypergraph
  -> State v l i t p
initState hg component wsa v00
  = foldr predict' state [ (p, v00) | (p, _) <- WSA.initialWeights wsa ]
  where
    state
      = State
        { back = backStar hg
        , byId = ((M.fromList idList) M.!)
        , comp = component . label
        , v0 = v00
        , itemq = Q.empty
        , pset = S.empty
        , smap = initScanMap (WSA.transitions wsa)
        , cmap = M.empty
        }
    initScanMap ts
      = M.fromListWith (++)
          [ ( (WSA.transStateIn t, WSA.transTerminal t)
            , [(WSA.transStateOut t, WSA.transWeight t)]
            )
          | t <- ts
          ]
    idList
      = [ (ident e, e)
        | v <- S.toList (nodes hg)
        , e <- backStar hg v
        ]


extract
  :: (Show v, Show p, Show i, Show t)
  => Item v i t p
  -> State v l i t p
  -> Maybe (Hyperedge (p, v, p) l i, ([p], Double))
extract item@Item{bRight = []} state
  = Just
  $ ( mkHyperedge 
        (firstState item, iHead item, head $ statl)
        (carryL (yesyes (comp state edge) (reverse statl)) (from edge))
        (label edge)
        ide
    , ( statl
      , weight item -- * L.product initial_weight
      )
    )
  where
    statl = stateList item
    ide = iEdge item
    edge = byId state ide
    carryL yesbaby vert
      = [ (l, v, r) | (i, v) <- zip [0..] vert
        , let (l, _, r) = yesbaby M.! i
        ]
    yesyes lab wsast = M.fromList [ (i, x) | x@(_, Left i, _) <- zip3 wsast lab (tail wsast) ]
    {-
    initial_weight 
      = if (iHead item == iniNode) -- START-SEPARATION REQUIRED
        then [ w
             | (s, w) <- initialWeights wsa
             , s == firstState item ]
             ++ [ w
                | (s, w) <- finalWeights wsa
                , s == head $ stateList item
                ]
        else []
    -}
extract _ _ = Nothing


iter
  :: (Ord p, Ord v, Ord t, Ord i, Show p, Show v, Show t, Show i)
  => (Item v i t p -> State v l i t p -> Maybe a)
  -> State v l i t p
  -> [a]
  -> [a]
iter extract s r
  = case Q.deqMaybe (itemq s) of
      Nothing -> r
      Just (i, itemq')
        -> s' `seq` r' `seq` iter extract s' r'
        where
          s' = complete i
             $ scan i
             $ predict i
             -- $ traceShow i
             $ s{itemq = itemq'}
          r' = maybe id (:) (extract i s') r
 

predict
  :: (Ord p, Ord v, Ord t, Ord i)
  => Item v i t p -> State v l i t p -> State v l i t p
predict item
  = case bRight item of
      Left v:_ -> predict' (head (stateList item), v)
      _ -> id

predict'
  :: (Ord p, Ord v, Ord t, Ord i)
  => (p, v) -> State v l i t p -> State v l i t p
predict' pv@(p, v) s
  | S.member pv (pset s) = s
  | otherwise = s{ pset = pset', itemq = itemq' }
  where
    pset' = S.insert pv (pset s)
    itemq' = Q.enqList itemlist (itemq s)
    itemlist
      = [ Item v (map conv (comp s edge)) (ident edge) [p] p 1
        | edge <- back s v
        , let conv = (either (Left . (deref edge)) (Right . id))
        ]


scan
  :: (Ord p, Ord v, Ord t, Ord i)
  => Item v i t p -> State v l i t p -> State v l i t p
scan item@Item{ bRight = Right t : bright'
              , stateList = slist
              , weight = weyght }
     s
  = s{ itemq = itemq' }
  where
    itemq' = Q.enqList itemlist (itemq s)
    itemlist
      = [ item
            { bRight = bright'
            , stateList = p:slist
            , weight = w * weyght
            }
        | (p, w) <- M.findWithDefault [] (head $ stateList item, t) (smap s)
        ]
scan _ s = s
        
complete
  :: (Ord p, Ord v, Ord t, Ord i)
  => Item v i t p -> State v l i t p -> State v l i t p
complete i@Item{ stateList = p:_, bRight = Left v:_ } s
  = s { itemq = itemq', cmap = cmap' }
  where
    ps' = maybe [] (S.toList . fst) (M.lookup (p, v) (cmap s))
    itemq' = Q.enqListWith (flip completeItem i) ps' (itemq s)
    cmap'
      = M.alter
          (Just . maybe (S.empty, [i]) (second (i:)))
          (p, v)
          (cmap s)
complete i@Item{ iHead = v, stateList = p':_, firstState = p, bRight = [] } s
  = if maybe False (S.member p' . fst) (M.lookup (p, v) (cmap s))
    then s
    else s{ itemq = itemq', cmap = cmap' }
  where
    cmap'
      = M.alter
          (Just . maybe (S.singleton p', []) (first (S.insert p')))
          (p, v)
          (cmap s)
    is = maybe [] snd (M.lookup (p, v) (cmap s))
    itemq' = Q.enqListWith (completeItem p') is (itemq s)
complete _ s
  = s -- error "Earley.complete"

completeItem :: p -> Item v i t p -> Item v i t p
completeItem p' i@Item{ stateList = slist, bRight = _:bright' }
  = i{ stateList = p' : slist, bRight = bright' }

