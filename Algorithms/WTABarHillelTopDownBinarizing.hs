-- (c) 2011 Matthias Büchse <Matthias.Buechse@tu-dresden.de>
-- (c) 2011 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

{-- snippet head --}
--module WTABarHillelTopDown(intersect, intersect') where
module Algorithms.WTABarHillelTopDownBinarizing where

import Data.Hypergraph
import qualified Data.WSA as WSA
import qualified Data.WTA as WTA
import Tools.Miscellaneous (mapFst, mapSnd)

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

-- import Debug.Trace

{-- /snippet head --}
----- Queue --------------------------------------------------------------------
{-- snippet queue --}
data Queue a = Queue [a] [a] (Set.Set a) deriving (Show)

emptyq :: Queue a
emptyq = Queue [] [] Set.empty

isEmpty :: Queue a -> Bool
isEmpty (Queue [] [] _) = True
isEmpty _               = False

enq :: (Show a, Ord a) => a -> Queue a -> Queue a
enq y q@(Queue xs ys s) =
  if Set.member y s
  then q
  else Queue xs (y:ys) (Set.insert y s)


deq :: Queue a -> (a, Queue a)
deq (Queue (x:xs) ys       s) = (x, Queue xs ys s)
deq (Queue []     ys@(_:_) s) = deq (Queue (reverse ys) [] s)
deq (Queue []     []       _) = error "Cannot dequeue from empty queue."

enqList :: (Show a, Ord a) => [a] -> Queue a -> Queue a
enqList xs q = foldr enq q xs

enqListWith :: (Show a, Ord a) => (b -> a) -> [b] -> Queue a -> Queue a
enqListWith f xs q = foldr (enq . f) q xs

toList :: Queue a -> [a]
toList (Queue xs ys _) = xs ++ (reverse ys)
{-- /snippet queue --}

----- Main ---------------------------------------------------------------------
{-- snippet Item --}
-- Item wsaState wtaState terminal weight
data Item p q t w i = Item
    { wsaStateFst  :: p
    , wsaStateSnd  :: p
    , wtaTrans     :: Hyperedge q t w i
    , bullet       :: Int -- redundant: bullet == length wtaTransSt
    , wtaTransSt   :: [q]
    , wtaTransRest :: [q]
    , weight       :: w
    }
    -- deriving (Eq, Ord)


{--instance (Eq p, Eq i) => Eq (Item p q t w i) where
  i1 == i2 =    wsaStateFst i1 == wsaStateFst i2
             && wsaStateSnd i1 == wsaStateSnd i2
             && eId (wtaTrans i1) == eId (wtaTrans i2)
             && bullet i1 == bullet i2--}

{--instance (Eq p, Eq i) => Eq (Item p q t w i) where
  i1 == i2 = (==)
    (wsaStateFst i1, wsaStateSnd i1, eId (wtaTrans i1), bullet i1)
    (wsaStateFst i2, wsaStateSnd i2, eId (wtaTrans i2), bullet i2) --}
instance (Eq p, Eq i) => Eq (Item p q t w i) where
  Item sf1 ss1 t1 b1 _ _ _ == Item sf2 ss2 t2 b2 _ _ _ =
    (sf1, ss1, eId t1, b1) == (sf2, ss2, eId t2, b2)
    -- (wsaStateFst i1, wsaStateSnd i1, eId (wtaTrans i1), bullet i1)
    -- (wsaStateFst i2, wsaStateSnd i2, eId (wtaTrans i2), bullet i2)
{--
instance (Ord p, Ord i) => Ord (Item p q t w i) where
  i1 `compare` i2 = compare
    (wsaStateFst i1, wsaStateSnd i1, eId (wtaTrans i1), bullet i1)
    (wsaStateFst i2, wsaStateSnd i2, eId (wtaTrans i2), bullet i2) --}
    {--   wsaStateFst i1 <= wsaStateFst i2
    && wsaStateSnd i1 <= wsaStateSnd i2
    && eId (wtaTrans i1) <= eId (wtaTrans i2)
    && bullet i1 <= bullet i2--}
instance (Ord p, Ord i) => Ord (Item p q t w i) where
  Item sf1 ss1 t1 b1 _ _ _ `compare` Item sf2 ss2 t2 b2 _ _ _ =
    (sf1, ss1, eId t1, b1) `compare` (sf2, ss2, eId t2, b2)

instance (Show p, Show q, Show t, Show w) => Show (Item p q t w i) where
  show = showItem

{-- /snippet Item --}
{-- snippet State --}
-- State wsaState wtaState terminal weight
data State p q t w i = State
    { itemq :: Queue (Item p q t w i)
    , pmap  :: Map.Map q ([Hyperedge q t w i], Set.Set p)
    , smap  :: Map.Map (p, t) [(p, w)]
    , cmap  :: Map.Map (p, q) (Set.Set p, [Item p q t w i])
    , epsilonTest :: t -> Bool
    , _trans :: [Hyperedge (p,[q],p) (Maybe t) w i]
    }
{-- /snippet State --}
{-- snippet intersect --}
{-- snippet head --}
intersect
  :: (Show p, Show q, Show t, Show w,
      Ord p, Ord i, Ord q, Ord t, Ord w, Num w)
  => WSA.WSA p t w -> WTA.WTA q t w i -> WTA.WTA (p, [q], p) (Maybe t) w i
intersect wsa wta = intersect' (const False) wsa wta

intersect'
  :: (Show p, Show q, Show t, Show w,
      Ord p, Ord i, Ord q, Ord t, Ord w, Num w)
  => (t -> Bool)
  -> WSA.WSA p t w
  -> WTA.WTA q t w i
  -> WTA.WTA (p, [q], p) (Maybe t) w i
{-- /snippet head --}
intersect' epsTest wsa wta
  = let finals  = [ ((ssi, [ts], sso), w1 * w2 * w3)
                  | (ssi, w1) <- WSA.initialWeights wsa
                  , (ts , w2) <- Map.toList $ WTA.finalWeights wta
                  , (sso, w3) <- WSA.finalWeights wsa ]
        trans   = _trans $ iter $ initState epsTest wsa wta
    in WTA.wtaCreate finals trans
{-- /snippet intersect --}
{-- snippet iter --}
iter
  :: (Show p, Show q, Show t, Show w,
      Ord p, Ord i, Ord q, Ord t, Ord w, Num w)
  => State p q t w i -> State p q t w i
iter s
  = if isEmpty (itemq s)
    then s
    else
      let (i, itemq') = deq (itemq s)
      in iter $ complete i $ predict i s{itemq = itemq'}
{-- /snippet iter --}

----- Initialization -----------------------------------------------------------
{-- snippet init --}
initScanMap ::
  (Ord p, Ord t) =>
  [WSA.Transition p t w] -> Map.Map (p, t) [(p, w)]
initScanMap ts
  = Map.fromListWith (++)
      [ (  (WSA.transStateIn  t, WSA.transTerminal t)
        , [(WSA.transStateOut t, WSA.transWeight   t)]
        )
      | t <- ts
      ]


initState
  :: (Show p, Show q, Show t, Show w,
      Ord p, Ord i, Ord q, Ord t, Ord w, Num w)
  => (t -> Bool) -> WSA.WSA p t w -> WTA.WTA q t w i -> State p q t w i
initState epsTest wsa wta
  = let state = State { itemq = emptyq
                      , pmap  = Map.map (\x -> (x, Set.empty))
                              $ edgesM
                              $ WTA.toHypergraph wta
                      , smap  = initScanMap (WSA.transitions wsa)
                      , cmap  = Map.empty
                      , epsilonTest = epsTest
                      , _trans = []
                      }
    in foldr predict' state
          [ (p, q)
          | (p, _) <- WSA.initialWeights wsa
          , q <- Map.keys $ WTA.finalWeights wta
          ]
{-- /snippet init --}

----- Predictor and Scanner ----------------------------------------------------
{-- snippet predict --}
predict
  :: (Show p, Show q, Show t, Show w,
      Ord p, Ord i, Ord q, Ord t, Ord w, Num w)
  => Item p q t w i -> State p q t w i -> State p q t w i
predict
    Item { wsaStateSnd = p
         , wtaTransRest = q:_
         }
  = predict' (p, q)
predict _
  = id
{-- /snippet predict --}
{-- snippet predict_ --}
predict'
  :: (Show p, Show q, Show t, Show w,
      Ord p, Ord i, Ord q, Ord t, Ord w, Num w)
  => (p, q) -> State p q t w i -> State p q t w i
predict' (p, q) s
  = case Map.lookup q (pmap s) of
      Nothing -> s
      Just (ts, ps) ->
        if Set.member p ps
        then s
        else
          let newI t = Item p p t 0 [] (eTail t) (eWeight t)
              pmap'  = Map.insert q (ts, Set.insert p ps) (pmap s)
          in foldr (scan . newI) (s { pmap = pmap' }) ts
{-- /snippet predict_ --}
{-- snippet scan --}
-- FIXME: if epsilonTest returns true, we must also add a transition!!
scan
  :: (Show p, Show q, Show t, Show w,
      Ord p, Ord i, Ord q, Ord t, Ord w, Num w)
  => Item p q t w i -> State p q t w i -> State p q t w i
scan i@Item{wtaTransRest = []} s
  = let p = wsaStateFst i
        t = eLabel (wtaTrans i)
        scanI (p', w) = let i' = i
                                 { wsaStateSnd = p'
                                 , weight = w * weight i
                                 }
                            tr = hyperedge
                              (p,[eHead $ wtaTrans i], p')
                              []
                              (Just $ eLabel $ wtaTrans i)
                              (weight i')
                              (eId $ wtaTrans i)
                        in (i', tr)
        is = map scanI (fromMaybe [] (Map.lookup (p, t) (smap s)))
        update = enqListWith fst is
        trans' = (map snd is) ++ (_trans s)
    in if (epsilonTest s) t
    then s { itemq = enq (fst $ scanI (p, 1)) (itemq s) }
    else s { itemq = update (itemq s), _trans = trans' }
scan i s
  = s { itemq = enq i (itemq s) }
{-- /snippet scan --}

----- Completer ----------------------------------------------------------------
{-- snippet complete --}
complete
  :: (Show p, Show q, Show t, Show w,
      Ord p, Ord i, Ord q, Ord t, Ord w, Num w)
  => Item p q t w i -> State p q t w i -> State p q t w i
complete
    i@Item { wsaStateSnd = p
           , wtaTransRest = q:_
           }
    s
  = let ps'     = maybe [] (Set.toList . fst) (Map.lookup (p, q) (cmap s))
        ps''    = map (flip completeItem i) ps'
        itemq'  = enqListWith fst ps'' (itemq s)
        cmap'   = Map.alter
                    (Just . maybe (Set.empty, [i]) (mapSnd (i:)))
                    (p, q)
                    (cmap s)
        trans'  = (concat $ map snd ps'') ++ _trans s
    in s { itemq = itemq', cmap = cmap', _trans = trans' }
complete
    i@Item { wsaStateSnd = p'
           , wtaTransRest = []
           }
    s
  = let p       = wsaStateFst i
        q       = eHead (wtaTrans i)
        cmap'   = Map.alter
                    (Just . maybe (Set.singleton p', [])
                                  (mapFst (Set.insert p')))
                    (p, q)
                    (cmap s)
        is      = maybe [] snd (Map.lookup (p, q) (cmap s))
        is'     = map (completeItem p') is
        itemq'  = enqListWith fst is' (itemq s)
        trans'  = (concat $ map snd is') ++ _trans s
    in if maybe False (Set.member p' . fst) (Map.lookup (p, q) (cmap s))
       then s
       else s { itemq = itemq', cmap = cmap', _trans = trans' }


completeItem
  :: Num w
  => p
  -> Item p q t w i
  -> (Item p q t w i, [Hyperedge (p,[q],p) (Maybe t) w i])
completeItem p' i
  = let i' = i { wsaStateSnd = p'
               , bullet = (bullet i)+1
               , wtaTransSt = head (wtaTransRest i):wtaTransSt i
               , wtaTransRest = tail (wtaTransRest i)
               }
        t1 = if null $ wtaTransSt i then [] else
          [ hyperedge
            (wsaStateFst i,wtaTransSt i', p')
            [ (wsaStateFst i,wtaTransSt i, wsaStateSnd i),
              (wsaStateSnd i,[head (wtaTransRest i)], p')]
            Nothing
            1
            (eId $ wtaTrans i)
          ]
        t2 = if not . null $ wtaTransRest i' then []
          else [ hyperedge
                 (wsaStateFst i,[eHead $ wtaTrans i], p')
                 [(wsaStateFst i,wtaTransSt i', p')]
                 (Just $ eLabel $ wtaTrans i)
                 (eWeight $ wtaTrans i)
                 (eId $ wtaTrans i)
               ]
    in (i', t1++t2)

{-- /snippet complete --}
----- Debugging ----------------------------------------------------------------

showItem ::
  (Show p, Show q, Show t, Show w) => Item p q t w i -> [Char]
showItem i
  = let t   = wtaTrans i
        qs' = init (show (wtaTransSt i)) ++ "*" ++ tail (show (wtaTransRest i))
    in     "[("
        ++ qs'
        ++ ", "
        ++ show (eLabel t)
        ++ ", "
        ++ show (eHead t)
        ++ ")"
        ++ ", "
        ++ show (weight i)
        ++ ", "
        ++ show (wsaStateFst i)
        ++ ", "
        ++ show (wsaStateSnd i)
        ++ "]"

showItemLaTeX :: (Show w) => Item Char Char Char w i -> [Char]
showItemLaTeX i
  = let t   = wtaTrans i
        qs  = eTail t
        l   = length qs - length (wtaTransRest i)
        qs' = take l qs ++ "{\\bullet}" ++ drop l qs
        cts = (:[])
        term 'a' = "\\alpha"
        term 'b' = "\\beta"
        term _   = error "WTABarHillelTopDownBin.showItemLaTeX"
    in     "[("
        ++ qs'
        ++ ", "
        ++ term (eLabel t)
        ++ ", "
        ++ cts (eHead t)
        ++ ")"
        ++ ", "
        ++ show (weight i)
        ++ ", "
        ++ show (wsaStateSnd i)
        ++ "]"


intersectionItemCount
  :: (Show p, Show q, Show t, Num w, Ord p, Ord i, Ord q, Ord t, Ord w)
  => WSA.WSA p t w -> WTA.WTA q t w i -> Int
intersectionItemCount wsa wta
 = (\ (Queue _ _ s) -> Set.size s) $ itemq $ iter $ initState (const False) wsa wta
