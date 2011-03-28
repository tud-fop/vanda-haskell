-- Copyright (c) 2010, Toni Dietze
{-- snippet head --}
--module WTABarHillelTopDown(intersect, intersect') where
module WTABarHillelTopDownBin where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.WSA as WSA
import qualified Data.WTA as WTA
import Data.Maybe (fromMaybe)
import Debug.Trace

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
  if (not $ Set.member y s) then -- trace (show y) $
    Queue xs (y:ys) (Set.insert y s)
  else q

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
data Item p q t w = Item
    { wsaStateFst  :: p
    , wsaStateSnd  :: p
    , wtaTrans     :: WTA.Transition q t w
    , wtaTransSt   :: [q]
    , wtaTransRest :: [q]
    , weight       :: w
    }
    deriving (Eq, Ord)
instance (Show p, Show q, Show t, Show w) => Show (Item p q t w) where
  show = showItem
{-- /snippet Item --}
{-- snippet State --}
-- State wsaState wtaState terminal weight
data State p q t w = State
    { itemq :: Queue (Item p q t w)
    , pmap  :: Map.Map q ([WTA.Transition q t w], Set.Set p)
    , smap  :: Map.Map (p, t) [(p, w)]
    , cmap  :: Map.Map (p, q) (Set.Set p, [Item p q t w])
    , epsilonTest :: t -> Bool
    , _trans :: [WTA.Transition (p,[q],p) (Maybe t) w]
    }
{-- /snippet State --}
{-- snippet intersect --}
{-- snippet head --}
intersect
  :: (Show p, Show q, Show t, Show w, Ord p, Ord q, Ord t, Ord w, Num w)
  => WSA.WSA p t w -> WTA.WTA q t w -> WTA.WTA (p, [q], p) (Maybe t) w
intersect wsa wta = intersect' (const False) wsa wta

intersect'
  :: (Show p, Show q, Show t, Show w, Ord p, Ord q, Ord t, Ord w, Num w)
  => (t -> Bool)
  -> WSA.WSA p t w
  -> WTA.WTA q t w
  -> WTA.WTA (p, [q], p) (Maybe t) w
{-- /snippet head --}
intersect' epsilonTest wsa wta
  = let finals  = [ ((ssi, [ts], sso), w1 * w2 * w3)
                  | (ssi, w1) <- WSA.initialWeights wsa
                  , (ts , w2) <- WTA.finalWeights wta
                  , (sso, w3) <- WSA.finalWeights wsa ]
        trans   = _trans $ iter $ initState epsilonTest wsa wta
    in WTA.create trans finals
{-- /snippet intersect --}
{-- snippet iter --}
iter
  :: (Show p, Show q, Show t, Show w, Ord p, Ord q, Ord t, Ord w, Num w)
  => State p q t w -> State p q t w
iter s
  = if isEmpty (itemq s)
    then s
    else
      let (i, itemq') = deq (itemq s)
      in iter $ complete i $ predict i s{itemq = itemq'}
{-- /snippet iter --}

----- Initialization -----------------------------------------------------------
{-- snippet init --}
initPredictMap ::
  (Ord q) =>
  [WTA.Transition q t w]
  -> Map.Map q ([WTA.Transition q t w], Set.Set p)
initPredictMap ts
  = Map.map (\x -> (x, Set.empty)) $
    Map.fromListWith (++) [ (WTA.transState t, [t]) | t <- ts ]


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


initState ::
  (Show p, Show q, Show t, Show w, Ord p, Ord q, Ord t, Ord w, Num w) =>
  (t -> Bool) -> WSA.WSA p t w -> WTA.WTA q t w -> State p q t w
initState epsTest wsa wta
  = let state = State { itemq = emptyq
                      , pmap  = initPredictMap (WTA.transitions wta)
                      , smap  = initScanMap (WSA.transitions wsa)
                      , cmap  = Map.empty
                      , epsilonTest = epsTest
                      , _trans = []
                      }
    in foldr predict' state
          [ (p, q)
          | (p, _) <- WSA.initialWeights wsa
          , (q, _) <- WTA.finalWeights wta
          ]
{-- /snippet init --}

----- Predictor and Scanner ----------------------------------------------------
{-- snippet predict --}
predict ::
  (Show p, Show q, Show t, Show w, Ord p, Ord q, Ord t, Ord w, Num w) =>
  Item p q t w -> State p q t w -> State p q t w
predict
    Item { wsaStateSnd = p
         , wtaTransRest = q:_
         }
  = predict' (p, q)
predict _
  = id
{-- /snippet predict --}
{-- snippet predict_ --}
predict' ::
  (Show p, Show q, Show t, Show w, Ord p, Ord q, Ord t, Ord w, Num w) =>
  (p, q) -> State p q t w -> State p q t w
predict' (p, q) s
  = case Map.lookup q (pmap s) of
      Nothing -> s
      Just (ts, ps) ->
        if Set.member p ps
        then s
        else
          let newI t = Item p p t [] (WTA.transStates t) (WTA.transWeight t)
              pmap'  = Map.insert q (ts, Set.insert p ps) (pmap s)
          in foldr (scan . newI) (s { pmap = pmap' }) ts
{-- /snippet predict_ --}
{-- snippet scan --}
-- FIXME: if epsilonTest returns true, we must also add a transition!!
scan ::
  (Show p, Show q, Show t, Show w, Ord p, Ord q, Ord t, Ord w, Num w) =>
  Item p q t w -> State p q t w -> State p q t w
scan i@Item{wtaTransRest = []} s
  = let p = wsaStateFst i
        t = WTA.transTerminal (wtaTrans i)
        scanI (p', w) = let i' = i
                                 { wsaStateSnd = p'
                                 , weight = w * weight i
                                 }
                            tr = WTA.Transition
                              (Just $ WTA.transTerminal $ wtaTrans i)
                              (p,[WTA.transState $ wtaTrans i], p')
                              []
                              w
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
  :: (Show p, Show q, Show t, Show w, Ord p, Ord q, Ord t, Ord w, Num w)
  => Item p q t w -> State p q t w -> State p q t w
complete
    i@Item { wsaStateSnd = p
           , wtaTransRest = q:qs'
           }
    s
  = let ps'     = maybe [] (Set.toList . fst) (Map.lookup (p, q) (cmap s))
        ps''    = map (flip completeItem i) ps'
        itemq'  = enqListWith fst ps'' (itemq s)
        cmap'   = Map.alter
                    (Just . maybe (Set.empty, [i]) (liftSnd (i:)))
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
        q       = WTA.transState (wtaTrans i)
        cmap'   = Map.alter
                    (Just . maybe (Set.singleton p', [])
                                  (liftFst (Set.insert p')))
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
  -> Item p q t w
  -> (Item p q t w, [WTA.Transition (p,[q],p) (Maybe t) w])
completeItem p' i
  = let i' = i { wsaStateSnd = p'
               , wtaTransSt = head (wtaTransRest i):wtaTransSt i
               , wtaTransRest = tail (wtaTransRest i)
               }
        t1 = if null $ wtaTransSt i then [] else
          [ WTA.Transition
            Nothing
            (wsaStateFst i,wtaTransSt i', p')
            [ (wsaStateFst i,wtaTransSt i, wsaStateSnd i),
              (wsaStateSnd i,[head (wtaTransRest i)], p')]
            1
          ]
        t2 = if not . null $ wtaTransRest i' then []
          else [ WTA.Transition
                 (Just $ WTA.transTerminal $ wtaTrans i)
                 (wsaStateFst i,[WTA.transState $ wtaTrans i], p')
                 [(wsaStateFst i,wtaTransSt i', p')]
                 (WTA.transWeight $ wtaTrans i)
               ]
    in (i', t1++t2)

{-- /snippet complete --}
----- Helpers ------------------------------------------------------------------
{-- snippet complete --}
liftFst :: (a -> c) -> (a, b) -> (c, b)
liftFst f (x, y) = (f x, y)

liftSnd :: (b -> c) -> (a, b) -> (a, c)
liftSnd f (x, y) = (x, f y)
{-- /snippet complete --}

----- Debugging ----------------------------------------------------------------

showItem ::
  (Show p, Show q, Show t, Show w) => Item p q t w -> [Char]
showItem i
  = let t   = wtaTrans i
        qs' = init (show (wtaTransSt i)) ++ "*" ++ tail (show (wtaTransRest i))
    in     "[("
        ++ qs'
        ++ ", "
        ++ show (WTA.transTerminal t)
        ++ ", "
        ++ show (WTA.transState t)
        ++ ")"
        ++ ", "
        ++ show (weight i)
        ++ ", "
        ++ show (wsaStateFst i)
        ++ ", "
        ++ show (wsaStateSnd i)
        ++ "]"

showItemLaTeX :: (Show w) => Item Char Char Char w -> [Char]
showItemLaTeX i
  = let t   = wtaTrans i
        qs  = WTA.transStates t
        l   = length qs - length (wtaTransRest i)
        qs' = take l qs ++ "{\\bullet}" ++ drop l qs
        cts = (:[])
        term 'a' = "\\alpha"
        term 'b' = "\\beta"
    in     "[("
        ++ qs'
        ++ ", "
        ++ term (WTA.transTerminal t)
        ++ ", "
        ++ cts (WTA.transState t)
        ++ ")"
        ++ ", "
        ++ show (weight i)
        ++ ", "
        ++ show (wsaStateSnd i)
        ++ "]"
--
--intersectionIntemCount wsa wta
--  = length $ iter Just (initState (const False) wsa wta)
