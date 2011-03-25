-- Copyright (c) 2010, Toni Dietze
{-- snippet head --}
--module WTABarHillelTopDown(intersect, intersect') where
module WTABarHillelTopDown where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.WSA as WSA
import qualified Data.WTA as WTA

{-- /snippet head --}
----- Queue --------------------------------------------------------------------
{-- snippet queue --}
data Queue a = Queue [a] [a] deriving (Show)

emptyq :: Queue a
emptyq = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

enq :: a -> Queue a -> Queue a
enq y (Queue xs ys) = Queue xs (y:ys)

deq :: Queue a -> (a, Queue a)
deq (Queue (x:xs) ys      ) = (x, Queue xs ys)
deq (Queue []     ys@(_:_)) = deq (Queue (reverse ys) [])
deq (Queue []     []      ) = error "Cannot dequeue from empty queue."

enqList :: [a] -> Queue a -> Queue a
enqList xs q = foldr enq q xs

enqListWith :: (b -> a) -> [b] -> Queue a -> Queue a
enqListWith f xs q = foldr (enq . f) q xs

toList :: Queue a -> [a]
toList (Queue xs ys) = xs ++ (reverse ys)
{-- /snippet queue --}

----- Main ---------------------------------------------------------------------
{-- snippet Item --}
-- Item wsaState wtaState terminal weight
data Item p q t w = Item
    { wsaStatesRev :: [p]
    , wsaStatesFst :: p
    , wtaTrans     :: WTA.Transition q t w
    , wtaTransRest :: [q]
    , weight       :: w
    }
{-- /snippet Item --}
{-- snippet State --}
-- State wsaState wtaState terminal weight
data State p q t w = State
    { itemq :: Queue (Item p q t w)
    , pmap  :: Map.Map q ([WTA.Transition q t w], Set.Set p)
    , smap  :: Map.Map (p, t) [(p, w)]
    , cmap  :: Map.Map (p, q) (Set.Set p, [Item p q t w])
    , epsilonTest :: t -> Bool
    }
{-- /snippet State --}
{-- snippet intersect --}
{-- snippet head --}
intersect ::
  (Ord p, Ord q, Ord t, Num w) =>
  WSA.WSA p t w -> WTA.WTA q t w -> WTA.WTA (p, q, p) t w
intersect wsa wta = intersect' (const False) wsa wta

intersect' ::
  (Ord p, Ord q, Ord t, Num w) =>
  (t -> Bool)
  -> WSA.WSA p t w
  -> WTA.WTA q t w
  -> WTA.WTA (p, q, p) t w
{-- /snippet head --}
intersect' epsilonTest wsa wta
  = let finals  = [ ((ssi, ts, sso), w1 * w2 * w3)
                  | (ssi, w1) <- WSA.initialWeights wsa
                  , (ts , w2) <- WTA.finalWeights wta
                  , (sso, w3) <- WSA.finalWeights wsa ]
        trans   = iter extractTransition (initState epsilonTest wsa wta)
    in WTA.create trans finals
{-- /snippet intersect --}
{-- snippet iter --}
iter ::
  (Ord p, Ord q, Ord t, Num w) =>
  (Item p q t w -> Maybe a) -> State p q t w -> [a]
iter extract s
  = if isEmpty (itemq s)
    then []
    else
      let (i, itemq') = deq (itemq s)
      in maybe id (:) (extract i) $
        iter extract $ complete i $ predict i s{itemq = itemq'}
{-- /snippet iter --}
{-- snippet extractTransition --}
extractTransition ::
  Item p q t w -> Maybe (WTA.Transition (p, q, p) t w)
extractTransition
    Item { wtaTransRest = []
         , wsaStatesRev = psRev
         , wtaTrans = trans
         , weight = w
         }
  = let terminal = WTA.transTerminal trans
        ps       = reverse psRev
        state    = (head ps, WTA.transState trans, head psRev)
        states   = zip3 ps (WTA.transStates trans) (tail ps)
    in Just (WTA.Transition terminal state states w)
extractTransition _
  = Nothing
{-- /snippet extractTransition --}

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
  (Ord p, Ord q, Ord t, Num w) =>
  (t -> Bool) -> WSA.WSA p t w -> WTA.WTA q t w -> State p q t w
initState epsTest wsa wta
  = let state = State { itemq = emptyq
                      , pmap  = initPredictMap (WTA.transitions wta)
                      , smap  = initScanMap (WSA.transitions wsa)
                      , cmap  = Map.empty
                      , epsilonTest = epsTest
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
  (Ord p, Ord q, Ord t, Num w) =>
  Item p q t w -> State p q t w -> State p q t w
predict
    Item { wsaStatesRev = p:_
         , wtaTransRest = q:_
         }
  = predict' (p, q)
predict _
  = id
{-- /snippet predict --}
{-- snippet predict_ --}
predict' ::
  (Ord p, Ord q, Ord t, Num w) =>
  (p, q) -> State p q t w -> State p q t w
predict' (p, q) s
  = case Map.lookup q (pmap s) of
      Nothing -> s
      Just (ts, ps) ->
        if Set.member p ps
        then s
        else
          let newI t = Item [p] p t (WTA.transStates t) (WTA.transWeight t)
              pmap'  = Map.insert q (ts, Set.insert p ps) (pmap s)
          in foldr (scan . newI) (s { pmap = pmap' }) ts
{-- /snippet predict_ --}
{-- snippet scan --}
scan ::
  (Ord p, Ord t, Num w) =>
  Item p q t w -> State p q t w -> State p q t w
scan i@Item{wtaTransRest = []} s
  = let p = wsaStatesFst i
        t = WTA.transTerminal (wtaTrans i)
        scanI (p', w) = i { wsaStatesRev = p':(wsaStatesRev i)
                          , weight = w * weight i
                          }
        update = maybe id (enqListWith scanI) (Map.lookup (p, t) (smap s))
    in if (epsilonTest s) t
    then s { itemq = enq (scanI (p, 1)) (itemq s) }
    else s { itemq = update (itemq s) }
scan i s
  = s { itemq = enq i (itemq s) }
{-- /snippet scan --}

----- Completer ----------------------------------------------------------------
{-- snippet complete --}
complete ::
  (Ord p, Ord q) => Item p q t w -> State p q t w -> State p q t w
complete
    i@Item { wsaStatesRev = ps@(p:_)
           , wtaTransRest = q:qs'
           }
    s
  = let ps'     = maybe [] (Set.toList . fst) (Map.lookup (p, q) (cmap s))
        itemq'  = enqListWith (flip completeItem i) ps' (itemq s)
        cmap'   = Map.alter
                    (Just . maybe (Set.empty, [i]) (liftSnd (i:)))
                    (p, q)
                    (cmap s)
    in s { itemq = itemq', cmap = cmap' }
complete
    i@Item { wsaStatesRev = p':_
           , wtaTransRest = []
           }
    s
  = let p       = wsaStatesFst i
        q       = WTA.transState (wtaTrans i)
        cmap'   = Map.alter
                    (Just . maybe (Set.singleton p', [])
                                  (liftFst (Set.insert p')))
                    (p, q)
                    (cmap s)
        is      = maybe [] snd (Map.lookup (p, q) (cmap s))
        itemq'  = enqListWith (completeItem p') is (itemq s)
    in if maybe False (Set.member p' . fst) (Map.lookup (p, q) (cmap s))
       then s
       else s { itemq = itemq', cmap = cmap' }


completeItem :: p -> Item p q t w -> Item p q t w
completeItem p' i
  = i { wsaStatesRev = p':(wsaStatesRev i)
      , wtaTransRest = tail (wtaTransRest i)
      }

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
        qs  = WTA.transStates t
        l   = length qs - length (wtaTransRest i)
        qs' = init (show (take l qs)) ++ "*" ++ tail (show (drop l qs))
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
        ++ show (reverse $ wsaStatesRev i)
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
        ++ reverse (wsaStatesRev i)
        ++ "]"

getIntersectItems ::
  (Ord p, Show p, Ord q, Show q, Ord t, Show t, Num w) =>
  (t -> Bool) -> WSA.WSA p t w -> WTA.WTA q t w -> [[Char]]
getIntersectItems epsilonTest wsa wta
  = iter (Just . showItem) (initState epsilonTest wsa wta)

getIntersectItemsLaTeX ::
  (Num w) =>
  (Char -> Bool)
  -> WSA.WSA Char Char w
  -> WTA.WTA Char Char w
  -> [[Char]]
getIntersectItemsLaTeX epsilonTest wsa wta
  = zipWith (++)
        (map (\x -> "\\\\\n&  i_{" ++ show x ++ "} = ") [1 ..])
        (iter (Just . showItemLaTeX) (initState epsilonTest wsa wta))


intersectionIntemCount wsa wta
  = length $ iter Just (initState (const False) wsa wta)
