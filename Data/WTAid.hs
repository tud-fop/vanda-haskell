-- Copyright (c) 2010-2011, Toni Dietze
{-- snippet types --}
module Data.WTAid(
  Transition(..)
, WTA
, states
, transitions
, finalWeights
, create
, fromHypergraph
, toHypergraph
, transIsLeaf
, binarize
, addId
, properize
, randomizeWeights
, mapStates
, showTransition
, printTransition
, showWTA
, printWTA
, weightTree
, generate
, generate'
) where

import Data.Hypergraph hiding (properize, randomizeWeights)
import Tools.FastNub(nub)
import Tools.Miscellaneous(mapFst, mapRandomR)

import Control.DeepSeq
import qualified Data.List as L
import qualified Data.Map as M
import qualified Random as R
import qualified Data.Tree as T
import Data.Maybe(fromJust)

data Transition q t w i = Transition
    { transTerminal :: t
    , transState    :: q
    , transStates   :: [q]
    , transWeight   :: w
    , transId       :: i
    } deriving (Eq, Ord, Show)

data WTA q t w i = WTA
    { states       :: [q]
    , transitions  :: [Transition q t w i]
    , finalWeights :: [(q, w)]
    } deriving Show

create :: (Ord q) => [Transition q t w i] -> [(q, w)] -> WTA q t w i
create ts fs
  = let ss = nub $
             map fst fs ++
             concatMap (\t -> transState t : transStates t) ts
    in WTA ss ts fs
{-- /snippet types --}


fromHypergraph target g
  = WTA
      (vertices g)
      ( map (\ e -> Transition (eLabel e) (eHead e) (eTail e) (eWeight e) (eId e))
      $ edges g
      )
      [(target, 1)]

toHypergraph wta
  = hypergraph
  . map (\ t -> hyperedge (transState t) (transStates t) (transTerminal t) (transWeight t) (transId t))
  . transitions
  $ wta


transIsLeaf :: Transition q t w i -> Bool
transIsLeaf (Transition {transStates = []}) = True
transIsLeaf _                               = False


binarize :: (Ord q, Num w) => WTA q t w i -> WTA [q] (Maybe t) w ()
binarize wta
  = let states = nub $
                 -- map (:[]) (states wta) ++
                 map ((:[]) . transState) (transitions wta) ++
                 concat
                 [ tailsNonempty w
                 | w <- map transStates (transitions wta)
                 ]
        trans  = [ Transition (Just t) [q] [] w ()
                 | Transition       t   q  [] w _ <- transitions wta
                 ] ++
                 [ Transition (Just t) [q] [qs]       w ()
                 | Transition       t   q   qs@(_:_)  w _ <- transitions wta
                 ] ++
                 [ Transition Nothing qqs [[q], qs] 1 ()
                 | qqs@(q:qs@(_:_)) <- states
                 ]
        finals = map (mapFst (:[])) (finalWeights wta)
    in WTA states trans finals

addId :: WTA q t w i -> WTA q t w Int
addId wta@WTA{ transitions = ts } = wta{ transitions = map f (zip ts [0..]) }
  where f (t,i) = t{ transId = i }

tailsNonempty :: [a] -> [[a]]
tailsNonempty []         =  []
tailsNonempty xxs@(_:xs) =  xxs : tailsNonempty xs


properize :: (Ord q, Fractional w) => WTA q t w i -> WTA q t w i
properize wta@WTA{transitions = ts}
  = let counts =
              M.fromListWith (+)
            . map (\t -> (transState t, transWeight t))
            $ ts
        normalize t =
            t{transWeight =
              transWeight t / fromJust (M.lookup (transState t) counts)
            }
    in wta{transitions = map normalize ts}


-- | @randomizeWeights r wta g@ multiplies every weight of @wta@ by a
-- random number in the range @(1-r, 1+r)@.
randomizeWeights
  :: (Num w, R.Random w, R.RandomGen g)
  => w -> WTA q t w i -> g -> (WTA q t w i, g)
randomizeWeights r wta g = mapWeightsRandomR (1-r, 1+r) (*) wta g


-- | 'mapRandomR' for the weights of a 'WTA'.
mapWeightsRandomR
  :: (R.Random r, R.RandomGen g)
  => (r, r) -> (w -> r -> w') -> WTA q t w i -> g -> (WTA q t w' i, g)
mapWeightsRandomR r f wta g
  = let (ts, g' ) = mapRandomR
                      r
                      (\t r -> t{transWeight = f (transWeight t) r})
                      (transitions wta)
                      g
        (fs, g'') = mapRandomR
                      r
                      (\(q, w) r -> (q, f w r))
                      (finalWeights wta)
                      g'
    in (wta{transitions = ts, finalWeights = fs}, g'')
  where
    h :: (a -> b) -> (b -> c -> (d, e)) -> (a -> d -> f) -> a -> c -> (f, e)
    h unpack f pack x g = let (y, g') = f (unpack x) g in (pack x y, g')


mapStates :: (Ord q) => (p -> q) -> WTA p t w i -> WTA q t w i
mapStates f wta
  = create
    (map
      (\t -> t
        { transState  =     f (transState  t)
        , transStates = map f (transStates t)
        }
      )
      (transitions wta)
    )
    (map (mapFst f) (finalWeights wta))


showTransition
  :: (Show i, Show q, Show t, Show w)
  => Transition q t w i
  -> String
showTransition t
  =   show (transId t)
  ++  ":  "
  ++  show (transState t)
  ++  " -> "
  ++  show (transTerminal t)
  ++  "-"
  ++  show (transStates t)
  ++  " ("
  ++  show (transWeight t)
  ++  ")"


printTransition
  :: (Show i, Show q, Show t, Show w)
  => Transition q t w i
  -> IO ()
printTransition = putStrLn . showTransition


showWTA :: (Show i, Show q, Show t, Show w) => WTA q t w i -> String
showWTA wta
  =   "Transitions:\n"
  ++  (unlines . map showTransition . transitions $ wta)
  ++  "\nStates:\n"
  ++  (unlines . map show . states $ wta)
  ++  "\nFinal Weights:\n"
  ++  (unlines . map show . finalWeights $ wta)


printWTA :: (Show i, Show q, Show t, Show w) => WTA q t w i -> IO ()
printWTA wta = putStr . showWTA $ wta


weightTree :: (Eq q, Eq t, Num w) => WTA q t w i -> T.Tree t -> w
weightTree wta tree
  = sum
  . map (\(q, w) -> weightTree' wta q tree * w)
  $ finalWeights wta


weightTree' :: (Eq q, Eq t, Num w) => WTA q t w i -> q -> T.Tree t -> w
weightTree' wta q tree
  = sum
      [ product (zipWith (weightTree' wta) qs trees) * transWeight t
      | let root = T.rootLabel tree
      , let trees = T.subForest tree
      , let lTrees = length trees
      , t <- transitions wta
      , transState    t == q
      , transTerminal t == root
      , let qs = transStates t
      , lTrees == length qs
      ]


generate :: (Ord q) => WTA q t w i -> [T.Tree t]
generate = fmap (fmap (\(_, t, _) -> t)) . generate'


generate' :: (Ord q) => WTA q t w i -> [T.Tree (q, t, w)]
generate' wta = map fst $ generateHeight wta 0 M.empty


generateHeight
  :: (Num h, Ord h, Ord q)
  => WTA q t w i
  -> h
  -> M.Map q [(T.Tree (q, t, w), h)]
  -> [(T.Tree (q, t, w), h)]
generateHeight wta h m
  = let trees
          = [ ( T.Node (transState t, transTerminal t, transWeight t) trees'
              , h + 1 )
            | t <- transitions wta
            , (trees', h') <- generateSubs (transStates t) m
            , h' == h
            ]
    in if null trees
    then []
    else trees
      ++ generateHeight
          wta
          (h + 1)
          (foldr
            (\x@(t, _) ->
              M.insertWith
                (++)
                (let (q, _, _) = T.rootLabel t in q)
                [x]
            )
            m
            trees
          )


generateSubs :: (Num h, Ord h, Ord q) => [q] -> M.Map q [(t, h)] -> [([t], h)]
generateSubs (q:qs) m
  = let tss = generateSubs qs m
    in maybe
        []
        (\ts' -> [(t':ts, max h' h) | (ts, h) <- tss, (t', h') <- ts'])
        (M.lookup q m)
generateSubs [] _ = [([], 0)]


{-
generate wta q
  = [ T.Node (transTerminal t, q, transWeight t) subs
    | t <- transitions wta
    , transState t == q
    , subs <- combinations (map (generate wta) (transStates t))
    ]


generateH wta 1 q
  = [ T.Node (transTerminal t, q, transWeight t) []
    | t <- transitions wta
    , transState t == q
    , transStates t == []
    ]
generateH wta n q
  = [ T.Node (transTerminal t, q, transWeight t) subs
    | t <- transitions wta
    , transState t == q
    , subs <- combinations (map (generateH wta (n-1)) (transStates t))
    ]


combinations (xs:xss) = [ x:ys | ys <- combinations xss, x <- xs ]
combinations [] = [[]]


split [] = []
split (x:xs) = it [] x xs
  where
    it fxs y zs = (fxs, y, zs):
      case zs of
        (z:zs') -> it (fxs ++ [y]) z zs'
        _ -> []
-}

-- ---------------------------------------------------------------------------

instance (NFData q, NFData t, NFData w) => NFData (Transition q t w i) where
  rnf (Transition l hd tl w i) = rnf l `seq` rnf hd `seq` rnf tl `seq` rnf w

instance (NFData q, NFData t, NFData w) => NFData (WTA q t w i) where
  rnf (WTA s t f) = rnf s `seq` rnf t `seq` rnf f