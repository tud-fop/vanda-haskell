-- Copyright (c) 2010, Toni Dietze
{-- snippet types --}
module Data.WTA(Transition(..), WTA, states, transitions, finalWeights, create,
    transIsLeaf, binarize, properize, mapStates, showTransition,
    printTransition, showWTA, printWTA, weightTree, generate) where

import Tools.FastNub(nub)
import Tools.Miscellaneous(mapFst)

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Tree as T
import Data.Maybe(fromJust)

data Transition q t w = Transition
    { transTerminal :: t
    , transState    :: q
    , transStates   :: [q]
    , transWeight   :: w
    } deriving (Eq, Ord, Show)

data WTA q t w = WTA
    { states       :: [q]
    , transitions  :: [Transition q t w]
    , finalWeights :: [(q, w)]
    } deriving Show

create :: (Ord q) => [Transition q t w] -> [(q, w)] -> WTA q t w
create ts fs
  = let ss = nub $
             map fst fs ++
             concatMap (\t -> transState t : transStates t) ts
    in WTA ss ts fs
{-- /snippet types --}


transIsLeaf :: Transition q t w -> Bool
transIsLeaf (Transition {transStates = []}) = True
transIsLeaf _                               = False


binarize :: (Ord q, Num w) => WTA q t w -> WTA [q] (Maybe t) w
binarize wta
  = let states = nub $
                 -- map (:[]) (states wta) ++
                 map ((:[]) . transState) (transitions wta) ++
                 concat
                 [ tailsNonempty w
                 | w <- map transStates (transitions wta)
                 ]
        trans  = [ Transition (Just t) [q] [] w
                 | Transition       t   q  [] w <- transitions wta
                 ] ++
                 [ Transition (Just t) [q] [qs]       w
                 | Transition       t   q   qs@(_:_)  w <- transitions wta
                 ] ++
                 [ Transition Nothing qqs [[q], qs] 1
                 | qqs@(q:qs@(_:_)) <- states
                 ]
        finals = map (mapFst (:[])) (finalWeights wta)
    in WTA states trans finals


tailsNonempty :: [a] -> [[a]]
tailsNonempty []         =  []
tailsNonempty xxs@(_:xs) =  xxs : tailsNonempty xs


properize :: (Ord q, Fractional w) => WTA q t w -> WTA q t w
properize wta@WTA{transitions = ts}
  = let counts =
              M.fromListWith (+)
            $ map (\t -> (transState t, transWeight t))
            $ ts
        normalize t =
            t{transWeight =
              transWeight t / fromJust (M.lookup (transState t) counts)
            }
    in wta{transitions = map normalize ts}


mapStates :: (Ord q) => (p -> q) -> WTA p t w -> WTA q t w
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


showTransition :: (Show q, Show t, Show w) => Transition q t w -> [Char]
showTransition t
  =   show (transState t)
  ++  " -> "
  ++  show (transTerminal t)
  ++  "-"
  ++  show (transStates t)
  ++  " ("
  ++  show (transWeight t)
  ++  ")"


printTransition :: (Show q, Show t, Show w) => Transition q t w -> IO ()
printTransition t = putStrLn . showTransition $ t


showWTA :: (Show q, Show t, Show w) => WTA q t w -> [Char]
showWTA wta
  =   "Transitions:\n"
  ++  (unlines $ map showTransition $ transitions wta)
  ++  "\nStates:\n"
  ++  (unlines $ map show $ states wta)
  ++  "\nFinal Weights:\n"
  ++  (unlines $ map show $ finalWeights wta)


printWTA :: (Show q, Show t, Show w) => WTA q t w -> IO ()
printWTA wta = putStr . showWTA $ wta


weightTree :: (Eq q, Eq t, Num w) => WTA q t w -> T.Tree t -> w
weightTree wta tree
  = sum
  . map (\(q, w) -> weightTree' wta q tree * w)
  $ finalWeights wta


weightTree' :: (Eq q, Eq t, Num w) => WTA q t w -> q -> T.Tree t -> w
weightTree' wta q tree
  = sum
      [ product (zipWith (weightTree' wta) qs trees) * (transWeight t)
      | let root = T.rootLabel tree
      , let trees = T.subForest tree
      , let lTrees = length trees
      , t <- transitions wta
      , transState    t == q
      , transTerminal t == root
      , let qs = transStates t
      , lTrees == length qs
      ]


generate :: (Ord q) => WTA q t w -> [T.Tree (t, q, w)]
generate wta = map fst $ generateHeight wta 0 M.empty


generateHeight
  :: (Num h, Ord h, Ord q)
  => WTA q t w
  -> h
  -> M.Map q [(T.Tree (t, q, w), h)]
  -> [(T.Tree (t, q, w), h)]
generateHeight wta h m
  = let trees
          = [ ( T.Node (transTerminal t, transState t, transWeight t) trees'
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
                (let (_, q, _) = T.rootLabel t in q)
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