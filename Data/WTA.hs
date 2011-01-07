-- Copyright (c) 2010, Toni Dietze
{-- snippet types --}
module Data.WTA(Transition(..), WTA, states, transitions, finalWeights, create,
    transIsLeaf, binarize, properize, mapStates, showTransition,
    printTransition, showWTA, printWTA) where

import Tools.FastNub(nub)
import Tools.Miscellaneous(mapFst)

import qualified Data.Map as M
import Data.Maybe(fromJust)

data Transition state terminal weight = Transition
    { transTerminal :: terminal
    , transState    :: state
    , transStates   :: [state]
    , transWeight   :: weight
    } deriving (Eq, Ord, Show)

data WTA state terminal weight = WTA
    { states       :: [state]
    , transitions  :: [Transition state terminal weight]
    , finalWeights :: [(state, weight)]
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


showTransition t
  =   show (transState t)
  ++  " -> "
  ++  show (transTerminal t)
  ++  "-"
  ++  show (transStates t)
  ++  " ("
  ++  show (transWeight t)
  ++  ")"

printTransition t = putStrLn . showTransition $ t

showWTA wta
  =   "Transitions:\n"
  ++  (unlines $ map showTransition $ transitions wta)
  ++  "\nStates:\n"
  ++  (unlines $ map show $ states wta)
  ++  "\nFinal Weights:\n"
  ++  (unlines $ map show $ finalWeights wta)

printWTA wta = putStr . showWTA $ wta
