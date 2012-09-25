{-# LANGUAGE LiberalTypeSynonyms
           , ExistentialQuantification
           , RankNTypes
           , TupleSections
           , EmptyDataDecls #-}

module Main where

import qualified Data.Ix as Ix
import qualified Data.Tree as T

-- import Vanda.Hypergraph.EdgeList as EL
import Vanda.Hypergraph.IntHypergraph

data Var l = Var Int | NV l deriving Show

data WTA l = WTA
             { finalState :: Int
             , transitions :: Hypergraph l ()
             }
             deriving Show

data BOT -- =

type RegRep l l' = l -> [WTA (Var l')] -> WTA (Var l')

rene :: Int -> Int -> WTA l -> WTA l
rene i i' (WTA v0 (Hypergraph vs es))
  = WTA (v0 + i) $ Hypergraph (vs + i) (es' ++ map (mapHE (i +)) es)
  where
    es' = if i' == 0
          then []
          else [ mkHyperedge i' (map (i +) $ from e) (label e) (ident e)
               | e <- es
               , to e == 0
               ]

relab :: [l'] -> WTA l -> WTA (l, l')
relab ls (WTA v0 (Hypergraph vs es))
  = WTA v0
  $ Hypergraph vs
  $ [ mkHyperedge (to e) (from e) (label e, l') (ident e)
    | e <- es
    , l' <- ls
    ]

varta :: RegRep l l' -> Var l -> [WTA (Var l')] -> WTA (Var l')
varta _ (Var i) [] = WTA 0 $ Hypergraph 1 [mkHyperedge 0 [] (Var i) ()]
varta rr (NV l) tas = rr l tas
varta _ _ _ = error "Variables cannot have children"


regrep :: RegRep l l' -> T.Tree (Var l) -> WTA (Var l')
regrep rr = let go (T.Node l ts) = varta rr l (map go ts) in go


data StrLabel = StrConcat | StrConst !Int deriving Show

cumu :: Int -> [Int] -> [Int]
cumu _ [] = []
cumu a (x : xs) = let x' = a + x in x' : cumu x' xs

strrr :: RegRep StrLabel StrLabel
strrr sc@(StrConst _) []
  = WTA 0
  $ Hypergraph 3
  $ [ mkHyperedge 0 [] (NV StrConcat) ()     -- [0,0] -> eps
    , mkHyperedge 2 [] (NV StrConcat) ()     -- [1,1] -> eps
    , mkHyperedge 1 [] (NV sc) ()            -- [0,1] -> i
    , mkHyperedge 0 [0, 0] (NV StrConcat) () -- [0,0] -> [0,0]*[0,0]
    , mkHyperedge 1 [0, 1] (NV StrConcat) () -- [0,1] -> [0,0]*[0,1]
    , mkHyperedge 1 [1, 2] (NV StrConcat) () -- [0,1] -> [0,1]*[1,1]
    , mkHyperedge 2 [2, 2] (NV StrConcat) () -- [1,1] -> [1,1]*[1,1]
    ]
strrr sc@StrConcat tas
  = WTA (st (0, k))
  $ Hypergraph (last bnds)
  $ concat
  $ [ [ mkHyperedge (st (i, i)) [] (NV sc) () | i <- [0..k] ]
    , [ mkHyperedge (st (i, j)) [st (i, i'), st (i', j)] (NV sc) ()
      | i <- [0..k], i' <- [i..k], j <- [i'..k]
      ]
    ]
    ++ 
    [ edges $ transitions $ rene b (st (i - 1, i)) ta
    | (i, b, ta) <- zip3 [1..] bnds tas
    ]
  where
    k = length tas
    ix = ((0, 0), (k, k))
    bnd = Ix.rangeSize ix
    bnds = cumu bnd $ 0 : map (nodes . transitions) tas
    st ij = Ix.index ix ij
strrr _ _ = error "String constants must be nullary"


-- an IRTG is an IntHypergraph l i together with mappings
-- l -> Data.Tree l'
{-
data IntTree
  = Nullary { label :: !Int }
  | Unary   { label :: !Int, succ1 :: IntTree }
  | Binary  { label :: !Int, succ1 :: IntTree, succ2 :: IntTree }
  | Node    { label :: !Int, succ :: [IntTree] }

mkIntTree :: Int -> [IntTree] -> IntTree
mkIntTree l s
  = case s of
      []       -> Nullary { label = l }
      [s1]     -> Unary   { label = l, succ1 = s1 }
      [s1, s2] -> Binary  { label = l, succ1 = s1, succ2 = s2 }
      _        -> Node    { label = l, succ = s }


arity :: IntTree -> Int
arity Nullary{} = 0
arity Unary{} = 1
arity Binary{} = 2
arity Node{ succ = s } = length s


type RegRep = Int -> Int -> IntHypergraph Int ()



data IRTG l i = IRTG
                { rtg :: IntHypergraph l i
                , 
-}

main :: IO ()
main = print
     $ relab [ [1, 2, 3], [4, 5, 6], [7, 8, 9] ]
     $ regrep strrr
     $ T.Node (NV StrConcat)
     $ [ T.Node (Var 0) []
       , T.Node (Var 1) []
       , T.Node (Var 2) []
       ]
