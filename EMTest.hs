module EMTest where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Hypergraph
import Algorithms.ExpectationMaximization
import Tools.Miscellaneous (mapSnd)
import Tools.PrettyPrint (prettyIndentBracktes)

type Grammar n t i = ([(n, n, n, i)], [(n, t, i)])

foreach :: [i] -> (i -> s -> s) -> s -> s
foreach l iter = foldr (flip (.)) id (map iter l)

data State n t i = State
  (S.Set (Int,n,Int))
  [((Int,n,Int),(Int,n,Int),(Int,n,Int),i)]

fst3 :: (a, b, c) -> a
fst3 (a,_,_) = a

cyk :: (Ord n,Eq t) => Grammar n t i -> [t] -> Grammar (Int, n, Int) t i
cyk (l1, l2) w = (reverse l1', l2')
  where
  -- l2' is simple
  l2' = [((i,n,i+1),t,_id) | (i,t) <- zip [0..] w, (n,t',_id) <- l2, t' == t]
  -- for l1', we use foreach
  State _ l1' = foreach index iter initial
  index = [(i,j,k,p) | k <- [2..m], i <- [0..m-k], j <- [1..k-1], p <- l1 ]
  m = length w
  initial = State (S.fromList . fst3 . unzip3 $ l2') []
  iter (i,j,k,(n1,n2,n3,_id)) state@(State items l1'')
    | (i,n2,i+j) `S.member` items && (i+j,n3,i+k) `S.member` items = state1
    | otherwise = state
    where
      state1 = State (S.insert (i,n1,i+k) items) (((i,n1,i+k),(i,n2,i+j),(i+j,n3,i+k),_id):l1'')

makeHyper
  :: (Ord n)
  => Grammar n t i
  -> Hypergraph n (Either (n,n,n) (n,t)) Double i
makeHyper (l1, l2) = hypergraph (map f2 l2 ++ map f1 l1) where
  f1 (a1, a2, a3, _id) = hyperedge a1 [a2,a3] (Left (a1,a2,a3)) 1.0 _id
  f2 (a, t, _id) = hyperedge a [] (Right (a,t)) 1.0 _id

doEM
  :: (Ord n, Eq t, Ord i, Show n)
  => Grammar n t i
  -> n
  -> [[t]]
  -> [(Double, M.Map i Double)]
doEM g@(l1,l2) n0 es
  = take 10 $ forestEMlist
    part
    [((0,n0,length e), makeHyper (cyk g e), 1) | e <- es]
    eId
    -- (\dl it -> it > 10)
    (normalize part initM) where
      initM = M.fromList [(_id, 1.0) | _id <- concat part ]
      part
        = map (S.elems)
        . M.elems
        $ foldl (flip f2) (foldl (flip f1) M.empty l1) l2 
      f1 (a1,_,_,_id) = M.insertWith (S.union) a1 (S.singleton _id)
      f2 (a,_,_id) = M.insertWith (S.union) a (S.singleton _id)

main :: IO ()
main = do
  --print (cyk ([('Z','Z','Z',1)],[('Z','a',2)]) "aaaaa")
  putStr . prettyIndentBracktes . show $ g
  -- putStr . prettyIndentBracktes . show $ es
  putStr . prettyIndentBracktes . show $ zip es $ map (cyk g) es
  putStr . prettyIndentBracktes . show $ map (mapSnd M.assocs) $ doEM g 'S' es
  putStr "\n" where
    -- g = ([('Z','Z','Z',1)],[('Z','a',2)])
    -- g = ([('Z','A','Z',1),('Z','Z','A',2),('Z','A','A',3)],[('A','a',4)])
    g = (
      [
        ('S','S','S',1)
      , ('S','A','T',2)
      , ('T','S','B',3)
      , ('S','A','B',4)
      , ('S','B','A',5)
      ],
      [
        ('S','c',6)
      , ('A','a',7)
      , ('B','b',8)
      ])
    es = ["abc", "acb", "abab"]

