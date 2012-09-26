{-# LANGUAGE LiberalTypeSynonyms
           , ExistentialQuantification
           , RankNTypes
           , TupleSections
           , EmptyDataDecls
           , RecordWildCards #-}

module Main where

import Control.Monad ( when, forM_, forM, liftM3 )
import Control.Monad.ST
import qualified Data.Array.Base as AB
-- import qualified Data.Array.MArray as MA
import qualified Data.Array.ST as STA
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Ix as Ix
import qualified Data.Map as M
import Data.STRef
import qualified Data.Tree as T

import Debug.Trace

-- import Vanda.Hypergraph.EdgeList as EL
import Vanda.Hypergraph.IntHypergraph
import Vanda.Util

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


{- chTo :: Int -> Hyperedge l i -> Hyperedge l i
chTo i e@Nullary{} = e{ to = i } -} 

type GigaMap = M.Map IS.IntSet Int

forwMskel
  :: forall l' . Show l' => GigaMap -> Int -> WTA (Var l') -> (WTA Int, GigaMap, Int)
forwMskel gm_ gmi_ WTA{ .. } = runST $ do
  gm <- newSTRef gm_         -- maps variable sets to terminal symbols
  gmi <- newSTRef gmi_       -- max. terminal symbol
  nt <- STA.newArray (0, nodes transitions - 1) []
        :: ST s (STA.STArray s Int [Hyperedge Int ()])
  -- mmap :: GigaMap <- M.empty -- maps variable sets to states of the wta
  imap <- newSTRef (IM.empty :: IM.IntMap (IS.IntSet, Int))
  -- ^ maps each state to its var. set and the gm image of that var. set
  forwA <- computeForwardA transitions
  q <- newSTRef [ e | e@Nullary{} <- edges transitions ]
  let -- addt v e = AB.unsafeRead nt v >>= AB.unsafeWrite nt v . (e :)
      register s = do
        mb <- fmap (M.lookup s) $ readSTRef gm
        case mb of
          Nothing -> do
                       i <- readSTRef gmi
                       modifySTRef' gmi (+ 1)
                       modifySTRef' gm $ M.insert s i
                       return i
          Just i -> return i
      construct = fmap concat
                $ forM [0 .. nodes transitions - 1]
                $ AB.unsafeRead nt
      go = do
        lviewSTRef' q
          ( liftM3 (,,)
              (fmap (WTA finalState . mkHypergraph) construct)
              (readSTRef gm)
              (readSTRef gmi)
          )
          $ \ e -> do
            case e of
              Nullary{ label = Var i, .. } -> let s = IS.singleton i in do
                ti <- register s
                modifySTRef' imap $ IM.insert to (s, ti)
                es <- AB.unsafeRead nt to
                AB.unsafeWrite nt to (Nullary{ label = ti, .. } : es)
              Nullary{ .. } -> do
                es <- AB.unsafeRead nt to
                when (null es) $ let s = IS.empty in do
                  ti <- register s
                  modifySTRef' imap $ IM.insert to (s, ti)
                  AB.unsafeWrite nt to (Nullary{ label = ti, ..} : es)
              Unary{ .. } -> do
                sti <- fmap (IM.! from1) $ readSTRef imap
                modifySTRef' imap $ IM.insert to sti
                -- divert transitions that end in from also to to
                esf <- AB.unsafeRead nt from1
                est <- AB.unsafeRead nt to
                AB.unsafeWrite nt to $ est ++ map (\ e1 -> e1{ to = to }) esf
              Binary{ .. } -> do
                (s1, ti1) <- fmap (IM.! from1) $ readSTRef imap
                (s2, ti2) <- fmap (IM.! from2) $ readSTRef imap
                let s = s1 `IS.union` s2
                ti <- register s
                modifySTRef imap $ IM.insert to (s, ti)
                est <- AB.unsafeRead nt to
                case (ti == ti1, ti == ti2, to /= from1, to /= from2) of
                  (False, False, _, _) ->   -- insert new edge
                    AB.unsafeWrite nt to (Binary{ label = ti, .. } : est)
                  (True, False, True, _) -> do -- divert from1 also to to
                    esf <- AB.unsafeRead nt from1
                    AB.unsafeWrite nt to $ est ++ map (\ e1 -> e1{ to = to }) esf
                  (False, True, _, True) -> do -- divert from2 also to to
                    esf <- AB.unsafeRead nt from2
                    AB.unsafeWrite nt to $ est ++ map (\ e1 -> e1{ to = to }) esf
                  _ -> return () -- probably s1 = s2 = s = empty
              _ -> error "WTA is not BINARY"
            hes <- AB.unsafeRead forwA (to e)
            forM_ hes $ updateHe $ \ e1 -> modifySTRef' q (e1 :)
            AB.unsafeWrite forwA (to e) []
            go
  go


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

fst3 (x, _, _) = x

myshow WTA { .. } = "Final state: " ++ show finalState ++ "\nTransitions:\n"
                    ++ unlines (map show (edges transitions))

main :: IO ()
main = let wta = regrep strrr
               $ T.Node (NV StrConcat)
               $ [ T.Node (Var 0) []
                 , T.Node (Var 1) []
                 , T.Node (Var 2) []
                 , T.Node (Var 3) []
                 , T.Node (Var 4) []
                 , T.Node (Var 5) []
                 , T.Node (Var 6) []
                 , T.Node (Var 7) []
                 , T.Node (Var 8) []
                 , T.Node (Var 9) []
                 ]
       in do
            putStrLn $ myshow wta
            putStrLn $ myshow $ fst3 $ forwMskel M.empty 0 wta
     -- $ relab [ [1, 2, 3], [4, 5, 6], [7, 8, 9] ]

