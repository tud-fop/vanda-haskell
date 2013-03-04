{-# LANGUAGE ImpredicativeTypes #-}
-- (c) 2012 Matthias Büchse <Matthias.Buechse@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

module Vanda.Grammar.XRS.Binarize ( binarizeXRS ) where

import Prelude hiding ( sequence )

import Control.Monad ( forM_, forM )
import Control.Monad.ST
import qualified Data.Array as A
import Data.Function ( on )
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Ix as Ix
import Data.List ( foldl', sortBy )
import qualified Data.Map as M
import Data.Maybe ( listToMaybe, fromJust )
import Data.NTT
import qualified Data.Set as S
import qualified Data.Vector as V

import Vanda.Grammar.XRS.IRTG
import Vanda.Hypergraph.IntHypergraph
import qualified Vanda.Hypergraph.Tree as T
import Vanda.Util

-- import Debug.Trace ( traceShow )


-- myshow WTA { .. } = "Final state: " ++ show finalState ++ "\nTransitions:\n"
--                     ++ unlines (map show (edges transitions))

data WTA l = WTA
             { finalState :: Int
             , portStates :: [Int]
             , transitions :: Hypergraph l ()
             }
             deriving Show


type RegSeed l = l -> WTA (Var l)


liftOp :: WTA (Var l) -> [WTA (Var l)] -> WTA (Var l)
liftOp (WTA q0 qs (Hypergraph vs es)) tas
  = WTA q0 []
  $ Hypergraph (last bnds)
  $ es
    ++
    [ mkHyperedge toe' (map (b +) (from e)) (label e) (ident e)
    | (q, b, ta) <- zip3 qs bnds tas
    , e <- edges $ transitions ta
    , let toe = to e
    , let toe' = if toe == finalState ta then q else b + toe
    ]
  where
    bnds = cumu vs $ 0 : map (nodes . transitions) tas
    cumu :: Int -> [Int] -> [Int]
    cumu _ [] = []
    cumu a (x : xs) = let x' = a + x in x' : cumu x' xs


liftSeed :: RegSeed l -> T.Tree (Var l) -> WTA (Var l)
liftSeed rs t
  = case T.rootLabel t of
      NV l' -> liftOp (rs l') $ map (liftSeed rs) $ T.subForest t
      Var i -> WTA 0 [] $ Hypergraph 1 [mkHyperedge 0 [] (var i) ()]


data Label = Concat !Int | Symbol !Int deriving (Eq, Ord, Show)


strrr :: RegSeed Label
strrr sc@Symbol{}
  = WTA 0 [] $ Hypergraph 1 $ [ mkHyperedge 0 [] (NV sc) () ]
strrr (Concat 0)
  = WTA 0 [] $ Hypergraph 1 $ [ mkHyperedge 0 [] (NV (Concat 0)) () ]
strrr (Concat 1)
  = error "no concat 1, please"
strrr (Concat k)
  = WTA (st (0, k)) [ st (i - 1, i) | i <- [1..k] ]
  $ Hypergraph (Ix.rangeSize ix)
  $ [ mkHyperedge (st (i, j)) [st (i, i'), st (i', j)] (NV (Concat 2)) ()
    | i <- [0 .. k]
    , i' <- [i + 1 .. k]
    , j <- [i' + 1 .. k]
    ]
  where
    ix = ((0, 0), (k, k))
    st ij = Ix.index ix ij


treerr :: RegSeed Label
treerr sc@Symbol{}
  = WTA 0 [1] $ Hypergraph 2 $ [ mkHyperedge 0 [1] (NV sc) () ]
treerr tc@Concat{} = strrr tc



type GigaMap = M.Map IS.IntSet Int

type VarMap = IM.IntMap (IS.IntSet, Int)


vars :: Var l -> IS.IntSet
vars (Var i) = IS.singleton i
vars _ = IS.empty


computeVars :: GigaMap -> WTA (Var l) -> (GigaMap, VarMap)
computeVars gm_ WTA{ transitions = ts, .. } = runST $ do
  gm <- newSTRef gm_
  amap <- newSTRef (IM.empty :: VarMap)
  traverseForward ts (\ _ -> pairM (readSTRef gm, readSTRef amap)) $ \ e -> do
    s <- fmap ( foldl' IS.union (vars (label e)) . flip map (from e)
              . \ am -> fst . (am IM.!) ) $ readSTRef amap
    ti <- register gm s
    modifySTRef amap $ IM.insert (to e) (s, ti)


computeVars' :: [WTA (Var l)] -> [VarMap]
computeVars' = reverse . snd . flip foldl' (M.empty, [])
                  (\ (gm, vs) ta -> second' (: vs) $ computeVars gm ta)


sequence :: VarMap -> [Int] -> [Int]
sequence am qs
  = map snd
  $ sortBy (compare `on` fst)
  $ [ (IS.findMin s, mq)
    | q <- qs
    , let (s, mq) = am IM.! q
    , not (IS.null s)
    ]


forwMskel :: VarMap -> WTA l -> WTA Int
forwMskel am WTA{ finalState = q0, transitions = tr }
  = WTA{ finalState = snd $ am IM.! q0
       , portStates = []
       , transitions = mkHypergraph $ S.toList $ S.fromList $ tr'
       }
  where
    tr' = [ mkHyperedge mtoe (sequence am (from e)) mtoe (ident e)
          | e <- edges tr
          , let (_, mtoe) = am IM.! to e
          ]


inters :: Eq l => WTA l -> WTA l -> WTA l
inters (WTA fs1 _ (Hypergraph vs1 tr1)) (WTA fs2 _ (Hypergraph vs2 tr2))
  = WTA (st (fs1, fs2)) [] $ Hypergraph (Ix.rangeSize ix) tr
  where
    ix = ((0, 0), (vs1 - 1, vs2 - 1))
    st ij = Ix.index ix ij
    tr = [ mkHyperedge
             (st (to e1, to e2))
             (map st (zip (from e1) (from e2)))
             ll
             ()
         | e1 <- tr1
         , e2 <- tr2
         , arity e1 == arity e2
         , let ll = label e1
         , ll == label e2
         ]


type Branches = IM.IntMap [Int]

extractBranches :: Branches -> [T.Tree Int] -> Branches
extractBranches !s [] = s
extractBranches !s (T.Nullary{} : ts) = extractBranches s ts
extractBranches !s (T.Binary i t1 t2 : ts) = extractBranches
              (IM.insert i [T.rootLabel t1, T.rootLabel t2] s) (t1 : t2 : ts)
extractBranches _ _ = error "should not happen: branches"


backMskel :: Branches -> VarMap -> WTA (Var l') -> WTA (Var l')
backMskel branches am wta@WTA{ transitions = tr@Hypergraph{ .. } }
  = wta{ transitions = tr{ edges = filter p edges } }
  where
    p e = let s = sequence am (from e)
          in length s < 2 || s == IM.findWithDefault [] (snd $ am IM.! to e) branches


fmap' :: (Int -> Int) -> Var l -> Var l
fmap' _ nv@NV{} = nv
fmap' f (Var i) = var (f i)


freshen :: (Int -> Int) -> T.Tree (Var l) -> T.Tree (Var l)
freshen f = fmap (fmap' f)


replace
  :: Int -> T.Tree (Var l) -> T.Tree (Var l) -> T.Tree (Var l)
replace i t' t
  = case T.rootLabel t of
      NV _ -> T.mapChildren (replace i t') t
      Var i' | i == i' -> t'
             | otherwise -> t

merge
  :: T.Tree (Var l)
  -> Int
  -> Int
  -> [(Int, (T.Tree (Var (T.Tree (Var l)), IS.IntSet)))]
  -> [(Int, (T.Tree (Var (T.Tree (Var l)), IS.IntSet)))]
  -> (T.Tree (Var l), [(Int, (T.Tree (Var (T.Tree (Var l)), IS.IntSet)))])
merge frag _ _ [] ts' = (frag, ts')
merge frag k l (it@(i, t) : ts) ts'
  = case T.rootLabel t of
      (Var _, _) -> merge frag k l ts (it : ts')
      (NV t', _) -> let l' = T.arity t in
        case k + l' - 1 of
          k' | k' <= 2 -> merge
                            (replace i (freshen (+ l) t') frag)
                            k' (l + l') ts
                            (map (first' (+ l)) (zip [0..] (T.subForest t))
                              ++ ts')
             | otherwise -> merge frag k l ts (it : ts')


decompose :: Show l => T.Tree (Var l) -> T.Tree (Var (T.Tree (Var l)), IS.IntSet)
decompose (T.Nullary (Var i))
  = let is = IS.singleton i
    in T.Unary (NV (T.Nullary (var 0)), is) (T.Nullary (var i, is))
-- above: inserted NV (T.Nullary ...), otherwise we can get "empty" decompositions (only x0)
decompose (T.Nullary (NV l)) = T.Nullary (NV (T.Nullary (NV l)), IS.empty)
decompose t
  = case T.rootLabel t of
      NV l -> let ts = map decompose (T.subForest t)
                  k = length ts
                  ini = T.node (NV l) [ T.Nullary (var i) | i <- [0..k - 1] ]
                  tssort = sortBy (compare `on` fst) [ (T.arity $ snd p, p) | p <- zip [0..] ts ]
                  (mg, ts') = merge ini k k (map snd tssort) []
                  sorted = sortBy (compare `on` (IS.findMin . snd . T.rootLabel . snd)) ts'
                  revar = IM.fromList $ zip (map fst sorted) [0..]
                  fin = freshen (revar IM.!) mg
              in T.node (NV fin, foldl' IS.union IS.empty (map (snd . T.rootLabel . snd) ts'))
                   (map snd sorted)


mkHom :: (t -> t') -> M.Map t Int -> V.Vector t'
mkHom f m = V.fromList $ A.elems $ A.array (0, M.size m - 1)
          $ map (swap . first' f) $ M.toList m


binarizeTerms
  :: Show l
  => RegSeed l
  -> RegSeed l
  -> (forall l'. (WTA l' -> Maybe (T.Tree l')))
  -> (forall l'. (WTA l' -> Maybe (T.Tree l')))
  -> (T.Tree (Var l), T.Tree (Var l))
  -> Maybe (T.Tree (Var (T.Tree (Var l))), T.Tree (Var (T.Tree (Var l))))
binarizeTerms b1 b2 select select1 (h1alpha, h2alpha)
  = let rhs1 = liftSeed b1 h1alpha
        rhs2 = liftSeed b2 h2alpha
        [m1, m2] = computeVars' [rhs1, rhs2]
        vb1 = forwMskel m1 rhs1
        vb2 = forwMskel m2 rhs2
        b' = inters vb1 vb2
    in do
       t' <- select b'
       let bran = extractBranches IM.empty [t']
       let t1 = fromJust $ select1 $ backMskel bran m1 rhs1
       let t2 = fromJust $ select1 $ backMskel bran m2 rhs2
       return (fmap fst $ decompose t1, fmap fst $ decompose t2)


smallest :: WTA l -> Maybe (T.Tree l)
smallest (WTA fs _ tr)
  = fmap (fmap label . deriv) $ listToMaybe
  $ (A.! fs) $ knuth tr (\ _ _ _ -> 1.0)


smallestLeftHeavy :: WTA l -> Maybe (T.Tree l)
smallestLeftHeavy (WTA fs _ tr)
  = fmap (fmap label . deriv) $ listToMaybe
  $ (A.! fs) $ knuth tr
  $ \ _ _ ds -> foldl' (\ x (y, z) -> x - y * z) 0.0 (zip [1..] ds)


binarizeXRS :: IRTG Int -> IRTG Int
binarizeXRS irtg@IRTG{ .. }
  = runST $ do
    tr <- newSTRef []
    newh1 <- newSTRef M.empty
    newh2 <- newSTRef M.empty
    virt <- newSTRef M.empty
    let addRule v vs ti si i
          = let e = mkHyperedge v vs (SIP ti si) i
            in e `seq` modifySTRef' tr (e :)
    let mkRules atroot e (t1, t2)
          = case (T.rootLabel t1, T.rootLabel t2) of
              (Var i, Var _) -> return $ e `deref` i
              (NV f1, NV f2) -> do
                vs <- forM (zip (T.subForest t1) (T.subForest t2))
                        $ mkRules False e
                ti <- register newh1 f1
                si <- register newh2 f2
                if atroot
                  then let v = to e
                       in addRule v vs ti si (ident e) >> return v
                  else register' virt (+ nodes rtg) (ti, si, vs)
                         $ \ v -> addRule v vs ti si (-1)
              _ -> error "something BAD has happened"
    forM_ (edges rtg) $ \ e ->
      case label e of
        SIP i1 i2 ->
          let tterm = h1convert (h1 V.! i1)
              sterm = h2convert $ V.toList (h2 V.! i2)
          in case binarizeTerms treerr strrr smallest smallest -- LeftHeavy
                    (tterm, sterm) of
            Just decomp ->
              mkRules True e decomp >> return ()
            Nothing -> do -- carry over
              ti <- register newh1 tterm
              si <- register newh2 sterm
              modifySTRef' tr (e{ label = SIP ti si } :)
    nodes <- fmap ((+ nodes rtg) . M.size) $ readSTRef virt
    edges <- readSTRef tr
    h1_ <- fmap (mkHom $ fmap h1cc) $ readSTRef newh1
    h2_ <- fmap (mkHom (V.fromList . h2cc)) $ readSTRef newh2
    return irtg{ rtg = Hypergraph{ .. }, h1 = h1_, h2 = h2_ } 
  where
    h1convert t
      = case T.rootLabel t of
          NT i -> T.Nullary (var i)
          T i -> case T.subForest t of
                   [t'] -> T.Unary (NV (Symbol i)) (h1convert t')
                   ts -> T.Unary (NV (Symbol i))
                         $ T.node (NV (Concat (length ts))) (map h1convert ts)
    h2convert [x] = h2cv x
    h2convert xs = T.node (NV (Concat (length xs))) (map h2cv xs)
    h2cv (NT i) = T.Nullary (var i)
    h2cv (T i) = T.Nullary (NV (Symbol i))
    h1cc (Var i) = nt i
    h1cc (NV (Symbol i)) = tt i
    h1cc (NV (Concat _)) = T (-1)
    h2cc (T.Nullary (Var i)) = [nt i]
    h2cc (T.Nullary (NV (Symbol i))) = [tt i]
    h2cc (T.Binary (NV (Concat _)) c1 c2) = h2cc c1 ++ h2cc c2
    h2cc (T.Node (NV (Concat _)) cs) = concatMap h2cc cs
    h2cc t = error (show t) -- "should not happen"

-- fst3 (x, _, _) = x

{-
main :: IO ()
main = let swta = regrep strrr
                $ T.Node (NV StrConcat)
                $ [ T.Node (Var i) []
                  | i <- [0 .. 2]
                  ]
           twta = regrep treerr
                $ T.Node (NV (TreeConcat 0))
                $ [ T.Node (NV (TreeConcat 1)) []
                  , T.Node (Var 2) []
                  , T.Node (Var 1) []
                  , T.Node (Var 0) []
                  ]
           gm0 = M.singleton IS.empty 0
           gmi0 = 1
           (fswta, smap, gm1, gmi1) = forwMskel gm0 gmi0 swta
           (ftwta, tmap, _, _)      = forwMskel gm1 gmi1 twta
           inter = inters fswta ftwta
           options (WTA fs tr) = (A.! fs) $ knuth tr (\ _ _ _ -> 1.0)
           choose = fmap label . deriv . head
           bran = extractBranches IM.empty [ choose (options inter) ]
           (sswap, sback) = backMskel smap swta bran
           (tswap, tback) = backMskel tmap twta bran
           stree = choose (options sback)
           ttree = choose (options tback)
       in do
            putStrLn $ myshow swta
            putStrLn $ myshow twta
            putStrLn $ myshow inter
            -- putStrLn $ myshow sback
            -- putStrLn $ myshow tback
            -- print $ choose inter
            -- print $ stree
            -- print $ dissect sswap stree
            -- print $ ttree
            -- print $ dissect tswap ttree
     -- $ relab [ [1, 2, 3], [4, 5, 6], [7, 8, 9] ]
-}
