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

{-# LANGUAGE FlexibleContexts #-}  -- for 'Stream'

module Vanda.Grammar.XRS.Text
  ( XRSRule
  , mkIRTG
  , parseXRSRule
  , prettyPrint'
  , prettyPrint
  , prettyPrintJoshua'
  , prettyPrintJoshua
  ) where

import Control.Applicative ( many )
import Control.Monad ( forM_, forM )
import Control.Monad.ST
import Control.Seq
import qualified Data.Array as A
import qualified Data.IntMap as IM
import Data.List ( foldl', elemIndex )
import Data.NTT
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import qualified Data.Vector as V
import Text.Parsec hiding ( many, label )
import Text.Parsec.Text.Lazy

import Vanda.Grammar.XRS.IRTG
import Vanda.Hypergraph.IntHypergraph
import Vanda.Hypergraph.NFData ()
import qualified Vanda.Hypergraph.Tree as T
import Vanda.Token
import Vanda.Util
import Data.Interner


data XRSRule
  = XRSRule
    { lhs :: !(T.Tree (Var TS.Text))
    , rhs :: ![Var TS.Text]
    , qs :: !(IM.IntMap TS.Text)
    , weight :: !Double
    }


parseXRSRule :: T.Text -> XRSRule
parseXRSRule t
  = case parse p_rule (T.unpack t) t of
      Left e -> error (show e)
      Right r -> r


p_rule :: Parser XRSRule
p_rule = do
  (lhs, qs) <- p_tree
  _ <- string "-> "
  rhs <- p_string
  _ <- string "||| "
  weight <- p_weight
  return $! XRSRule{ .. }


p_tree :: Parser (T.Tree (Var TS.Text), IM.IntMap TS.Text)
p_tree = choice [ p_tterm, p_tvar, p_tnonterm ]

p_tterm :: Parser (T.Tree (Var TS.Text), IM.IntMap TS.Text)
p_tterm = do
  _ <- char '"'
  !si <- fmap TS.pack $ many (noneOf "\"")
  _ <- char '"'
  spaces
  return (T.node (NV si) [], IM.empty)

p_tvar :: Parser (T.Tree (Var TS.Text), IM.IntMap TS.Text)
p_tvar = do
  _ <- char 'x'
  i <- fmap read $ many1 $ oneOf "0123456789"
  _ <- char ':'
  !si <- fmap TS.pack $ many (noneOf " )")
  spaces
  return (T.node (Var i) [], IM.singleton i si)

p_tnonterm :: Parser (T.Tree (Var TS.Text), IM.IntMap TS.Text)
p_tnonterm = do
  !si <- fmap TS.pack $ many (noneOf " ()")
  _ <- char '('
  (ts, qss) <- fmap unzip $ many p_tree
  _ <- char ')'
  spaces
  return (T.node (NV si) ts, foldl' IM.union IM.empty qss)


p_string :: Parser [Var TS.Text]
p_string = many $ choice [ p_sterm, p_svar ]

p_sterm :: Parser (Var TS.Text)
p_sterm = do
  _ <- char '"'
  !si <- fmap TS.pack $ many (noneOf "\"")
  _ <- char '"'
  spaces
  return $ NV si

p_svar :: Parser (Var TS.Text)
p_svar = do
  _ <- char 'x'
  !i <- fmap read $ many1 $ oneOf "0123456789"
  spaces
  return $ Var i


p_weight :: Parser Double
p_weight = do
  c <- oneOf "0123456789"
  cs <- many (oneOf "0123456789.E-")
  return $! read (c : cs)


mkLst :: Interner t -> [t]
mkLst = A.elems . internerToArray


mkHom :: Interner t -> V.Vector t
mkHom = V.fromList . mkLst


registerToken :: STRef s TokenMap -> TS.Text -> ST s Int
registerToken m s = do -- i <- register m s
  (m', i) <- fmap (flip updateToken s) $ readSTRef m
  writeSTRef m m'
  return i


registerTree :: STRef s TokenMap -> T.Tree (Var TS.Text) -> ST s (T.Tree NTT)
registerTree m t = do
  t' <- forM (T.subForest t) (registerTree m)
  case T.rootLabel t of
    Var i -> let i' = nt i in i' `seq` return $! T.node i' t'
    NV s -> do
      i <- registerToken m s
      let i' = tt i in i' `seq` return $! T.node i' t'


registerString :: STRef s TokenMap -> [Var TS.Text] -> ST s (V.Vector NTT)
registerString m xs
  = fmap V.fromList $ forM xs $ \ x ->
      case x of
        Var i -> let i' = nt i in i' `seq` return i' -- return $! nt i
        NV s -> do
          i <- registerToken m s
          let i' = tt i in i' `seq` return i' -- return $! tt i


mkIRTG
  :: (TokenMap, TokenMap, TokenMap)
  -> [XRSRule]
  -> (IRTG Int, [Double], TokenMap, TokenMap, TokenMap)
mkIRTG (em_, fm_, nm_) rs_ = runST $ do
  em <- newSTRef em_
  fm <- newSTRef fm_
  nm <- newSTRef nm_
  tm <- newSTRef (emptyInterner :: Interner (T.Tree NTT))
  sm <- newSTRef (emptyInterner :: Interner (V.Vector NTT))
  ws <- newSTRef (emptyInterner :: Interner Double)
  rs <- newSTRef ([] :: [Hyperedge StrictIntPair Int])
  forM_ rs_ $ \ XRSRule{ .. } -> let NV q = T.rootLabel lhs in do
    !q_ <- registerToken nm q
    !qs_ <- forM (IM.elems qs) $ registerToken nm
    !lhs_ <- registerTree em lhs
    !rhs_ <- registerString fm rhs
    !ti <- internST tm lhs_
    !si <- internST sm rhs_
    !i <- internST ws weight
    let e = mkHyperedge q_ (qs_ `using` seqList rseq) (SIP ti si) i
      in e `seq` modifySTRef' rs (e :)
  rtg <- fmap mkHypergraph $ readSTRef rs
  h1 <- fmap mkHom $ readSTRef tm
  h2 <- fmap mkHom $ readSTRef sm
  initial <- fmap (flip getToken (TS.pack "ROOT")) $ readSTRef nm
  let irtg = return $! IRTG{ .. }
  let ws_ = fmap mkLst $ readSTRef ws
  quintM (irtg, ws_, readSTRef em, readSTRef fm, readSTRef nm)




  

att :: T.Text
att = T.singleton '@'

lrb :: T.Text
lrb = T.singleton '('

rrb :: T.Text
rrb = T.singleton ')'

spa :: T.Text
spa = T.singleton ' '

col :: T.Text
col = T.singleton ':'

dqu :: T.Text
dqu = T.singleton '"'


toString' :: TokenArray -> TokenArray -> Hyperedge l i -> T.Tree NTT -> T.Text
toString' ta na e = go
  where
    gs i = if i < 0 then att else T.pack $ TS.unpack $ getString ta i
    go (T.Nullary (T (-1))) = att
    go (T.Nullary (T i)) = T.concat [dqu, gs i, dqu]
    go (T.Nullary (NT i)) = T.concat
                            [ T.pack ("x" ++ show i)
                            , col
                            , T.pack $ TS.unpack $ getString na (e `deref` i)
                            ]
    go (T.Unary (T i) t) = T.concat [gs i, lrb, go t, rrb]
    go (T.Binary (T i) t1 t2)
      = T.concat [gs i, lrb, go t1, spa, go t2, rrb]
    go (T.Node (T i) sF)
      = T.concat [gs i, lrb, T.unwords (map go sF), rrb]


prettyPrintNTT :: TokenArray -> NTT -> T.Text
prettyPrintNTT ta (T i)
  = T.concat [dqu, T.pack (TS.unpack (getString ta i)), dqu]
prettyPrintNTT _ (NT i)
  = T.pack ("x" ++ show i)


prettyPrint'
  :: TokenArray
  -> TokenArray
  -> TokenArray
  -> (l -> T.Tree NTT)
  -> (l -> [NTT])
  -> (i -> Double)
  -> Hyperedge l i
  -> T.Text
prettyPrint' ea fa na h1 h2 w e
  = let l = label e
    in T.concat
    [ T.pack $ TS.unpack $ getString na (to e)
    , T.pack ": "
    , toString' ea na e $ h1 l
    , T.pack " -> "
    , T.unwords $ map (prettyPrintNTT fa) $ h2 l
    , T.pack " ||| "
    , T.pack $ show $ w (ident e)
    ]


prettyPrint
  :: TokenArray
  -> TokenArray
  -> TokenArray
  -> IRTG Int
  -> V.Vector Double
  -> Hyperedge StrictIntPair Int
  -> T.Text
prettyPrint ea fa na IRTG{ .. } w e
  = prettyPrint' ea fa na ((h1 V.!) . _fst) (V.toList . (h2 V.!) . _snd)
      (\ i -> if i < 0 then 1.0 else w V.! i) e


prettyPrintJoshua'
  :: TokenArray
  -> TokenArray
  -> TokenArray
  -> (l -> [NTT])
  -> (l -> [NTT])
  -> (i -> Double)
  -> Hyperedge l i
  -> T.Text
prettyPrintJoshua' ea fa na h1 h2 w e
  = let l = label e
        lhs = h2 l
        -- lhs /= [NT 0]
        xs = [ i | NT i <- lhs ]
    in T.pack $
        "[" ++ show (to e) ++ "] ||| "
        ++ unwords
           [ case x of
               NT i -> let Just j = i `elemIndex` xs
                       in "[" ++ show (e `deref` j) ++ "," ++ show (j + 1) ++ "]"
               T i -> TS.unpack $ getString fa i
           | x <- lhs
           ]
        ++ " ||| "
        ++ unwords
           [ case x of
               NT i -> let j = xs !! i
                       in "[" ++ show (e `deref` j) ++ "," ++ show (j + 1) ++ "]"
               T i -> if i < 0 then "" else TS.unpack $ getString ea i
           | x <- h1 l
           ]
        ++ " ||| "
        ++ show (w (ident e))


front :: T.Tree NTT -> [NTT]
front T.Nullary{ .. } = [rootLabel]
front T.Unary{ sub1 = T.Nullary{ rootLabel = T (-1) }, .. } = [rootLabel]
front t = concatMap front $ T.subForest t


prettyPrintJoshua
  :: TokenArray
  -> TokenArray
  -> TokenArray
  -> IRTG Int
  -> V.Vector Double
  -> Hyperedge StrictIntPair Int
  -> T.Text
prettyPrintJoshua ea fa na IRTG{ .. } w e
  = prettyPrintJoshua' ea fa na
      (front . (h1 V.!) . _fst)
      (V.toList . (h2 V.!) . _snd)
      (\ i -> if i < 0 then 1.0 else w V.! i) e

{-
makeIRTG
  :: ( ( TokenMap
       , TokenMap
       , TokenMap
       , (M.Map (T.Tree NTT) Int, Int)
       , (M.Map [NTT] Int, Int)
       )
     , [(Hyperedge StrictIntPair Int, Double)]
     )
  -> (IRTG Int, [Double], TokenMap, TokenMap, TokenMap)
makeIRTG ((em, fm, nm, (tm, tmc), (sm, smc)), ews)
  = let ws = S.toList $ S.fromList $ map snd ews
        wmap = M.fromList $ zip ws [(0 :: Int) ..]
        es = [ mapHEi (const (wmap M.! w)) e
             | (e, w) <- ews
             ]
        rtg = mkHypergraph es
        h1 = V.fromList $ A.elems $ A.array (0, tmc - 1)
           $ map swap $ M.toList tm
        h2 = V.fromList $ A.elems $ A.array (0, smc - 1)
           $ map swap $ M.toList sm
        initial = getToken em (T.pack "ROOT")
    in (IRTG { .. }, ws, em, fm, nm)
-}
