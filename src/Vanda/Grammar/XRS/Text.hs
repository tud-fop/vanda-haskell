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

module Vanda.Grammar.XRS.Text where

import Control.Applicative ( many )
import Control.DeepSeq ( NFData )
import Control.Monad ( liftM2 )
import Data.IntMap as IM
import Data.Map as M
import Data.NTT
import qualified Data.Text.Lazy as T
import Text.Parsec hiding ( many )
import Text.Parsec.Text.Lazy

import Vanda.Grammar.XRS.IRTG
import Vanda.Hypergraph.IntHypergraph
import Vanda.Hypergraph.NFData ()
import qualified Vanda.Hypergraph.Tree as T
import Vanda.Util

-- import Debug.Trace ( trace, traceShow )

type GenMapper u s = u -> s -> (u, Int)

type Mapper u = GenMapper u T.Text

{- instance Ord l => Ord (T.Tree l) where
  T.Node l1 ts1 `compare` T.Node l2 ts2
    = case (l1 `compare` l2, ts1 `compare` ts2) of
        (LT, _) -> LT
        (EQ, LT) -> LT
        (EQ, EQ) -> EQ
        _ -> GT
-}

parseXRSMap
  :: (NFData ue, NFData uf, NFData un)
  => Mapper ue   -- ^ English symbol mapping function, e.g. 'updateToken'
  -> ue          -- ^ initial token structure
  -> Mapper uf   -- ^ French symbol mapping function, e.g. 'updateToken'             
  -> uf          -- ^ initial structure
  -> Mapper un   -- ^ node mapping function, e.g. 'updateToken'
  -> un          -- ^ initial token structure
  -> T.Text      -- ^ grammar file
  -> ( (ue, uf, un, (M.Map (T.Tree NTT) Int, Int), (M.Map [NTT] Int, Int))
     , [(Hyperedge StrictIntPair Int, Double)]
     )           -- ^ resulting token structure and hyperedges
parseXRSMap me ue0 mf uf0 mn un0 gr
  = lazyMany (p_grammar me' mf' mn' mt' ms') "xrs" u0 ({-tail $-} T.lines gr)
  where
    me' = (p_map $ liftMapper me in_15 out_15) . T.pack
    mf' = (p_map $ liftMapper mf in_25 out_25) . T.pack
    mn' = (p_map $ liftMapper mn in_35 out_35) . T.pack
    mt' = p_map $ liftMapper mapit in_45 out_45
    ms' = p_map $ liftMapper mapit in_55 out_55
    u0 = (ue0, uf0, un0, (M.empty, 0), (M.empty, 0))
    in_15 (x, _, _, _, _) = x
    in_25 (_, y, _, _, _) = y
    in_35 (_, _, z, _, _) = z
    in_45 (_, _, _, a, _) = a
    in_55 (_, _, _, _, b) = b
    out_15 (_, y, z, a, b) x = (x, y, z, a, b)
    out_25 (x, _, z, a, b) y = (x, y, z, a, b)
    out_35 (x, y, _, a, b) z = (x, y, z, a, b)
    out_45 (x, y, z, _, b) a = (x, y, z, a, b)
    out_55 (x, y, z, a, _) b = (x, y, z, a, b)

lazyMany :: GenParser u a -> SourceName -> u -> [T.Text] -> (u, [a])
lazyMany parser _ ustate contents
  = go ustate $ zip [(0 :: Int) ..] contents
  where
    go !u [] = (u, [])
    go !u ((i, x) : xs) =
      case runParser (liftM2 (,) parser getState) u ("line " ++ show i) x of
        Right (x', u') -> let (u'', xs') = go u' xs
                          in (u'', x' : xs')
        Left ed -> error $ show ed

{-
lazyMany :: GenParser u a -> SourceName -> u -> [T.Text] -> (u, [a])
lazyMany p file ustate contents
  = case contents of
      [] -> ustate either (error . show) id $ runParser mp ustate file contents
  where
    mp = do
      xs <- many p
      u <- getState
      return $! (u, xs)
-}

liftMapper :: GenMapper u s -> (u' -> u) -> (u' -> u -> u') -> GenMapper u' s
liftMapper m in_ out_ u' s = first' (out_ u') $ m (in_ u') s

type TheType t = (M.Map t Int, Int)

mapit :: Ord t => TheType t -> t -> (TheType t, Int)
mapit u@(m, imax) t
  = case M.lookup t m of
      Nothing -> let imax' = imax + 1
                 in imax' `seq` ((M.insert t imax m, imax'), imax)
      Just i -> (u, i)


p_map :: GenMapper u s -> s -> GenParser u Int
p_map mapper !s = do
  (u', l) <- fmap (flip mapper s) getState
  setState u'
  return l

-- trace' x = traceShow x x

-- XXX I am using pme throughout because it would be too complicated to
-- also register root labels with pmn  ---  see {- ! -}
p_tree
  :: (String -> GenParser u Int)
  -> (String -> GenParser u Int)
  -> GenParser u (T.Tree NTT, IM.IntMap Int)
p_tree pme pmn = choice [ p_tterm pme, p_tvar pme{- ! -}, p_tnonterm pme pmn ]

p_tterm
  :: (String -> GenParser u Int)
  -> GenParser u (T.Tree NTT, IM.IntMap Int)
p_tterm pme = do
  _ <- char '"'
  si <- pme =<< many (noneOf "\"")
  _ <- char '"'
  spaces
  return (T.node (tt si) [], IM.empty)

p_tvar
  :: (String -> GenParser u Int)
  -> GenParser u (T.Tree NTT, IM.IntMap Int)
p_tvar pmn = do
  _ <- char 'x'
  i <- fmap (read . (: "")) $ oneOf "0123456789"
  _ <- char ':'
  si <- pmn =<< many (noneOf " )")
  spaces
  return (T.node (nt i) [], IM.singleton i si)

p_tnonterm
  :: (String -> GenParser u Int)
  -> (String -> GenParser u Int)
  -> GenParser u (T.Tree NTT, IM.IntMap Int)
p_tnonterm pme pmn = do
  si <- pme =<< many (noneOf " ()")
  _ <- char '('
  (ts, ms) <- fmap unzip $ many $ p_tree pme pmn
  _ <- char ')'
  spaces
  return (T.node (tt si) ts, Prelude.foldr IM.union IM.empty ms)


p_string
  :: (String -> GenParser u Int)
  -> GenParser u [NTT]
p_string pmf = many $ choice [ p_sterm pmf, p_svar ]

p_sterm :: (String -> GenParser u Int) -> GenParser u NTT
p_sterm pmf = do
  _ <- char '"'
  si <- pmf =<< many (noneOf "\"")
  _ <- char '"'
  spaces
  return (tt si)

p_svar :: GenParser u NTT
p_svar = do
  _ <- char 'x'
  i <- fmap (read . (: "")) $ oneOf "0123456789"
  spaces
  return (nt i)

p_grammar
  :: (String -> GenParser u Int)
  -> (String -> GenParser u Int)
  -> (String -> GenParser u Int)
  -> (T.Tree NTT -> GenParser u Int)
  -> ([NTT] -> GenParser u Int)
  -> GenParser u (Hyperedge StrictIntPair Int, Double)
p_grammar pme pmf _ {- ! -} pmt pms = do
  (t, m) <- p_tree pme pme {- ! -}
  case T.rootLabel t of
    T i -> do
      _ <- string "-> "
      si <- pms =<< p_string pmf
      _ <- string "||| "
      w <- p_weight
      ti <- pmt t
      return (mkHyperedge i{- ! -} (IM.elems m) (SIP ti si) 0, w)
    _ -> unexpected "terminal or variable at root"

p_weight :: GenParser u Double
p_weight
  = do
    { c <- oneOf "0123456789"
    ; cs <- many (oneOf "0123456789.E-")
    ; return $! read (c : cs)
    }
