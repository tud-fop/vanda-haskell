-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CBSM.CRTG
-- Copyright   :  (c) Technische Universität Dresden 2014–2017
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Vanda.CBSM.CRTG
( Rule(..)
, Count
, CRTG(..)
, rules
, forestToGrammar
, forestToGrammar'
, prettyPrintCRTG
, toHypergraph
, asBackwardStar
, bests
, ForwardStar
, forwardStar
, BidiStar
, bidiStar
) where


import           Control.Applicative ((<*>), (<$>))
import           Control.DeepSeq (NFData(rnf))
import qualified Data.Binary as B
import           Data.Coerce (coerce)
import           Data.List (intercalate, transpose)
import           Data.List.Extra (mergeListsBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Ord (comparing, Down(..))
import qualified Data.Set as S
import           Data.Tree
import           Data.Tuple (swap)
import qualified Data.Vector as V
import           Numeric.Log (Log(..))

import qualified Vanda.Features as F
import qualified Vanda.Hypergraph as H
import           Vanda.Util.PrettyPrint (columnize)
import           Vanda.Util.Tree as T


-- | 'CRTG' rules. The field names are inspired by 'H.Hyperedge'.
data Rule s t = Rule
  { to    :: !s
  -- ^ left-hand side
  , from  :: ![s]
  -- ^ right-hand side
  , label :: !t
  -- ^ terminal symbol
  } deriving (Eq, Ord)

instance (Show s, Show t) => Show (Rule s t) where
  show Rule{..} = "Rule " ++ show to ++ " " ++ show from ++ " " ++ show label

instance (B.Binary v, B.Binary l) => B.Binary (Rule v l) where
  put (Rule x y z) = B.put x >> B.put y >> B.put z
  get = Rule <$> B.get <*> B.get <*> B.get

instance (NFData s, NFData t) => NFData (Rule s t) where
  rnf Rule{..} = rnf to `seq` rnf from `seq` rnf label


type Count = Double


-- | Count RTG – A regular tree grammar with associated counts, e.g., w.r.t.
-- a corpus.
data CRTG v l = CRTG
  { cntRule  :: !(Map (Rule v l) Count)
  -- ^ counts of the rules
  , cntState :: !(Map v Count)
  -- ^ counts of the states
  , cntInit  :: !(Map v Count)
  -- ^ counts of the initial states
  } deriving Show

instance (B.Binary v, B.Binary l) => B.Binary (CRTG v l) where
  put (CRTG x y z) = B.put x >> B.put y >> B.put z
  get = CRTG <$> B.get <*> B.get <*> B.get

instance (NFData l, NFData v) => NFData (CRTG v l) where
  rnf CRTG{..} = rnf cntState `seq` rnf cntInit `seq` rnf cntRule


rules :: CRTG v l -> [Rule v l]
rules = M.keys . cntRule


-- | A type for fast lookup of 'Rule's with a specific state in 'from' and a
-- specific 'label'.
type ForwardStar v l = Map v (Map l [Rule v l])


forwardStar :: (Ord v, Ord l) => [Rule v l] -> ForwardStar v l
forwardStar
  = fmap (M.fromListWith (++)) . M.fromListWith (++) . concatMap step
  where
    step r@(Rule _ vs l)
      = map (\ v -> (v, [(l, [r])]))
      $ (S.toList . S.fromList) vs


-- | Bidirectional Star: A type for fast lookup of 'Rule's that use a specific
-- state (i.e., the state is equal to 'to' or contained in 'from').
type BidiStar v l = Map v [Rule v l]


bidiStar :: Ord v => [Rule v l] -> BidiStar v l
bidiStar = M.fromListWith (++) . concatMap step
  where
    step r@(Rule v vs _)
      = map (\ v' -> (v', [r]))
      $ (S.toList . S.fromList) (v : vs)


{-
fromList :: (Ord s, Ord t) => [Rule s t] -> RTG s t
fromList = unions . concatMap step
  where
    step r@(Rule v vs l _) = singletonBW v l r : map (\ v' -> singletonFW v' l r) vs
    singletonBW v l r = M.singleton v $ M.singleton l (S.singleton r :-> S.empty)
    singletonFW v l r = M.singleton v $ M.singleton l (S.empty :-> S.singleton r)
    union = M.unionWith (M.unionWith unionRuleSets)
    unions = foldl' union M.empty
-}
{-
unionRuleSets
  :: (Ord l, Ord v) => RuleSets v l -> RuleSets v l -> RuleSets v l
unionRuleSets (bw1 :-> fw1) (bw2 :-> fw2)
  = (S.union bw1 bw2 :-> S.union fw1 fw2)
-}

prettyPrintCRTG :: Ord v => (v -> String) -> (l -> String) -> CRTG v l -> String
prettyPrintCRTG showV showL CRTG{..}
  = unlines
      [ columnize ["  "]
        $ transpose
        $ (["state", "count"] :)
        $ map (\ (v, c) -> [showV v, show c])
        $ M.assocs cntState
      , columnize ["  "]
        $ transpose
        $ (["initial", "count", "probability", "log₂ probability"] :)
        $ map (\ (v, c) -> let s = sum (M.elems cntInit) in
            [ showV v
            , show c
            , show (c / s)
            , show $ ln (Exp (log c) / Exp (log s) :: Log Double)
                      / log 2
            ])
        $ M.assocs cntInit
      , columnize [" -> ", " ", "  #  ", "  ", "  "]
        $ transpose
        $ ( [ "state"
            , "terminal"
            , "states"
            , "count"
            , "probability"
            , "log₂ probability"
            ] : )
        $ map (\ (Rule{..}, cR) -> let cTo = cntState M.! to in
            [ showV to
            , showL label
            , "[" ++ intercalate ", " (map showV from) ++ "]"
            , show cR
            , show (cR / cTo)
            , show $ ln (Exp (log cR) / Exp (log cTo) :: Log Double)
                      / log 2
            ])
        $ M.assocs cntRule
      ]


-- | Convert a 'CRTG' into a 'H.Hypergraph' and a 'M.Map' of initial weights.
-- The 'H.ident's of the 'H.Hyperedge's are abused to store a weight. The
-- weights are probabilities, namely the normalized counts of the 'CRTG'.
toHypergraph
  :: (H.Hypergraph h, Ord v) => CRTG v l -> (h v l Double, Map v Double)
  -- not the most general type: Double is specific
toHypergraph CRTG{..}
  = ( H.mkHypergraph
      $ map (\ (Rule{..}, count) -> (H.mkHyperedge to from label
                       (count / (cntState M.! to))))
      $ M.toList cntRule
    , M.map (/ sum (M.elems cntInit))
            cntInit
    )


-- | List the derivations of a 'CRTG' together with their probabilities in
-- descending order w.r.t. the probabilities.
bests :: (Ord v, Eq l) => CRTG v l -> [(Double, H.Derivation v l Double)]
bests g
  = mergeListsBy (comparing (Down . fst))
  $ M.elems
  $ M.intersectionWith (\ w' -> map (\ (F.Candidate w d _) -> (w' * w, d))) ini
--   $ M.map (map (\ (F.Candidate w d _) -> (w, d)))
  $ H.bests (asBackwardStar hg) feature (V.singleton 1)
  where
    (hg, ini) = toHypergraph g
    feature = F.Feature (\ _ i xs -> i * product xs) V.singleton


-- | Helper to prevent ambiguous type of a 'H.Hypergraph'.
asBackwardStar :: H.BackwardStar v l i -> H.BackwardStar v l i
asBackwardStar = id


{-
data ShowTree a
  = a :< [ShowTree a]
  | L a
  deriving (Eq, Ord, Show)

showTree   (x `Node` []) = L x
showTree   (x `Node` ts) = x :<     map showTree   ts
unshowTree (L x        ) = x `Node` []
unshowTree (x :<     ts) = x `Node` map unshowTree ts
-}


{-
stripQuotes :: String -> String
stripQuotes cs@[_]                           = cs
stripQuotes cs@('"' : cs') | last cs' == '"' = init cs'
stripQuotes cs                               = cs


data Term a = Lit a
            | a :++ a
            | Term a :+ Term a
            | Term a :* Term a deriving (Read, Show)
infixl 6 :+
infixl 7 :*
infixl 5 :++

x1, x2 :: Term Int
x1 = Lit 1 :+ Lit 2 :* Lit 3
x2 = Lit 4 :* Lit 5 :+ Lit 6
x3 = OrdTree $ Node x1 [Node x2 [], Node x1 [Node x2 []]]
x4 = OrdTree $ Node x1 []
x5 = x4 :++ x4


instance Read a => Read (OrdTree a) where
  readsPrec d = readParen False $ \ cs0 ->
      [ (OrdTree (Node x (map unpack ts)), cs2)
      | (x , cs1) <- readsPrec d cs0
      , (ts, cs2) <- case lex cs1 of
                      ("(", _) : _ -> readsPrec 11 cs1
                      ("[", _) : _ -> readsPrec 11 cs1
                      _ ->  [([], cs1)]
      ]
    where unpack (OrdTree t) = t
-}

-- | Same as 'forestToGrammar'' assuming 'Count' @1@ for every 'Tree'.
forestToGrammar
  :: Ord l
  => [Tree l]
  -> (CRTG Int l, Map Int (Tree l))
forestToGrammar = forestToGrammar' . map (\ t -> (t, 1))


-- | Create canonical 'CRTG' from a 'Tree' corpus, i.e., a 'CRTG' that
-- generates exactly the corpus. The states of the 'CRTG' correspond to the
-- subtrees in the corpus; this correspondence is returned in a 'Map'.
forestToGrammar'
  :: Ord l
  => [(Tree l, Count)]
  -> (CRTG Int l, Map Int (Tree l))
forestToGrammar' corpus
  = ( CRTG
        (M.mapKeys toRule cntTrees)
        (M.mapKeysMonotonic (ints M.!) cntTrees)
        (M.mapKeysMonotonic (ints M.!) $ M.fromListWith (+) $ coerce corpus)
    , M.map unOrdTree $ M.fromAscList $ map swap $ M.toAscList $ ints
    )
  where
    cntTrees
      = M.fromListWith (+)
      $ concatMap
          (\ (t, c) -> map (\ t' -> (OrdTree t', c)) $ T.subTrees t)
          corpus
    ints = snd $ M.mapAccum (\ i _ -> (i + 1, i)) 0 $ cntTrees
    toRule t@(OrdTree (Node x ts))
      = Rule (ints M.! t) (map ((ints M.!) . OrdTree) ts) x
