module Vanda.Grammar.XRS.GHKM
  ( Alignment (..)
  , Rule (..)
  , extractRules
  ) where

import Prelude hiding ( span )

import Control.Arrow ( second )
import Control.Seq
import Control.Monad.State.Strict ( State, evalState, runState, get, put )

import Data.Function ( on )
import qualified Data.IntMap as IM
import Data.List ( foldl', sortBy, unzip4 )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Data.Vector as V

import Vanda.Hypergraph hiding ( label )
import Vanda.Util

-- | a parse tree
-- each node carries its span and span closure,
-- as well as string position (for terminal nodes)
data TreeNode l
  = TreeNode
    { label :: l
    , span :: S.Set Int
    , clos :: Maybe (Int, Int)
    , idx :: Int
    }
  deriving Show

data TreeNode2 l
  = TreeNode2
    { treeNode :: TreeNode l
    , complSpan :: S.Set Int
    , frontier :: Bool
    }
  deriving Show

data Alignment l
  = Alignment
    { almap :: M.Map Int (S.Set Int)  -- map an Engl. index to Fr. indices
    , snt :: V.Vector l
    }
  deriving Show


type Sync l = (T.Tree (Either Int l), [Either Int l])

newtype Rule v l = Rule { unRule :: Hyperedge v (Sync l) Int }


seqTree :: Strategy l -> Strategy (T.Tree l)
seqTree sl (T.Node l ts) = sl l `seq` seqList (seqTree sl) ts `seq` ()


-- | Depth-first left-to-right traversal of a tree (via the State monad),
-- adding the spans, span closures, and index information to each node
{-process :: Alignment l -> T.Tree l -> T.Tree (TreeNode l)
process = curry $ (`evalState` 0) . uncurry p-}
process :: Alignment l -> T.Tree l -> (Alignment l, T.Tree (TreeNode l))
process al t = (al, evalState (p t) 0)
  where
    p :: {- Alignment l -> -} T.Tree l -> State Int (T.Tree (TreeNode l))
    p (T.Node l ts)
      | null ts   = do
                      i <- get
                      put (i+1)
                      let sp = M.findWithDefault S.empty i (almap al)
                      return $ T.Node (TreeNode l sp (closure sp) i) []
      | otherwise = do
                      ts' <- mapM p ts
                      let sp = foldl' S.union S.empty
                             $ map (span . T.rootLabel) ts'
                      return $ T.Node (TreeNode l sp (closure sp) (-1)) ts'
    closure sp
      | S.null sp = Nothing
      | otherwise = Just (S.findMin sp, S.findMax sp)


-- | Recursive tree traversal to add complement span and frontier property
process2 :: T.Tree (TreeNode l) -> T.Tree (TreeNode2 l)
process2 = {-mytrace .-} p S.empty [] []
  where
    p :: S.Set Int    -- parent complement span
      -> [T.Tree (TreeNode l)] -- left siblings
      -> [T.Tree (TreeNode l)] -- right siblings
      -> T.Tree (TreeNode l)   -- tree
      -> T.Tree (TreeNode2 l)
    p par ls rs (T.Node tn ts)
      = T.Node
          (TreeNode2 tn cs (S.null (span tn `S.intersection` cs)))
          (q cs ts [])
        where
          cs = par `un` ls `un` rs
          un = foldl' (\ sp t -> sp `S.union` span (T.rootLabel t))
          -- (foldl' S.union par ls) rs
    q :: S.Set Int
      -> [T.Tree (TreeNode l)]
      -> [T.Tree (TreeNode l)]
      -> [T.Tree (TreeNode2 l)]
    q _ [] _ = []
    q par (t:ts) ts' = p par ts ts' t : q par ts (t:ts')


process3
  :: Alignment l
  -> Bool                      -- are we at the root?
  -> T.Tree (TreeNode2 l)
  -> State Int
      ( T.Tree (Either Int l)  -- left-hand side
      , [(Int, Maybe (Int, Int))]  -- rhs variables with span closure
      , IM.IntMap l            -- state assignment
      , [T.Tree (TreeNode2 l)] -- remaining trees
      )
process3 al atroot t@(T.Node tn2 ts)
  | not atroot && frontier tn2 = do
      i <- get
      put (i + 1)
      return ( T.Node (Left i) []
             , [(i, clos (treeNode tn2))]
             , IM.singleton i (label (treeNode tn2))
             , [t]
             )
  | otherwise = do
      (lhss, rhss, stmaps, rs) <- fmap unzip4 $ mapM (process3 al False) ts
      return $ ( T.Node (Right (label (treeNode tn2))) lhss 
               , concat rhss
               , foldl' IM.union IM.empty stmaps
               , concat rs
               )


process3'
  :: Alignment l
  -> T.Tree (TreeNode2 l)
  -> [(Int, Maybe (Int, Int))]
  -> [Either Int l]
process3' al (T.Node tn2 _)
  = case clos (treeNode tn2) of
      Nothing -> map (Left . fst)
      Just (lspan, rspan)
        -> go lspan
           . (++ [(-1, Just (rspan+1, undefined))]) -- add sentinel element
  where
    sp = span (treeNode tn2)
    go lspan ((i, Nothing) : xs)
      = Left i : go lspan xs
    go lspan ((-1, Just (rspan, _)) : _)
      = [ Right (snt al V.! j) | j <- [lspan..rspan-1], j `S.member` sp ]
    go lspan ((i, Just (rspan, rspan')) : xs)
      = [ Right (snt al V.! j) | j <- [lspan..rspan-1], j `S.member` sp ]
        -- ++ if i /= -1 then [ Left i ] else []
        ++ [ Left i ]
        ++ go (rspan' + 1) xs
    go _ [] = error "cannot happen because of sentinel element (see above)"


process4 :: [(Alignment l, T.Tree (TreeNode2 l))] -> [Rule l l]
process4 [] = []
process4 ((al, t) : ats)
  = case runState (process3 al True t) 0 of
      ((lhs, rhslist, stmap, rs'), n) ->
        let rhs = (process3' al t $ sortBy (compare `on` snd) rhslist)
                  `using` seqList (seqEither rseq rseq)
            e = mkHyperedge
                  (label (treeNode (T.rootLabel t)))
                  (map (stmap IM.!) [(0 :: Int) .. n - 1]
                    `using` seqList rseq)
                  (lhs, rhs)
                  0
            rs = process4 $ zip (repeat al) rs'
            rs2 = process4 ats
        in using lhs (seqTree rseq) `seq` e `seq` (Rule e : rs ++ rs2)


extractRules :: [(Alignment l, T.Tree l)] -> [Rule l l]
extractRules = process4 . map (second process2 . uncurry process)

