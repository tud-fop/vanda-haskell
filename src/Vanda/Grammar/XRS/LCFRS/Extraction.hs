-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Sebastian Mielke 2015
-- License     :  BSD-style
--
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.Grammar.XRS.LCFRS.Extraction
( extractPLCFRSFromNegra
) where

import           Control.DeepSeq (deepseq)
import           Control.Monad.Trans.State.Lazy (State, get, put, runState)
import           Control.Parallel.Strategies
import qualified Data.Foldable as F
import           Data.List (sortBy, findIndex)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Traversable as TR
import qualified Data.Tree as T

import           Data.NTT
import           Vanda.Corpus.Negra
import           Vanda.Util.Memorysavers

import           Vanda.Grammar.XRS.LCFRS

getNTName :: Maybe SentenceData -> String
getNTName Nothing
  = "ε"
getNTName (Just spart)
  = sdPostag spart

readoffAll
  :: (F.Foldable f)
  => f (T.Tree (NTIdent, Maybe TIdent, [Span]))
  -> (S.Set NTIdent, M.Map NTIdent (M.Map Rule Int))
  -- ^ Set of initial NTs and map from NTs to rules and their counts
readoffAll ts = F.foldl' rootworker (S.empty, M.empty) ts
  where
    rootworker
      :: (S.Set NTIdent, M.Map NTIdent (M.Map Rule Int))
      -> (T.Tree (NTIdent, Maybe TIdent, [Span]))
      -> (S.Set NTIdent, M.Map NTIdent (M.Map Rule Int))
    rootworker (s, m) root
      = (S.insert ((\(x,_,_) -> x) $ T.rootLabel root) s, worker m root)
    worker
      :: M.Map NTIdent (M.Map Rule Int)
      -> (T.Tree (NTIdent, Maybe TIdent, [Span]))
      -> M.Map NTIdent (M.Map Rule Int)
    worker m root
      = let newrule@((lhs, _), _) = readoffNode root
            innerInserter _ oldmap = M.insertWith (+) newrule 1 oldmap
            rootmap = M.insertWith innerInserter lhs (M.singleton newrule 1) m
        in F.foldl' worker rootmap (T.subForest root)

readoffNode
  :: (T.Tree (NTIdent, Maybe TIdent, [Span]))
  -- ^ the first Int is the POS-tag, the second can be a terminal word
  -> Rule
readoffNode (T.Node (tag, Nothing, rootspans) cs)
  = ( (tag, map ((\(x,_,_)->x) . T.rootLabel) cs)
    , map (map nt . solveSpanPuzzle allChildSpans) rootspans
    )
    where
      allChildSpans = concatMap ((\(_,_,x)->x) . T.rootLabel) cs
      solveSpanPuzzle :: [Span] -> Span -> [Int]
      solveSpanPuzzle spans (start, end)
        = if start-1 == end
          then [] -- All done!
          else let index = fromJust $ findIndex (\(s,_) -> s == start) spans
               in index : solveSpanPuzzle spans (1 + snd (spans !! index), end)
readoffNode (T.Node (tag, Just word, [(s1, s2)]) cs)
  = if not $ null cs then error "readoff: a word has children" else
    if s1 /= s2 then error "readoff: word has a wider span than 1" else
    ( (tag, [])
    , [[tt word]]
    )
readoffNode _ = error "readoff: malformed tree"

-- Intify NTs (POS-tags in SentenceNodes) and Ts (taken from SentenceWords)
-- simultaneously returning two separate maps.
-- Largely taken from from Memorysavers.hs
dualIntifyNegra
  :: [T.Tree (Maybe SentenceData, [Span])]
  -> ([T.Tree (NTIdent, Maybe TIdent, [Span])], (M.Map String NTIdent, M.Map String TIdent))
dualIntifyNegra t = withStrategy (evalTuple2 rdeepseq r0) $
                    runState (mapM dualIntifyNegraAction t) (M.empty, M.empty)

dualIntifyNegraAction
  :: T.Tree (Maybe SentenceData, [Span])
  -> State (M.Map String NTIdent, M.Map String TIdent)
           (T.Tree (NTIdent, Maybe TIdent, [Span]))
dualIntifyNegraAction = TR.mapM worker
  where
    worker :: (Maybe SentenceData, [Span])
           -> State (M.Map String NTIdent, M.Map String TIdent)
                    (NTIdent, Maybe TIdent, [Span])
    worker (Just SentenceWord{sdPostag = tag, sdWord = word}, spans)
      = do
        (m_nt, m_t) <- get
        let (m_nt', v_nt) =
               case M.lookup tag m_nt of
                 Just v  -> (m_nt, v)
                 Nothing -> let v = M.size m_nt
                            in (M.insert tag v m_nt, v)
        v_t <- case M.lookup word m_t of
                 Just v  -> put (m_nt', m_t) >> return v
                 Nothing -> let v = M.size m_t
                            in put (m_nt', M.insert word v m_t) >> return v
        return (v_nt, Just v_t, spans)
    worker (snode, spans)
      = do
        let tag = getNTName snode
        (m_nt, m_t) <- get
        case M.lookup tag m_nt of
          Just v  -> return (v, Nothing, spans)
          Nothing -> let v = M.size m_nt
                     in do put (M.insert tag v m_nt, m_t)
                           return (v, Nothing, spans)

establishSpanBasedOrderAndAnnotateFanOut
  :: T.Tree (Maybe SentenceData, [Span])
  -> T.Tree (Maybe SentenceData, [Span])
establishSpanBasedOrderAndAnnotateFanOut (T.Node (v, spans) cs)
  = T.Node (giveFanOut (length spans) v, spans)
  $ map establishSpanBasedOrderAndAnnotateFanOut
  $ sortBy (comparing earliestSpan) cs
    where earliestSpan = minimum -- earliest span start
                       . map fst -- span starts
                       . snd -- span list
                       . T.rootLabel

giveFanOut :: Int -> Maybe SentenceData -> Maybe SentenceData
giveFanOut i (Just sd)
  = let oldTag = sdPostag sd
    in Just sd{sdPostag = oldTag ++ "_" ++ show i}
giveFanOut _ v = v -- epsilon super-root

extractPLCFRSFromNegra :: Bool -> Negra -> PLCFRS
extractPLCFRSFromNegra removeTerminals negra
  = let (intifiedTrees, (m_nt, m_t))
          = dualIntifyNegra
          $ map ( establishSpanBasedOrderAndAnnotateFanOut
                . (if removeTerminals then removeLeaves else id)
                . negraToCrossedTree
                . sData
                )
          $ sentences
          $ negra
        (initialsSet, countRuleMap) = readoffAll intifiedTrees
        a_nt = invertMap m_nt
        a_t = invertMap m_t
        rulesAndProbs = M.assocs
                      $ M.foldl' M.union M.empty
                      $ normalizeRuleProbs countRuleMap
    in rulesAndProbs `deepseq` a_nt `deepseq` a_t `deepseq`
       (S.toList initialsSet, rulesAndProbs, (a_nt, a_t))
       -- test whether epsilon really only appears once per sentence (as root):
       -- trace (show $ M.foldl' (+) (0::Int) $ fromJust $ M.lookup 0 (readoffAll intifiedTrees))

normalize :: M.Map a Int -> M.Map a Double
normalize m = let gamma = fromIntegral $ M.foldl' (+) 0 m
              in M.map ((/gamma) . fromIntegral) m

normalizeRuleProbs :: M.Map b (M.Map a Int) -> M.Map b (M.Map a Double)
normalizeRuleProbs = M.map normalize
