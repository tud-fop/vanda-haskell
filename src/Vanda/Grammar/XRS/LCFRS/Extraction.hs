module Vanda.Grammar.XRS.LCFRS.Extraction where

import           Control.DeepSeq (deepseq)
import           Control.Monad.State.Lazy hiding (mapM)
import           Control.Parallel.Strategies
import qualified Data.Array as A
import qualified Data.Foldable as F
import           Data.List (sortBy, findIndex)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
import           Data.Ord (comparing)
import qualified Data.Traversable as TR
import qualified Data.Tree as T

import           Data.NTT
import           Vanda.Corpus.Negra
import           Vanda.Util.Memorysavers

import           Vanda.Grammar.XRS.LCFRS

import Debug.Trace

normalize :: M.Map a Int -> M.Map a Double
normalize m = let gamma = fromIntegral $ M.foldl' (+) 0 m
              in M.map ((/gamma) . fromIntegral) m

getNTName :: Maybe SentenceData -> String
getNTName Nothing
  = "ε"
getNTName (Just spart)
  = sdPostag spart

readoffAll
  :: (F.Foldable f)
  => f (T.Tree (Int, Maybe Int, [Span]))
  -> M.Map Int (M.Map Rule Int) -- ^ Mapping NTs (Int) to rules and their counts
readoffAll ts = F.foldl' worker M.empty ts
  where
    worker :: M.Map Int (M.Map Rule Int) -> (T.Tree (Int, Maybe Int, [Span])) -> M.Map Int (M.Map Rule Int)
    worker inmap root
      = let newrule@((lhs, _), _) = readoffNode root
            innerInserter _ oldmap = M.insertWith (+) newrule 1 oldmap
            rootmap = M.insertWith innerInserter lhs (M.singleton newrule 1) inmap
        in F.foldl' worker rootmap (T.subForest root)

readoffNode
  :: (T.Tree (Int, Maybe Int, [Span])) -- ^ the first Int is the POS-tag, the second can be a terminal word
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
  -> ([T.Tree (Int, Maybe Int, [Span])], (M.Map String Int, M.Map String Int))
dualIntifyNegra t = withStrategy (evalTuple2 rdeepseq r0) $
                    runState (mapM dualIntifyNegraAction t) (M.empty, M.empty)

dualIntifyNegraAction
  :: T.Tree (Maybe SentenceData, [Span])
  -> State (M.Map String Int, M.Map String Int) (T.Tree (Int, Maybe Int, [Span]))
dualIntifyNegraAction = TR.mapM worker
  where
    worker :: (Maybe SentenceData, [Span])
           -> State (M.Map String Int, M.Map String Int) (Int, Maybe Int, [Span])
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
                     in put (M.insert tag v m_nt, m_t) >> return (v, Nothing, spans)

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
giveFanOut i (Just sd) = let oldTag = sdPostag sd
                         in Just sd{sdPostag = oldTag ++ "_" ++ show i}
giveFanOut _ v = v -- epsilon super-root


extractRulesFromNegra
  :: Negra
  -> (M.Map Int (M.Map Rule Double), (A.Array Int String, A.Array Int String)) -- ^ map from each NT to a list of possible (intified) productions and their probability and a NT and T dictionary
extractRulesFromNegra negra =
  let (intifiedTrees, (m_nt, m_t)) = dualIntifyNegra
                                   $ map ( establishSpanBasedOrderAndAnnotateFanOut
                                         . negraToCrossedTree
                                         . sData)
                                   -- $ take 10
                                   $ sentences
                                   $ negra
      pRuleMap = M.map normalize $ readoffAll intifiedTrees
      a_nt = invertMap m_nt
      a_t = invertMap m_t
  in pRuleMap `deepseq` a_nt `deepseq` a_t `deepseq` (pRuleMap, (a_nt, a_t))
     -- to test whether epsilon really only apperas once per sentence (as root):
     -- trace (show $ M.foldl' (+) (0::Int) $ fromJust $ M.lookup 0 (readoffAll intifiedTrees))
