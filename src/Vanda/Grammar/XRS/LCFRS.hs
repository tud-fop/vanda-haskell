module Vanda.Grammar.XRS.LCFRS where

{-

LCFRS look okay.
Rule extraction works and look pretty probabilistic to me.
No Binarization yet.

Ideas/TODOs: have NTs know their fanout. I'd need my own NTT-Type for that or I'd keep it in an extra map...
             Although I still haven't reached the point where that is really necessary :O


-}
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
import qualified Data.Vector as V
import qualified Data.Text.Lazy.IO as TIO
import           Text.Printf (printf)

import           Data.NTT
import           Vanda.Hypergraph.IntHypergraph
import qualified Vanda.Hypergraph.Tree as VT
import           Vanda.Corpus.Negra
import           Vanda.Corpus.Negra.Text (parseNegra)
import           Vanda.Util.Memorysavers


-- DATA STRUCTURES

type Rule = ((Int, [Int]), [[NTT]])

-- The weight vector seems to be adressed with i.
-- The label adresses the homomorphisms. Both refer to rules, obviously.
data MIRTG -- Mono-IRTG! I should not be allowed to name things.
  = MIRTG
    { rtg :: Hypergraph Int Int -- SIP divided by two gives us... Int I guess?
    , initial :: [Int] -- this one is probably a node of the hypergraph, so a NT
    , h :: V.Vector (V.Vector (V.Vector NTT))
        -- Outer vector lets me map rules (using their Int-label or one component of these if we use SIPs) to the
        -- Middle vector, which represents the components of the lhs-NT, its length is that NTs fan-out
        -- Inner vector represents the concatenation of Ts and variables (linearly adressable, thus Int)
        -- The NTTs Ts are indeed the Ts, the NTs however are the variables (zero-indexed)
    }

data MXRS
  = MXRS
    { irtg :: MIRTG
    , weights :: [Double]
    }

instance Show MXRS where
  show (MXRS (MIRTG hg _ h') w)
    = unlines
    . map (\ he -> (cut 2 . show . to $ he)
                ++ " <- "
                ++ (cut 10 . show . from $ he)
                ++ " # "
                ++ (cut 5 . show . (!!) w . ident $ he)
                ++ " || "
                ++ (show . (V.!) h' . label $ he)
          )
    . edges
    $ hg

cut :: Int -> [Char] -> [Char]
cut n = take n . (++ repeat ' ')

-- FUNCTIONS

translateNTTString
  :: (A.Array Int String) -- ^ NTs
  -> (A.Array Int String) -- ^ Ts
  -> NTT
  -> String
translateNTTString a_nt _ (NT i) = a_nt A.! i
translateNTTString _ a_t (T i) = a_t A.! i

sententialFront
  :: MIRTG
  -> (A.Array Int String) -- ^ NTs
  -> (A.Array Int String) -- ^ Ts
  -> Derivation Int Int
  -> V.Vector String -- a sentence (terminal symbol sequence)
sententialFront (MIRTG hg _ h') a_nt a_t dt
  -- TODO check first rules lhs for =init and check fan-out of this start symbol =1
  | not $ and $ fmap (`elem` (edges hg)) dt
    = error "Not all rules are in the IRTG!"
  | otherwise
    = fmap (translateNTTString a_nt a_t) $ join $ getSpans h' dt

getSpans
  :: (Integral i)
  => V.Vector (V.Vector (V.Vector NTT)) -- ^ homomorphism, see above def.
  -> Derivation Int i -- ^ hyperedge tree
  -> V.Vector (V.Vector NTT) -- ^ Tuple of terminal vectors, should only contain Ts
getSpans h' dt = fmap (join . fmap evalNT) preResult
  where
    he = VT.rootLabel dt
    preResult = (V.!) h' . label $ he -- This already has the result type, but contains variables
    childSpans = V.concat $ map (getSpans h') $ VT.subForest dt -- V.Vector (V.Vector NTT)
    evalNT t@(T _) = V.singleton t
    evalNT (NT x) = childSpans V.! x

-- RESULTS \o/

main :: IO ()
main = do
       corpusText' <- TIO.readFile "/home/sjm/programming/LCFRS/tiger_release_aug07.export"
       
       let sentences' = take 3000
                      $ sentences . parseNegra
                      $ corpusText'
       
       let sentenceTrees = map (establishSpanBasedOrderAndAnnotateFanOut . negraToCrossedTree . sData) sentences'
       
       let (intifiedTrees, (m_nt, m_t)) = dualIntifyNegra sentenceTrees
       
       -- {-
       let ruleMap = readoffAll intifiedTrees
       
       let a_nt = invertMap m_nt
       let a_t = invertMap m_t
       ruleMap `deepseq` m_nt `deepseq` m_t `deepseq` return ()
       putStrLn "all deepseqed"
       
       writeFile "/tmp/a_nt" (show a_nt)
       writeFile "/tmp/a_t" (show a_t)
       writeFile "/tmp/rules" (show ruleMap)
       -- -}
       
       {-
       a_nt <- fmap read $ readFile "/tmp/a_nt" :: IO (A.Array Int String)
       a_t <- fmap read $ readFile "/tmp/a_t" :: IO (A.Array Int String)
       ruleMap <- fmap read $ readFile "/tmp/rules" :: IO (M.Map Int (M.Map Rule Int))
       putStrLn "opened files"
       -- -}
       
       putStrLn "\nOverall rule counts:\n"
       
       let flatRuleMap = M.foldl' M.union M.empty ruleMap
       
       print $ M.size flatRuleMap
       print $ M.foldl' (+) 0 flatRuleMap
       
       putStrLn "\nFrequently extracted rules:\n"
       
       printBestIn a_nt a_t show flatRuleMap
       
       putStrLn "\nBest probabilities for epsilon = 0, np = 7:\n"
       
       let pRuleMap = M.map normalize ruleMap
       
       -- epsilon = 0, np = 7
       printBestIn a_nt a_t (printf "%.4f") $ fromJust $ M.lookup 0 pRuleMap
       printBestIn a_nt a_t (printf "%.4f") $ fromJust $ M.lookup 7 pRuleMap
  where
    printBestIn a_nt a_t s = mapM_ (\(r, c) -> putStrLn $ cut 8 (s c) ++ retranslateRule a_nt a_t r)
                           . take 20
                           . reverse
                           . sortBy (comparing snd)
                           . M.assocs
    
    normalize :: M.Map a Int -> M.Map a Double
    normalize m = let gamma = fromIntegral $ M.foldl' (+) 0 m
                  in M.map ((/gamma) . fromIntegral) m
    
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
                     Just v  -> return v
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
    
    retranslateRule
      :: (A.Array Int String)
      -> (A.Array Int String)
      -> Rule -- ^ a rule
      -> String
    retranslateRule a_nt a_t ((lhs, rhs), hom_f)
      =  (cut 6 $ (A.!) a_nt lhs)
      ++ " -> "
      ++ (cut 100 $ show $ map ((A.!) a_nt) rhs)
      ++ " // "
      ++ (show $ map (map retHomComponent) hom_f)
        where
          retHomComponent (T t) = (A.!) a_t t
          retHomComponent (NT v) = show v -- Remember, these aren't real NTs, but variables for the reordering
    
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
