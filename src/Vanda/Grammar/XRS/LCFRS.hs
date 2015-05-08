module Vanda.Grammar.XRS.LCFRS where

{-

LCFRS look okay.
Rule extraction begins to work.
Nothing probabilistic yet.
No Binarization yet.

Ideas/TODOs: have NTs know their fanout. I'd need my own NTT-Type for that or I'd keep it in an extra map...


-}
import           Control.DeepSeq (deepseq)
import           Control.Monad.State.Lazy hiding (mapM)
import           Control.Parallel.Strategies
import qualified Data.Array as A
import           Data.List (sortBy, findIndex)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Traversable as TR
import qualified Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Text.Lazy.IO as TIO

import           Data.NTT
import           Vanda.Hypergraph.IntHypergraph
import qualified Vanda.Hypergraph.Tree as VT
import           Vanda.Corpus.Negra
import           Vanda.Corpus.Negra.Text (parseNegra)
import           Vanda.Util.Memorysavers


-- DATA STRUCTURES

-- Note: The ident type i should still be integral, since the weight vector seems to be adressed with i.
--       The label adresses the homomorphisms. Both refer to rules, obviously.
data MIRTG i -- Mono-IRTG! I should not be allowed to name things.
  = MIRTG
    { rtg :: Hypergraph Int i -- SIP divided by two gives us... Int I guess?
    , initial :: [Int] -- this one is probably a node of the hypergraph, so a NT
    , h :: V.Vector (V.Vector (V.Vector NTT))
        -- Outer vector lets me map rules (using their Int-label or one component of these if we use SIPs) to the
        -- Middle vector, which represents the components of the lhs-NT, its length is that NTs fan-out
        -- Inner vector represents the concatenation of Ts and variables (linearly adressable, thus Int)
        -- The NTTs Ts are indeed the Ts, the NTs however are the variables (zero-indexed)
    }

data MXRS
  = MXRS
    { irtg :: MIRTG Int
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
  :: (Integral i)
  => MIRTG i
  -> (A.Array Int String) -- ^ NTs
  -> (A.Array Int String) -- ^ Ts
  -> Derivation Int i
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
       {-
       putStrLn "\nExtracting a LCFRS from a real sentence:\n"
       corpusText <- TIO.readFile "/home/sjm/programming/LCFRS/tiger_release_aug07_shorttest.export"
       let firstSentenceTree = establishSpanBasedOrder
                             $ negraToCrossedTree . sData
                             $ head . sentences . parseNegra
                             $ corpusText
       putStrLn $ T.drawTree $ fmap getRep firstSentenceTree
       
       let ([intifiedTree], (m_nt, m_t)) = dualIntifyNegra [firstSentenceTree]
           a_nt = invertMap m_nt
           a_t = invertMap m_t
       
       putStrLn $ T.drawTree $ fmap show $ intifiedTree
       
       let rules = flattenSeeing readoff intifiedTree
       mapM_ print rules
       mapM_ putStrLn $ map (retranslateRule a_nt a_t) rules
       
       let myRules = rules
           myHyperedges = map (\(((lhs, rhs), _), i) -> mkHyperedge lhs rhs i i)
                        $ zip myRules [0..]
           myH = V.fromList $ map (V.fromList . map V.fromList . snd) myRules
           myMIRTG = MIRTG (mkHypergraph myHyperedges) [iSQ1] myH
           myLCFRS = MXRS myMIRTG (replicate (length myRules) 1.0)
           
           myDeriv = dn 1 [
                         dn 2 [
                             dn 3 [dn 4 []],
                             dn 5 []
                         ],
                         dn 6 [],
                         dn 7 [dn 8 []]
                     ]
               where dn i = VT.node (myHyperedges !! i)
       
       print $ sententialFront myMIRTG a_nt a_t myDeriv
       
       putStrLn $ replicate 20 '-'
       
       --}
       
       
       putStrLn "\nExtracting a LCFRS from a few more sentences:\n"
       
       ---
       
       corpusText' <- TIO.readFile "/home/sjm/programming/LCFRS/tiger_release_aug07.export" -- 7000 sentences
       let sentences' = sentences . parseNegra
                      $ corpusText'
       
       --print $ length $ concatMap show $ sentences' -- deepseq
       
       --print $ F.foldl' (+) (0::Integer) $ take 100000000 $ repeat 1 -- busy waiting
       
       ---
       
       let sentenceTrees = map (establishSpanBasedOrder . negraToCrossedTree . sData) sentences'
       
       --print $ length $ concatMap show $ sentenceTrees
       
       --print $ F.foldl' (+) (0::Integer) $ take 100000000 $ repeat 1
       
       ---
       
       let (intifiedTrees, (m_nt', m_t')) = dualIntifyNegra sentenceTrees
       
       --intifiedTrees `deepseq` return ()
       
       --print $ F.foldl' (+) (0::Integer) $ take 100000000 $ repeat 1
       
       ---
       
       let rules' = concatMap (flattenSeeing readoff) $ intifiedTrees
       
       -- {-
       let a_nt' = invertMap m_nt'
       let a_t' = invertMap m_t'
       rules' `deepseq` m_nt' `deepseq` m_t' `deepseq` return ()
       putStrLn "all deepseqed"
       print $ length rules'
       
       let nubbedRules = ordNub rules'
       
       writeFile "/tmp/a_nt" (show a_nt')
       writeFile "/tmp/a_t" (show a_t')
       writeFile "/tmp/rules" (show nubbedRules)
       -- -}
       
       {-
       a_nt' <- fmap read $ readFile "/tmp/a_nt" :: IO (A.Array Int String)
       a_t' <- fmap read $ readFile "/tmp/a_t" :: IO (A.Array Int String)
       nubbedRules <- fmap read $ readFile "/tmp/rules"
       putStrLn "opened files"
       -- -}
       
       print $ length $ nubbedRules
       
       let crossedRules = filter (any (\l -> all isNT l && sortBy (comparing getI) l /= l) . snd) nubbedRules
       
       mapM_ (putStrLn . retranslateRule a_nt' a_t') $ take 10 crossedRules
       print $ length crossedRules
  where
    isNT (T _) = False
    isNT (NT _) = True
    getI (T i) = i
    getI (NT i) = i
    
    -- https://github.com/nh2/haskell-ordnub
    ordNub :: (Ord a) => [a] -> [a]
    ordNub l = go S.empty l
      where
        go _ [] = []
        go s (x:xs) = if x `S.member` s then go s xs
                                        else x : go (S.insert x s) xs
    
    getNTName Nothing
      = "ε"
    getNTName (Just spart)
      = sdPostag spart
    
    readoff
      :: (T.Tree (Int, Maybe Int, [Span])) -- ^ the first Int is the POS-tag, the second can be a terminal word
      -> ((Int, [Int]), [[NTT]])
    readoff (T.Node (tag, Nothing, rootspans) cs)
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
    readoff (T.Node (tag, Just word, [(s1, s2)]) cs)
      = if not $ null cs then error "readoff: a word has children" else
        if s1 /= s2 then error "readoff: word has a wider span than 1" else
        ( (tag, [])
        , [[tt word]]
        )
    readoff _ = error "readoff: malformed tree"
    
    flattenSeeing :: (T.Tree a -> b) -> T.Tree a -> [b]
    flattenSeeing f n@(T.Node _ cs)
      = f n : concatMap (flattenSeeing f) cs
    
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
    dualIntifyNegraAction t = TR.mapM worker t
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
      :: (Show a, Show b)
      => (A.Array Int a)
      -> (A.Array Int b)
      -> ((Int, [Int]), [[NTT]]) -- ^ a rule
      -> String
    retranslateRule a_nt a_t ((lhs, rhs), hom_f)
      =  (cut 6 $ show $ (A.!) a_nt lhs)
      ++ " -> "
      ++ (cut 25 $ show $ map ((A.!) a_nt) rhs)
      ++ " // "
      ++ (show $ map (map retHomComponent) hom_f)
        where
          retHomComponent (T t) = show $ (A.!) a_t t
          retHomComponent (NT v) = show v -- Remember, these aren't real NTs, but variables for the reordering
    
    establishSpanBasedOrder :: T.Tree (v, [Span]) -> T.Tree (v, [Span])
    establishSpanBasedOrder (T.Node (v, spans) cs)
      = T.Node (v, spans)
      $ map establishSpanBasedOrder
      $ sortBy (comparing earliestSpan) cs
        where earliestSpan = minimum -- earliest span start
                           . map fst -- span starts
                           . snd -- span list
                           . T.rootLabel
