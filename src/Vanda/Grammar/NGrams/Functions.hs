-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Grammar.NGrams.Functions
-- Copyright   :  (c) Technische Universität Dresden 2013
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Tobias.Denkinger@mailbox.tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Top level functionality for NGrams.
--
-----------------------------------------------------------------------------


module Vanda.Grammar.NGrams.Functions where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Vanda.Grammar.NGrams.VandaNGrams
import Vanda.Grammar.NGrams.Text

import Debug.Trace

-- | Loads an NGram language model from a file.
loadNGrams
  :: FilePath                -- ^ file to load the model from
  -> IO (NGrams T.Text)      -- ^ NGrams model
loadNGrams
  = fmap parseNGrams
  . TIO.readFile

-- | completely broken (stupid alphas)
trainModel
  :: Int                     -- ^ minimum reliable count (/k/)
  -> Int                     -- ^ degree of the model (/n/)
  -> T.Text                  -- ^ text
  -> NGrams T.Text           -- ^ /n/-gram model
trainModel k n text
  = let sentenceCorpus   = M.fromListWith (+) [ (T.words x, 1) | x <- T.lines text ]
        nGramCorpus      = traceShow' $ M.fromList [ (n', fromSentenceCorpus sentenceCorpus $ fromIntegral n') | n' <- [1 .. n] ]
        smoothedCorpus   = traceShow' $ M.fromList [ (n', smoothCorpus (fromIntegral k) $ nGramCorpus M.! n') | n' <- [1 .. n] ]
        nGramWeight      = traceShow' $ M.fromList
                         $ [ (n', applyRFE (smoothedCorpus M.! n') $ nGramCorpus M.! (n' - 1))
                           | n' <- [2 .. n]
                           ]
                           ++
                           [ (1, flip M.map (smoothedCorpus M.! 1)
                               $ (\x -> (x + 1) / (fromIntegral $ M.foldl (+) 0 (nGramCorpus M.! 1)))
                             )
                           ]
        remainingMass    = M.fromList
                         $ [ (n', calculateRemainingMass (nGramWeight M.! (n' + 1)) (nGramWeight M.! n'))
                           | n' <- [1 .. (n - 1)]
                           ]
                           ++
                           [ (n, M.empty) ]
        f m1 m2          = M.unionWith
                             (\ (x, _) (_, y) -> (x, y))
                             m1
                             m2
        combinedMap      = M.unionWith
                             f
                             (M.map (M.map (\ x -> (x, Nothing))) nGramWeight)
                             (M.map (M.map (\ y -> (0, if y == 0 then Nothing else Just y))) remainingMass)
        weightedNGrams   = M.assocs . M.unions $ M.elems combinedMap
        g lm (w, (x, y)) = addNGram lm w x y
    in  L.foldl' g (empty n) weightedNGrams




-- | Evaluates a sentence given a NGrams model.
evaluateLine
  :: NGrams T.Text           -- ^ NGrams model
  -> T.Text                  -- ^ sentence to score
  -> Double                  -- ^ score
evaluateLine lm l
  = evaluate lm
  . (\ x -> ( L.replicate ((order lm) - 1)
            . T.pack
            $ "<s>"
            )
       L.++ x
       L.++ [(T.pack "</s>")]
    )
  . T.words
  $ l

-- | Converts a sentence corpus into a /n/-gram corpus.
fromSentenceCorpus
  :: Ord v
  => M.Map [v] Int -- ^ sentence corpus
  -> Int           -- ^ /n/
  -> M.Map [v] Int -- ^ /n/-gram corpus
fromSentenceCorpus m n
  = M.fromListWith (+)
  $ [ (gram, m M.! sent)
    | sent <- M.keys m
    , gram <- subsequences' n sent
    ]

-- | Applies Smoothting to a corpus.
smoothCorpus
  :: Ord v
  => Int              -- ^ minimum reliable count (/k/)
  -> M.Map [v] Int    -- ^ unsmoothed corpus
  -> M.Map [v] Double -- ^ smoothed corpus
smoothCorpus k m
  = M.map f m where
      f :: Int -> Double
      f d = if   d > k
            then fromIntegral d
            else {-((dd + 1) * (funcN $ d + 1) / (funcN d) - (dd) * (kk + 1) * (funcN $ k + 1) / (funcN 1))
                      / (1 - (kk + 1) * (funcN $ k + 1) / (funcN 1)) where
                      kk = fromIntegral k
                      dd = fromIntegral d -} -- mathematical bullshit
                 fromIntegral (d + 1) * (funcN $ d + 1) / (funcN d) -- only a workaround
      funcN :: Int -> Double -- N_c, but add-one smoothing is probably not so smart here
      funcN d' = fromIntegral $ length [ k' | k' <- M.keys m, m M.! k' == d' ] + 1

-- | Applies RFE with smoothing.
applyRFE
  :: Ord v
  => M.Map [v] Double -- ^ n-gram corpus for numerator, e.g., smoothed corpus
  -> M.Map [v] Int    -- ^ (/n/-1)-gram corpus for denominator, e.g., unsmoothed corpus
  -> M.Map [v] Double -- ^ RFE
applyRFE m1 m2
  = M.mapWithKey f m1 where
      f k d = (/) d
            $ if   length k > 1
              then fromIntegral . (M.!) m2 . flip take k $ length k - 1
              else fromIntegral $ sum [ m2 M.! k' | k' <- M.keys m2 ]

-- | Calculates the remaining probability mass.
calculateRemainingMass
  :: Ord v
  => M.Map [v] Double -- ^ (/n/+1)-gram weight function for numerator
  -> M.Map [v] Double -- ^ /n/-gram weight function for denominator
  -> M.Map [v] Double -- ^ remaining mass for /n/-grams
calculateRemainingMass m1 m2
  = M.fromList
  $ [ (k, (1 - s1) / (1 - s2))
    | k <- M.keys m2
    , let s1 = sum [ m1 M.! k' | k' <- M.keys m1, (reverse . drop 1 $ reverse k') == k ]
    , let s2 = sum [ m2 M.! k' | k' <- M.keys m2, (reverse . drop 1 $ reverse k') == drop 1 k ]
    ]

writeNGrams
  :: NGrams T.Text
  -> T.Text
writeNGrams lm
  = let grams = M.fromList
              $ [ (n, (ngrams, length ngrams))
                | n <- [1 .. (order lm)]
                , let ngrams = [ (map (invDict lm V.!) ngram, weights lm M.! ngram)
                               | ngram <- M.keys $ weights lm
                               , length ngram == n
                               ]
                ]
        header lst = [ T.pack "", T.pack "\\data\\" ]
                   ++ [ T.pack $ "ngram  " ++ show n ++ "=\t" ++ show l
                      | n <- [1 .. (order lm)]
                      , let l = snd $ lst M.! n
                      ]
                   ++ [ T.pack "" ]
        body lst = concat
                 $ [ [T.pack "", T.pack $ "\\" ++ show n ++ "-grams:"] ++ lns
                   | n <- [1 .. (order lm)]
                   , let lns = [ T.pack (show w1 ++ "\t")
                               `T.append` (T.intercalate (T.pack " ") gram)
                               `T.append` (T.pack $ case w2 of
                                                       Nothing -> ""
                                                       Just x  -> "\t" ++ show x)
                               | (gram, (w1, w2)) <- fst $ lst M.! n
                               ]
                   ]
    in  T.unlines $ header grams ++ body grams ++ [T.pack "\\end\\"]

subsequences' :: Int -> [v] -> [[v]]
subsequences' i xs
  = if   i <= length xs
    then (take i xs):(subsequences' i $ drop 1 xs)
    else []

traceShow' x
  = trace (f x) x where
      f m = unlines . map (\ (a, b) -> show (T.unwords a) ++ " -> " ++ show b) . M.toAscList . M.unions $ M.elems m
