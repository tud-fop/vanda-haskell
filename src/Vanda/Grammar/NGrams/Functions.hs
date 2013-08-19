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

-- | Loads an NGram language model from a file.
loadNGrams
  :: FilePath                -- ^ file to load the model from
  -> IO (NGrams T.Text)      -- ^ NGrams model
loadNGrams
  = fmap parseNGrams
  . TIO.readFile

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
