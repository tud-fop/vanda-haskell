-- Copyright (c) 2010, Toni Dietze

{-------------------------------------------------------------------------------

profiling:
    ghc --make -prof -auto-all -caf-all -fforce-recomp Main.hs
    time ./Main m +RTS -hc -p -i0.005 | wc -l; cat Main.prof; hp2ps -e210mm -c Main.hp
    time ./Main m +RTS -hc -p -i0.005; hp2ps -e210mm -c Main.hp; ps2pdf Main.ps

-}------------------------------------------------------------------------------

module Main where

import Parser.Negra
import Text.Parsec.String (parseFromFile)

import Control.DeepSeq
import Data.Tree as T

main :: IO ()
main
--   = parseFromFile p_negra "Parser/tiger_release_aug07_notable_2000_utf-8.export"
  = parseFromFile p_negra "Parser/tiger_release_aug07_part_notable.export"
  >>= \ d -> let ts = fmap negrasToTrees d in
            rnf d
      `seq` wait 500000
      `seq` rnf d
      `seq` rnf ts
      `seq` wait 500000
      `seq` rnf ts
      `seq` return ()


wait 0 = 0
wait i = wait (i - 1)


negrasToTrees :: [Sentence] -> [T.Tree String]
negrasToTrees
  = concatMap
      ( fmap negraTreeToTree
      . negraToForest
      . filterPunctuation
      . sData
      )
