-- Copyright (c) 2010, Toni Dietze

{-------------------------------------------------------------------------------

profiling:
    ghc --make -prof -auto-all -caf-all -fforce-recomp Main.hs
    time ./Main m +RTS -hc -p -i0.005 | wc -l; cat Main.prof; hp2ps -e210mm -c Main.hp

-}------------------------------------------------------------------------------

module Main where

import RuleExtraction
import Parser.Negra
import Parser.Penn
import Tools.PrettyPrint

import Parser.ApplicativeParsec

testdata = parseFromFile p_negra "Parser/tiger_release_aug07_part.export"

main = testdata >>= print . fmap length

main2 = fmap (extract . map negraTreeToTree . concatMap negraToForest . map sData) <$> testdata

-- main = parseFromFile p_negra "/var/local/share/gdp/nlp/resources/tigercorpus2.1/corpus/tiger_release_aug07.export" >>= print . fmap length