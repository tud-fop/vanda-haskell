-- Copyright (c) 2010, Toni Dietze

{-------------------------------------------------------------------------------

profiling:
    ghc --make -prof -auto-all -caf-all -fforce-recomp Main.hs
    time ./Main m +RTS -hc -p -i0.005 | wc -l; cat Main.prof; hp2ps -e210mm -c Main.hp

-}------------------------------------------------------------------------------

module Main where

import RuleExtraction
import Parser.ApplicativeParsec
import Parser.Negra
import Parser.Penn
import Tools.PrettyPrint

import Data.Function(on)
import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S
import qualified Data.Tree as T


main = mainExtract


testdata = parseFromFile p_negra "Parser/tiger_release_aug07_part.export"
-- testdata = parseFromFile p_negra "/var/local/share/gdp/nlp/resources/tigercorpus2.1/corpus/tiger_release_aug07.export"


withTestdata f = testdata >>= \x ->
    case x of
      Left  _ -> print x
      Right r -> putStrLn (f r)


mainExtractWTA = withTestdata
    $ show
    . extractWTA
    . fmap (fmap (\(x, y, _, _) -> (x, y)))
    . map negraTreeToTree
    . concatMap negraToForest
    . map sData


mainExtract = withTestdata
    $ unlines
    . fmap show
    . L.sortBy (compare `on` snd)
    . M.toList
    . extract
    . fmap (fmap (\(x, y, _, _) -> (x, y)))
    . map negraTreeToTree
    . concatMap negraToForest
    . map sData


mainGetFlatTrees = withTestdata
    $ unlines
    . fmap (T.drawTree . fmap show)
    . filter ((>) 5 . length . T.levels)
    . map negraTreeToTree
    . concatMap negraToForest
    . map sData


mainPrintTree i = withTestdata
    $ T.drawTree
    . fmap show
    . (!! i)
    . map negraTreeToTree
    . concatMap negraToForest
    . map sData
