module Main where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import System.Environment ( getArgs )

import Vanda.Corpus.Negra
import Vanda.Corpus.Negra.Text
import Vanda.Corpus.Penn.Text

-- unbound = ["UNKNOWN","--","$,","$.","$("]
-- unbound = map N.wtTag $ filter (not . N.wtBound) $ N.wordtags n
fltr unbound SentenceWord{sdPostag = tag} = tag `notElem` unbound
fltr _ _ = True

main = do
  args <- getArgs
  case args of
    ["-n", negraFile, "-p", pennFile] -> do
      nf <- TIO.readFile negraFile
      TIO.writeFile pennFile
        . unparsePenn
        . map negraTreeToTree
        . concatMap negraToForest
        . filter (not . null)
        . (\ (Negra wt s) ->
            map
              ( filter (fltr (map wtTag $ filter (not . wtBound) wt))
              . sData
              )
              s
          )
        . parseNegra
        $ nf
    _ -> print "Usage: NegraToPenn -n <negra file> -p <penn file>"