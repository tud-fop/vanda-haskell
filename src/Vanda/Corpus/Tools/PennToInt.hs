module Main where

import Control.Applicative ( (<*>), (<*), (*>), (<|>), (<$>), many )
import Control.Arrow ( second )
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Tree as T
import System.Environment ( getArgs )
import Text.Parsec hiding ( many, (<|>) )
import Text.Parsec.Text.Lazy

import Vanda.Token ( updateToken, fromText, toText )
import Vanda.Corpus.Penn.Text ( parsePennMap, unparsePenn )


main = do
  args <- getArgs
  case args of
    ["-m", mapFile, "-p", pennFile] -> do
      mf <- TIO.readFile mapFile
      pf <- TIO.readFile pennFile
      -- let ~(m', p') = parsePennMap updateToken (fromText mf) pf
      case parsePennMap updateToken (fromText mf) pf of
        (m', p') -> do
          -- let (m', _) = parsePennMap updateToken (fromText mf) pf
          TIO.writeFile (pennFile ++ ".int") (unparsePenn p')
          TIO.writeFile (mapFile ++ ".new") (toText m')
    _ -> print "Usage: PennToInt -m <map file> -p <penn file>"
