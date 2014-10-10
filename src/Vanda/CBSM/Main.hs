{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CBSM.Main
-- Copyright   :  (c) Technische Universität Dresden 2014
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.CBSM.Main where


import qualified Data.RevMap as RM
import           Vanda.Algorithms.EarleyMonadic
import qualified Vanda.Algorithms.Earley.WSA as WSA
import           Vanda.CBSM.CountBasedStateMerging
import           Vanda.Corpus.SExpression as SExp
import qualified Vanda.Features as F
import qualified Vanda.Hypergraph as H
import           Vanda.Util.Tree as T

import           Control.Applicative ((<$>))
import           Control.Arrow ((***))
import           Control.Monad
import qualified Data.Binary as B
import qualified Data.Map as M
import           Data.Ord
import qualified Data.Set as S
import           Data.Tree
import qualified Data.Vector as V
import           System.Console.CmdArgs.Explicit
import           System.Console.CmdArgs.Text
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath ((</>))


moduleName :: String
moduleName = "Vanda.CBSM.Main"


errorHere :: String -> String -> a
errorHere fun msg = error $ moduleName ++ "." ++ fun ++ ": " ++ msg


data Args
  = Help HelpFormat TextFormat
  | PrintCorpora
    { flagAsForests :: Bool
    , argCorpora :: [FilePath]
    }
  | CBSM
    { flagAsForests :: Bool
    , flagDefoliate :: Bool
    , flagGrammar :: FilePath
    , argIterations :: Int
    , argCorpora :: [FilePath]
    }
  | Parse
    { argGrammar :: FilePath
    , argCount :: Int
    }
  | Bests
    { argGrammar :: FilePath
    , argCount :: Int
    }
  deriving Show

argMode :: Mode Args
argMode
  = modes' "Main" (Help HelpFormatAll defaultWrap) "Count-Based State Merging"
  [ flagHelpFormat (\ h t _ -> Help h t) ]
  [ mode'
      "print-corpora"
      (PrintCorpora False [])
      "Print trees from TREEBANKs. Can be used to check for parsing \
      \errors. Every TREEBANK can be a file or a directory. Directories \
      \are traversed recursively. If no TREEBANK is given, the trees are \
      \read from standard input."
      [flagNoneAsForests]
      []
      (Just flagArgCorpora)
  , mode'
      "cbsm"
      (CBSM False False "" undefined [])
      "Read-off a grammar from TREEBANKs and generalize it. See \
      \printcorpora for further information about the TREEBANK arguments."
      [ flagNoneAsForests
      , flagNone ["defoliate"] (\ x -> x{flagDefoliate = True})
          "remove leaves from trees in TREEBANKs"
      , flagReq ["output"] (\ a x -> Right x{flagGrammar = a}) "FILE"
          "write result to FILE instead of stdout"
      ]
      [ (flagArg (readUpdate $ \ a x -> x{argIterations = a}) "ITERATIONS")
          {argRequire = True}
      ]
      (Just flagArgCorpora)
  , mode'
      "parse"
      (Parse "" 1)
      "Parse newline-separated sentences from standard input."
      []
      [ flagArgGrammar{argRequire = True}
      , flagArgCount
      ]
      Nothing
  , mode'
      "bests"
      (Bests "" 1)
      "View best trees of a grammar."
      []
      [ flagArgGrammar{argRequire = True}
      , flagArgCount
      ]
      Nothing
  ]
  where
    flagNoneAsForests
      = flagNone ["as-forests"] (\ x -> x{flagAsForests = True})
          "the TREEBANKs contain forests instead of trees"
    flagArgCorpora
      = flagArg (\ a x -> Right x{argCorpora = argCorpora x ++ [a]}) "TREEBANK"
    flagArgGrammar
      = flagArg (\ a x -> Right x{argGrammar = a}) "GRAMMAR-FILE"
    flagArgCount
      = flagArg (readUpdate $ \ a x -> x{argCount = a}) "COUNT"


readUpdate :: Read r => (r -> a -> a) -> Update a
readUpdate update a x = fmap (\ r -> update r x) (readEither "no parse" a)


readEither :: Read r => a -> String -> Either a r
readEither msg cs = case reads cs of
  (x, "") : _ -> Right x
  _           -> Left msg

mode' :: Name -> a -> Help -> [Flag a] -> [Arg a] -> Maybe (Arg a) -> Mode a
mode' name value help flags args argSweeper
  = (modeEmpty value)
  { modeNames = [name]
  , modeHelp = help
  , modeArgs = (args, argSweeper)
  , modeGroupFlags = toGroup flags
  }


modes' :: Name -> a -> Help -> [Flag a] -> [Mode a] -> Mode a
modes' name value help flags xs = (modeEmpty value)
  { modeNames      = [name]
  , modeHelp       = help
  , modeGroupFlags = toGroup flags
  , modeGroupModes = toGroup xs
  }


type BinaryCRTG = CRTG Int String


main :: IO ()
main = do
  arguments <- processArgs argMode
  case arguments of

    Help helpFormat textFormat
      -> putStr
       $ showText textFormat
       $ helpText [] helpFormat argMode

    PrintCorpora{..}
       -> putStr
        . unlines
        . concatMap (\ (i, t) -> [show i ++ ":", drawTreeColored t])
        . zip [1 :: Int ..]
      =<< readCorpora flagAsForests argCorpora

    CBSM{..}
       -> ( if null flagGrammar
            then print
            else B.encodeFile flagGrammar :: BinaryCRTG -> IO ()
          )
      =<< progress (\ i -> "Iteration " ++ show i) argIterations
        . cbsm
        . forestToGrammar
        . map (if flagDefoliate then T.defoliate else id)
      =<< readCorpora flagAsForests argCorpora

    Parse{..} -> do
      (hg, inis) <- toHypergraph <$> (B.decodeFile argGrammar :: IO BinaryCRTG)
      let comp e | a == 0    = [Right (H.label e)]
                 | otherwise = map Left [0 .. a - 1]
            where a = H.arity e
      let feature = F.Feature (\ _ (i, _) xs -> i * product xs) V.singleton
      sents <- map words . lines <$> getContents
      forM_ sents $ \ sent -> do
        let (hg', _) = earley' (asBackwardStar hg) comp (WSA.fromList 1 sent) (M.keys inis)
        let inis' = M.mapKeys (\ k -> (0, k, length sent)) inis
        printWeightedDerivations
          $ take argCount
          $ bestsIni hg' feature (V.singleton 1) inis'

    Bests{..} -> do
      (hg, inis) <- toHypergraph <$> (B.decodeFile argGrammar :: IO BinaryCRTG)
      let feature = F.Feature (\ _ i xs -> i * product xs) V.singleton
      printWeightedDerivations
        $ take argCount
        $ bestsIni (asBackwardStar hg) feature (V.singleton 1) inis

  where
    readCorpora :: Bool -> [FilePath] -> IO (Forest String)
    readCorpora asForests corpora
        = (if asForests then concatMap SExp.toForest else map SExp.toTree)
      <$> if null corpora
            then SExp.parse SExp.pSExpressions "stdin"
             <$> getContents
            else concat
             <$> (   SExp.parseFromFiles SExp.pSExpressions
                 =<< getContentsRecursive corpora
                 )

    printWeightedDerivations xs =
      forM_ (zip [1 :: Int ..] xs) $ \ (i, (w, t)) -> do
        let t' = fmap H.label t
        putStrLn $ show i ++ ": " ++ show w
        putStrLn $ unwords $ yield t'
        putStrLn $ drawTreeColored t'


drawTreeColored :: Tree String -> String
drawTreeColored
  = drawTree' (drawstyleCompact2 0 "")
  . mapLeafs (\ cs -> "\ESC[93m" ++ cs ++ "\ESC[m")


progress :: (Int -> String) -> Int -> [a] -> IO a
progress msg n xs0 = go xs0 0
  where
    go [] _ = errorHere "progress" "list too short"
    go (x : xs) i = do
      putStrLn $ msg i
      x `seq` if i >= n
        then return x
        else go xs $! i + 1


bestsIni
  :: (H.Hypergraph h, Ord v, Eq l)
  => h v l i
  -> F.Feature l i x
  -> V.Vector Double
  -> M.Map v Double
  -> [(Double, H.Derivation v l i)]
bestsIni hg feat wV inis
  = mergesBy (comparing (Down . fst))
  $ M.elems
  $ M.intersectionWith (\ w' -> map (\ (F.Candidate w d _) -> (w' * w, d))) inis
  $ H.bests hg feat wV
  where

    -- | Merge sorted lists to a single sorted list.
    mergesBy :: (a -> a -> Ordering) -> [[a]] -> [a]
    mergesBy cmp = foldl (mergeBy cmp) []


    -- | Merge two sorted lists to a single sorted list.
    mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
    mergeBy cmp xs@(x:xs') ys@(y:ys')
      = case x `cmp` y of
          GT ->  y : mergeBy cmp xs  ys'
          _  ->  x : mergeBy cmp xs' ys
    mergeBy _ [] ys = ys
    mergeBy _ xs [] = xs


getContentsRecursive :: [FilePath] -> IO [FilePath]
getContentsRecursive paths
  = fmap concat
  $ forM paths $ \ path ->
      ifM (doesDirectoryExist path)
        ( getDirectoryContents path
          >>= getContentsRecursive
            . map (path </>)
            . filter (`notElem` [".", ".."])  -- TODO: this seems like a hack
        )
        (return [path])


ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM predicateM thn els = do
  b <- predicateM
  if b then thn else els


test3 :: (Show l, Ord l) => Int -> [Tree l] -> IO ()
test3 n
  = putStr
  . unlines
  . concatMap (\ (w, d) -> [show w, drawTree' (drawstyleCompact2 0 "") $ fmap (show . H.label) d])
  . bests
  . (!! n)
  . iterate cbsmStep2
  . forestToGrammar


test2 :: (Show l, Ord l) => Int -> [Tree l] -> IO ()
test2 n
  = putStr
  . unlines
  . map (unlines . map show . H.edges . asEdgeList . fst . toHypergraph)
  . take n
  . iterate cbsmStep2
  . forestToGrammar


test1 :: (Show l, Ord l) => Int -> [Tree l] -> IO ()
test1 n
  = putStr
  . unlines
  . map (uncurry (++) . ((unlines . map show . H.edges . asEdgeList . fst . toHypergraph) *** (unlines . map showStep1)))
  . take n
  . tail
  . iterate step . (\ x -> (x, undefined))
  . forestToGrammar
  where
    step (g, _) = (cbsmStep2 g, refineRanking (mergeRanking g))

    showStep1 ((s, ((v1, n1), (v2, n2))), (mrg, delta))
      =  show s ++ "=" ++ show n1 ++ "+" ++ show n2 ++ ": "
      ++ show delta ++ ": "
      ++ show [v1, v2]
      ++ if M.size (RM.forward mrg) > 2
         then " -> " ++ show (map S.toList $ M.elems $ RM.backward mrg)
         else " (saturated)"


asEdgeList :: H.EdgeList v l i -> H.EdgeList v l i
asEdgeList = id

-- asBackwardStar :: H.BackwardStar v l i -> H.BackwardStar v l i
-- asBackwardStar = id
