-----------------------------------------------------------------------------
-- |
-- Module      :  VandaCLI.PBSM
-- Copyright   :  (c) Technische Universität Dresden 2014
-- License     :  BSD-style
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module VandaCLI.PBSM where


import Vanda.Algorithms.Earley.WSA as WSA (fromList)
import Vanda.Algorithms.Earley
import Vanda.Algorithms.ExpectationMaximizationAcyclic
import Vanda.Algorithms.InsideOutsideWeightsAcyclic
import Vanda.Corpus.Penn.Text
import qualified Vanda.Features as F
import Vanda.Hypergraph
import Vanda.PBSM.PatternBasedStateMerging
import Vanda.PBSM.Types
import Vanda.Util.Memorysavers
import Vanda.Util.IO
import qualified Vanda.Util.Tree as T

import Control.Applicative ((<$>))
import Control.DeepSeq
import Control.Monad
import Data.Binary (decodeFile, encodeFile)
import Data.List
import qualified Data.Map.Lazy as M
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Tree as T
import Data.Tuple
import qualified Data.Vector as V
import System.Console.CmdArgs
import System.Directory
import System.FilePath ((</>), (<.>))
import System.Posix.Signals (sigUSR1)


import Debug.Trace


data Args
  = PrepareTreebank
    { argWorktree :: FilePath
    , argTreebank :: FilePath
    , argDefoliate :: Bool
    }
  | PBSM
    { argWorktree :: FilePath
--     , argTreebank1 :: FilePath
--     , argTreebank2 :: FilePath
--     , argDefoliate :: Bool
    }
  | EM
    { argWorktree :: FilePath
    }
  | Parse
    { argWorktree :: FilePath
    }
  deriving (Data, Show, Typeable)

argModes :: [Args]
argModes
  = [ PrepareTreebank
      { argWorktree = "" &= explicit &= name "work-tree" &= typDir
          &= help "work in DIR instead of current directory"
      , argTreebank = def &= argPos 0 &= typ "TREEBANK"
      , argDefoliate = False &= explicit &= name "defoliate"
          &= help "remove leafs from treebank trees, e.g. to just deal \
                  \with preterminals"
      } &= details
      [ ""
      ]
    , PBSM
      { -- argTreebank1 = def &= argPos 0 &= typ "TREEBANK1"
--       , argTreebank2 = def &= argPos 1 &= typ "TREEBANK2"
--       , argDefoliate = False &= explicit &= name "defoliate"
--           &= help "remove leafs from treebank trees, e.g. to just deal \
--                   \with preterminals"
      } &= details
      [ "Read-off a grammar from TREEBANK1 and generalize it using TREEBANK2."
      ]
    , EM {} &= details
      [ "Add weights to the pbsm result using the EM algorithm."
      ]
    , Parse {} &= details
      [ "Parse newline-separated sentences from standard input."
      ]
    ]
  &= helpArg [name "h"]
  &= versionArg [ignore]
  &= program "command"
  &= summary "Pattern-Based State Merging"


type FileGrammar  = RTG Int String
type FileTreebank = [T.Tree String]
type FileWeights  = M.Map Int Double


initialNT :: Int   ; initialNT = 0
initialT  :: String; initialT  = "ROOT"


main :: IO ()
main = do
  arguments <- cmdArgs (modes argModes)
  print arguments
  createDirectoryIfMissing True (argWorktree arguments)
  let fileTreebank1 = argWorktree arguments </> "treebank1.bin"
      fileTreebank2 = argWorktree arguments </> "treebank2.bin"
      fileGrammar   = argWorktree arguments </> "grammar.bin"
      fileWeights   = argWorktree arguments </> "weights.bin"
  let loadCorpus defol c
        = map (if defol then T.defoliate else id)
        . parsePenn
        <$> Text.readFile c
  case arguments of

    PrepareTreebank { .. } -> do
      (c1, c2, m) <- partitionTreebank <$> loadCorpus argDefoliate argTreebank
      encodeFile fileTreebank1 (c1 :: FileTreebank)
      encodeFile fileTreebank2 (c2 :: FileTreebank)
      putStr
        $ unlines
        $ map (\ (n, x) -> show n ++ "\t" ++ show x)
        $ sort
        $ map swap
        $ M.toList m

    PBSM { .. } -> do
      [c1, c2] <- unify3 <$> mapM decodeFile [fileTreebank1, fileTreebank2]
               :: IO [FileTreebank]
      let g = generalize
                head
                (fst $ intifyNonterminals $ forestToGrammar c1)
                (traceForestTreesOnEvaluation c2)
      if null fileGrammar
        then print g
        else encodeFile fileGrammar (g :: FileGrammar)

    EM { .. } -> do  -- from now on everything is start-separated
      g <- decodeFile fileGrammar :: IO FileGrammar
      c <- force . unify2 . concat
       <$> mapM decodeFile [fileTreebank1, fileTreebank2]
        :: IO FileTreebank
      (concat <$> mapM decodeFile [fileTreebank1, fileTreebank2] :: IO FileTreebank)
        >>= (\ l -> putStrLn $ "Corpus contains " ++ show l ++ " trees.") . length
      let hg :: EdgeList Int String Int
          hg   = toHypergraphStartSeparated initialNT initialT g
          part = M.elems $ M.fromListWith (++) $ map (\ e -> (to e, [ident e])) $ edges hg
          hgs  = map (dropUnreach [initialNT] . parseTree hg . startSepTree) c
                 where startSepTree tree = T.Node initialT [tree]
          w0   = M.singleton initialNT (1 :: Double)
          ws   = M.fromList $ flip zip (repeat 1) $ map ident $ edges hg
      let worker update =
            forM_ (zip [0 :: Int ..] $ take 5 $ forestEMlist part (zip hgs (repeat 1)) ident w0 ws)
              $ \ (i, (lklhood, (_, ws'))) -> do
                putStrLn $ "EM step " ++ show i ++ "; Likelihood: " ++ show lklhood
                update (i, ws')
          handler (i, ws') = do
            putStrLn $ "Saving result of EM step " ++ show i ++ " ..."
            encodeFile (fileWeights <.> "tmp") (ws' :: FileWeights)
            renameFile (fileWeights <.> "tmp") fileWeights
            putStrLn $ "   ... result of EM step " ++ show i ++ " saved."
      handleOnDemand Nothing Nothing [sigUSR1] worker {-handler-}(\ _ -> return ())

    Parse { .. } -> do
      g <- decodeFile fileGrammar :: IO FileGrammar
      let hg :: BackwardStar Int (String, Int) Int
          hg = toHypergraphStartSeparatedRanked initialNT initialT g
          comp :: (t, Int) -> [Either Int t]
          comp (t, k) = if k == 0 then [Right t] else map Left [0 .. k - 1]
      weightM <- decodeFile fileWeights :: IO FileWeights
      let lkup i = M.findWithDefault err i weightM
          err = errorModule "main.lkup: vertex not found"
      let feature = F.Feature (\ _ (i, _) xs -> lkup i * product xs) V.singleton
      sents <- map words . lines <$> getContents
      forM_ sents $ \ sent -> do
        let (ntM, hg', weights') = earley hg comp (WSA.fromList 1 sent) initialNT
        putStrLn $ unlines $ map show $ edges hg'
        print weights'
        let bs = bests hg' feature (V.singleton 1)
        case M.lookup (0, initialNT, length sent) ntM of
          Nothing -> putStrLn "No parse."
          Just nt ->
            putStr
              $ unlines
              $ map (drawTree . fmap (fst . label) . F.deriv)
              $ take 10
              $ bs M.! nt


partitionTreebank
  :: Ord a => [T.Tree a] -> ([T.Tree a], [T.Tree a], M.Map (a, Int) Int)
partitionTreebank = go M.empty
  where
    go labelM [] = ([], [], labelM)
    go labelM (t : ts)
      = if all (\ k -> M.findWithDefault 0 k labelM > 0) labels
        then (ts1, t : ts2, labelM')
        else (t : ts1, ts2, labelM')
      where
        labels = T.flattenRanked t
        (ts1, ts2, labelM')
          = go (foldl' (\ m k -> M.insertWith (+) k 1 m) labelM labels) ts


traceForestTreesOnEvaluation :: [T.Tree String] -> [T.Tree String]
traceForestTreesOnEvaluation
  = zipWith zipper [1 :: Int ..]
  where
    zipper i t = trace msg t
      where msg = "<Tree " ++ show i ++ ">\n"
                ++ drawTree t
                ++ unwords (T.yield t)
                ++ "\n</Tree " ++ show i ++ ">"


drawTree :: T.Tree String -> String
drawTree = T.drawTree' (T.drawstyleCompact1 "─╴") . T.mapLeafs colorVividYellow
  where colorVividYellow :: String -> String
        colorVividYellow cs = "\ESC[93m" ++ cs ++ "\ESC[m"


errorModule :: String -> a
errorModule = error . ("Vanda.PBSM.Main." ++)
