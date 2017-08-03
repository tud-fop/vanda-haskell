-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.Corpus.SExpression.CmdArgs
-- Copyright   :  (c) Technische Universität Dresden 2016
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module Vanda.Corpus.SExpression.CmdArgs
( -- * Porcelain
  --
  -- | Example usage:
  --
  -- > import System.Console.CmdArgs.Explicit
  -- >
  -- > import Vanda.Corpus.SExpression.CmdArgs
  -- >
  -- > data Args
  -- >   = SomeCtor
  -- >     { flagSomething :: ()
  -- >     , flagCorpora   :: CmdArgsCorpora
  -- >     }
  -- >   deriving (Read, Show)
  -- >
  -- > cmdArgs :: Mode Args
  -- > cmdArgs
  -- >   = modes "…" undefined "…"
  -- >   [ (modeEmpty SomeCtor
  -- >        { flagSomething = ()
  -- >        , flagCorpora = defaultCmdArgsCorpora
  -- >        })
  -- >     { modeNames      = ["…"]
  -- >     , modeHelp       = "…"
  -- >     , modeArgs       = ([{- … -}], Just (flagArgCorpora liftCmdArgsCorpora))
  -- >     , modeGroupFlags = toGroup
  -- >                      $ [{- … -}] ++ modeFlagsCorpora liftCmdArgsCorpora
  -- >     }
  -- >   ]
  -- >   where
  -- >     liftCmdArgsCorpora f = \ x -> x{flagCorpora = f (flagCorpora x)}
  CmdArgsCorpora(..)
, defaultCmdArgsCorpora
, flagArgCorpora
, modeFlagsCorpora
, readCorpora
, -- * Plumbing
  -- ** Various CmdArgs 'Flag's
  flagNoneWeightedCorpus
, flagNoneAsForests
, flagNonePennFilter
, flagNoneDefoliate
, flagReqFilterByLeafs
, flagReqFilterByLength
, -- ** Functions to deal with the CmdArgs
  readSExpressions
, toCorpus
, preprocessCorpus
, readAllowedLeafs
, filterByLeafsBy
, filterByLength
) where


import           Control.Arrow (first)
import           Data.Maybe (mapMaybe)
import qualified Data.Set as S
import           Data.Tree
import           System.Console.CmdArgs.Explicit
import qualified Text.Parsec.Error as Parsec

import qualified Control.Error (errorHere)
import           System.Console.CmdArgs.Explicit.Misc (readUpdate)
import           System.Directory.Extra (getDirectoryContentsRecursive)
import           Vanda.Corpus.Binarization.CmdArgs
import           Vanda.Corpus.Penn.Filter
import           Vanda.Corpus.SExpression as SExp
import           Vanda.Util.Tree as T


errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.Corpus.SExpression.CmdArgs"


data CmdArgsCorpora
  = CmdArgsCorpora
    { flagWeightedCorpus :: Bool
    , flagAsForests :: Bool
    , flagPennFilter :: Bool
    , flagDefoliate :: Bool
    , flagFilterByLeafs :: FilePath
    , flagFilterByLength :: Int
    , flagBinarization :: FlagBinarization
    , argCorpora :: [FilePath]
    }
  deriving (Read, Show)


defaultCmdArgsCorpora :: CmdArgsCorpora
defaultCmdArgsCorpora
  = CmdArgsCorpora
      { flagWeightedCorpus = False
      , flagAsForests      = False
      , flagPennFilter     = False
      , flagDefoliate      = False
      , flagFilterByLeafs  = ""
      , flagFilterByLength = -1
      , flagBinarization   = FBNone
      , argCorpora         = []
      }


flagArgCorpora
  :: ((CmdArgsCorpora -> CmdArgsCorpora) -> a -> a)
     -- ^ lift update function to 'Arg'’s type
  -> Arg a
flagArgCorpora f
  = flagArg (\ a -> Right . f (\ x -> x{argCorpora = argCorpora x ++ [a]}))
            "TREEBANK"


modeFlagsCorpora
  :: ((CmdArgsCorpora -> CmdArgsCorpora) -> a -> a)
     -- ^ lift update function to 'Flag'’s type
  -> [Flag a]
modeFlagsCorpora f
  = [ flagNoneWeightedCorpus (       f $ \ x -> x{flagWeightedCorpus = True})
    , flagNoneAsForests      (       f $ \ x -> x{flagAsForests      = True})
    , flagNonePennFilter     (       f $ \ x -> x{flagPennFilter     = True})
    , flagNoneDefoliate      (       f $ \ x -> x{flagDefoliate      = True})
    , flagReqFilterByLeafs   (\ a -> f $ \ x -> x{flagFilterByLeafs  = a   })
    , flagReqFilterByLength  (\ a -> f $ \ x -> x{flagFilterByLength = a   })
    , flagReqBinarization    (\ b -> f $ \ x -> x{flagBinarization   = b   })
    ]


readCorpora :: (Num c, Read c) => CmdArgsCorpora -> IO [(Tree String, c)]
readCorpora CmdArgsCorpora{..} = do
  allowedLeafs <- readAllowedLeafs flagFilterByLeafs
  preprocessCorpus
          flagPennFilter
          flagDefoliate
          allowedLeafs
          flagFilterByLength
          flagBinarization
      . toCorpus
          flagWeightedCorpus
          flagAsForests
    <$> readSExpressions argCorpora


-- | Assume that the trees in the corpus have associated counts.
flagNoneWeightedCorpus :: (a -> a) -> Flag a
flagNoneWeightedCorpus update
  = flagNone ["weighted-corpus"] update
      "the TREEBANKs also contain weights for every tree. A corpus entry \
      \then has the form (weight tree) where weight is an integer and \
      \tree is an s-expression denoting a tree."


-- | Assume that the corpus contains forests instead of trees.
flagNoneAsForests :: (a -> a) -> Flag a
flagNoneAsForests update
  = flagNone ["as-forests"] update
      "the TREEBANKs contain forests instead of trees"


-- | Remove predicate argument structure annotations from corpus trees.
flagNonePennFilter :: (a -> a) -> Flag a
flagNonePennFilter update
  = flagNone ["penn-filter"] update
      "remove predicate argument structure annotations from TREEBANKs"


-- | Remove leaves from corpus trees.
flagNoneDefoliate :: (a -> a) -> Flag a
flagNoneDefoliate update
  = flagNone ["defoliate"] update
      "remove leaves from trees in TREEBANKs"


-- | Only use corpus trees whose leafs occur in the given file.
flagReqFilterByLeafs :: (FilePath -> a -> a) -> Flag a
flagReqFilterByLeafs update
  = flagReq ["filter-by-leafs"] ((Right .) . update)
      "FILE"
      "only use trees whose leafs occur in FILE"


-- | Filter corpus trees depending on the yields length.
flagReqFilterByLength :: (Int -> a -> a) -> Flag a
flagReqFilterByLength update
  = flagReq ["filter-by-length"] (readUpdate update)
      "LENGTH"
      "only use trees with a yield shorter than LENGTH (default -1 = no restriction)"


-- | Read 'SExpression's from given files. If no file is given, stdin is used.
-- The order in which the files are read is undefined.
readSExpressions :: [FilePath] -> IO [SExpression]
readSExpressions argCorpora
   = if null argCorpora
        then SExp.parse SExp.pSExpressions "stdin"
          <$> getContents
        else concat
          <$> (   SExp.parseFromFiles SExp.pSExpressions
              =<< getDirectoryContentsRecursive argCorpora
              )


toCorpus
  :: (Num c, Read c)
  => Bool                  -- ^ cf. 'flagNoneWeightedCorpus'
  -> Bool                  -- ^ cf. 'flagNoneAsForests'
  -> [SExpression]         -- ^ the 'SExpression's to be converted
  -> [(Tree String, c)]  -- ^ Trees with associated counts
toCorpus flagWeightedCorpus flagAsForests
   = (if flagAsForests      then concatMap (floatFst . first SExp.toForest)
                            else map (first SExp.toTree))
   . (if flagWeightedCorpus then map extractWeight else map (\ t -> (t, 1)))
  where
    extractWeight SExp.List{sExpList = [Atom{sExpAtom = w}, t]} = (t, read w)
    extractWeight x
      = errorHere "toCorpus.extractWeight"
      $ show
      $ Parsec.newErrorMessage
          (Parsec.Expect "list with two elements where the first is an atom")
          (SExp.sExpPos x)


preprocessCorpus
  :: Bool              -- ^ cf. 'flagNonePennFilter'
  -> Bool              -- ^ cf. 'flagNoneDefoliate'
  -> Maybe [String]    -- ^ cf. 'flagReqFilterByLeafs' and 'readAllowedLeafs'
  -> Int               -- ^ cf. 'flagReqFilterByLength'
  -> FlagBinarization  -- ^ cf. 'flagReqBinarization'
  -> [(Tree String, c)]
  -> [(Tree String, c)]
preprocessCorpus
     flagPennFilter
     flagDefoliate
     allowedLeafs
     flagFilterByLength
     flagBinarization
  = map (first $ encodeByFlag flagBinarization)
  . filterByLength flagFilterByLength
  . maybe id (filterByLeafsBy fst) allowedLeafs
  . (if flagDefoliate  then map (first T.defoliate)              else id)
  . (if flagPennFilter then mapMaybe (floatFst . first stripAll) else id)


-- | Read whitespace separated list of words from file. Returns 'Nothing' if
-- 'FilePath' is 'null'.
readAllowedLeafs
  :: FilePath  -- ^ cf. 'flagReqFilterByLeafs'
  -> IO (Maybe [String])
readAllowedLeafs ""   = return Nothing
readAllowedLeafs file = Just . words <$> readFile file


-- | @filterByLeafsBy f ws xs@ keeps only those entries @x@ of @xs@ such that
-- every leaf of @f x@ is contained in ws.
filterByLeafsBy
  :: Ord b
  => (a -> Tree b)
  -> [b]  -- ^ cf. 'flagReqFilterByLeafs' and 'readAllowedLeafs'
  -> [a]
  -> [a]
filterByLeafsBy f ws
  = filter (all (`S.member` S.fromList ws) . yield . f)


filterByLength
  :: Int  -- ^ cf. 'flagReqFilterByLength'
  -> [(Tree String, a)]
  -> [(Tree String, a)]
filterByLength l
  | l <= 0 = id
  | l == 1 = errorHere "filterByLength" "Lengths smaller than 1 doesn't make any sense."
  | otherwise = filter ((<l) . length . yield . fst)


floatFst :: Functor f => (f a, b) -> f (a, b)
floatFst (fx, y) = fmap (\ x -> (x, y)) fx
