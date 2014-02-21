{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding ( span )

import Codec.Compression.GZip ( compress )

import Control.Arrow ( (&&&), second )
import Control.DeepSeq ( NFData(..), ($!!) )
import Control.Seq
import Control.Monad.State.Strict ( State, evalState, runState, get, put )

import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap as IM
import Data.List ( foldl', sortBy, unzip4 )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as TIO
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Tree as T
import qualified Data.Vector as V

import System.Environment ( getArgs, getProgName )

import Text.Parsec hiding ( State, label )
import Text.Parsec.Text.Lazy ( GenParser )

import Vanda.Corpus.Penn.Text ( parsePenn )
import Vanda.Grammar.XRS.GHKM ( Alignment (..), Rule (..), extractRules )
import Vanda.Hypergraph hiding ( label )
import qualified Vanda.Hypergraph as Hypergraph ( label )
import Vanda.Token
import Vanda.Hypergraph.Binary ()

number :: T.Tree a -> T.Tree (Either a (a, Int))
number t = evalState (number' t) 1
  where
    number' :: T.Tree a -> State Int (T.Tree (Either a (a, Int)))
    number' (T.Node l ts)
      | null ts   = do
                      i <- get
                      put (i+1)
                      return $ T.Node (Right (l,i)) []
      | otherwise = do
                      ts' <- mapM number' ts
                      return $ T.Node (Left l) ts'


p_mapv :: (u -> String -> (u, l)) -> String -> GenParser u l
p_mapv mapper !s = do
  u <- getState
  let (u', v) = mapper u s
  setState u'
  return v

alignParser :: Show l => (u -> String -> (u, l)) -> GenParser u [(l, [Int])]
alignParser mapper
  = do
      res <- many1
              $ do
                {- wd <- p_mapv mapper =<< many1 (noneOf " ") -- alphaNum;
                ; trace (show wd) -}
                { wd1 <- many1 (noneOf " \n\t") -- alphaNum;
                ; {-trace (show wd1)
                $-} skipMany (char ' ')
                ; wd <- p_mapv mapper wd1
                ; _ <- char '('
                ; _ <- char '{'
                ; skipMany (char ' ')
                ; ls <- many
                        $ do { ds <- many1 digit
                             ; skipMany (char ' ')
                             ; return ds
                             }
                ; _ <- char '}'
                ; _ <- char ')'
                ; skipMany (char ' ')
                ; return (wd, map ((+(-1)) . read::String->Int) ls)
               }
      _ <- eof -- newline
      return res

parseAlign
  :: (NFData l, Show l)
  => (u -> TIO.Text -> (u, l)) -> u -> TIO.Text -> (u, [Alignment l])
parseAlign mapper ustate contents
  = go ustate $ zip [(0 :: Int)..] (TIO.lines contents)
  where
    go u [] = (u, [])
    go u ((i, x):xs) =
      case runParser p u ("line " ++ show i ++ show x) x of
        Right (u', x') -> let (u'', xs') = go u' xs
                          in (u'', x':xs')
        Left x -> error (show x)
    p = do
      x' <- alignParser (\ u -> mapper u . TIO.pack) -- (p_tag f)
      u' <- getState
      return (u', makeItSo x')
    {-

  = second (map makeItSo) . lazyMany (alignParser mapper) "align" u
    where
    -}
    makeItSo :: [(l, [Int])] -> Alignment l
    makeItSo
      = uncurry Alignment
      . (foldl f M.empty . zip [(0::Int)..] . snd &&& V.fromList . fst)
      . unzip
    f m (ind, als) = foldl (f' ind) m als
    f' ind m i = M.insertWith' S.union i (S.singleton ind) m

{-
lazyMany
  :: NFData a => GenParser u a -> SourceName -> u -> TIO.Text -> (u, [a])
lazyMany p file ustate contents
  = either (error . show) id $ runParser mp ustate file contents
  where
    mp = do
      xs <- many p -- many p
      u <- getState
      return $! (u, id $!! xs)
-}

printTree :: Show l => T.Tree (Either Int l) -> String
printTree (T.Node (Right l) ts)
  = "(" ++ show l ++ (if null ts then "" else " ")
    ++ unwords (map printTree ts) ++ ")"
printTree (T.Node (Left l) _)
  = "x" ++ show l

printString :: Show l => [Either Int l] -> String
printString = unwords . map p
  where
    p (Left i) = "x" ++ show i
    p (Right l) = show l

printRule :: (Show l, Show v) => Rule v l -> String
printRule (Rule e)
  = show (to e)
    ++ " -> <" ++ printTree (fst (Hypergraph.label e)) ++ ", "
    ++ printString (snd (Hypergraph.label e)) ++ ", " ++ (show (from e))
    ++ ">  #  " ++ show (ident e)

printTreeTA :: TokenArray -> T.Tree (Either Int Token) -> String
printTreeTA ta (T.Node (Right l) ts)
  = "(" ++ TIO.unpack (getString ta l) ++ (if null ts then "" else " ")
    ++ unwords (map (printTreeTA ta) ts) ++ ")"
printTreeTA _ (T.Node (Left l) _)
  = "x" ++ show l

printStringTA :: TokenArray -> [Either Int Token] -> String
printStringTA ta = unwords . map p
  where
    p (Left i) = "x" ++ show i
    p (Right l) = TIO.unpack $ getString ta l

printRuleTA :: TokenArray -> Rule Token Token -> String
printRuleTA ta (Rule e)
  = TIO.unpack (getString ta (to e))
    ++ " -> <" ++ printTreeTA ta (fst (Hypergraph.label e)) ++ ", "
    ++ printStringTA ta (snd (Hypergraph.label e)) ++ ", "
    ++ (show (map (getString ta) (from e)))
    ++ ">  #  " ++ show (ident e)

instance (Eq v, Eq l) => Eq (Rule v l) where
  Rule e1 == Rule e2 -- do not compare ident
    = (to e1, Hypergraph.label e1, from e1)
      == (to e2, Hypergraph.label e2, from e2)

instance (Ord v, Ord l) => Ord (Rule v l) where
  Rule e1 `compare` Rule e2
    = (to e1, Hypergraph.label e1, from e1)
      `compare` (to e2, Hypergraph.label e2, from e2)

{-instance Eq l => Eq (T.Tree l) where
  T.Node l1 ts1 == T.Node l2 ts2
    = l1 == l2 && ts1 == ts2-}

instance Ord l => Ord (T.Tree l) where
  T.Node l1 ts1 `compare` T.Node l2 ts2
    = case (l1 `compare` l2, ts1 `compare` ts2) of
        (LT, _) -> LT
        (EQ, LT) -> LT
        (EQ, EQ) -> EQ
        _ -> GT

-- instance Show (TokenMap) where
--   show m = show $ toMap m


main :: IO ()
main = do
  arg0 <- getProgName
  args <- getArgs
  case args of
    ["-m", mapFile, "-p", parseFile, "-a", alignFile, "-o", outFile] -> do
      mapContents <- TIO.readFile mapFile
      parseContents <- TIO.readFile parseFile
      alignContents <- TIO.readFile alignFile
      let tmap = fromText mapContents :: TokenMap
      case parseAlign updateToken tmap alignContents of
        (tmap', alignments) -> do
          let parseTrees = parsePenn parseContents :: [T.Tree Token]
          let rules
                = S.toList $ S.fromList
                $ extractRules $ zip alignments parseTrees
          let rul = [ mkHyperedge (to e) (from e) (Hypergraph.label e) i 
                    | (r, i) <- zip rules [0..]
                    , let e = unRule r
                    ]
          -- TIO.writeFile outFile (TIO.unlines (map (TIO.pack . printRule) rules))
          -- TIO.writeFile (outFile ++ ".string") (TIO.unlines (map (TIO.pack . printRuleTA (toArray tmap')) rules))
          let hyp = mkHypergraph rul 
                :: EdgeList Token (T.Tree (Either Int Token), [Either Int Token]) Int
          B.writeFile (outFile ++ ".bhg.gz") (compress $ B.encode hyp)
          TIO.writeFile (mapFile ++ ".new") (toText tmap')
    _ -> putStrLn ("Usage: " ++ arg0 ++ " -m mapfile -p parses -a align -o output")
