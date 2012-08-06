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
import Data.Function ( on )
import Data.Int ( Int32 )
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

-- | a parse tree
-- each node carries its span and span closure,
-- as well as string position (for terminal nodes)
data TreeNode l
  = TreeNode
    { label :: l
    , span :: S.Set Int
    , clos :: Maybe (Int, Int)
    , idx :: Int
    }
  deriving Show

data TreeNode2 l
  = TreeNode2
    { treeNode :: TreeNode l
    , complSpan :: S.Set Int
    , frontier :: Bool
    }
  deriving Show

data Alignment l
  = Alignment
    { almap :: M.Map Int (S.Set Int)  -- map an Engl. index to Fr. indices
    , snt :: V.Vector l
    }
  deriving Show

instance NFData l => NFData (TreeNode l) where
  rnf tn = rnf (label tn)
     `seq` rnf (span tn)
     `seq` rnf (clos tn)
     `seq` rnf (idx tn)

instance NFData l => NFData (TreeNode2 l) where
  rnf tn2 = rnf (treeNode tn2)
      `seq` rnf (complSpan tn2)
      `seq` rnf (frontier tn2)

{- instance NFData l => NFData (T.Tree l) where
  rnf (T.Node l ts) = rnf l `seq` rnf ts -}

instance NFData x => NFData (V.Vector x) where
  rnf s = rnf (V.toList s)

-- instance NFData x => NFData (S.Set x) where
  -- rnf s = rnf (S.toList s)

{-instance (NFData k, NFData v) => NFData (M.Map k v) where
  rnf m = rnf (M.toList m)-}

instance NFData l => NFData (Alignment l) where
  rnf al = rnf (almap al) `seq` rnf (snt al)

-- | Depth-first left-to-right traversal of a tree (via the State monad),
-- adding the spans, span closures, and index information to each node
{-process :: Alignment l -> T.Tree l -> T.Tree (TreeNode l)
process = curry $ (`evalState` 0) . uncurry p-}
process :: Alignment l -> T.Tree l -> (Alignment l, T.Tree (TreeNode l))
process al t = (al, evalState (p t) 0)
  where
    p :: {- Alignment l -> -} T.Tree l -> State Int (T.Tree (TreeNode l))
    p (T.Node l ts)
      | null ts   = do
                      i <- get
                      put (i+1)
                      let sp = M.findWithDefault S.empty i (almap al)
                      return $ T.Node (TreeNode l sp (closure sp) i) []
      | otherwise = do
                      ts' <- mapM p ts
                      let sp = foldl' S.union S.empty
                             $ map (span . T.rootLabel) ts'
                      return $ T.Node (TreeNode l sp (closure sp) (-1)) ts'
    closure sp
      | S.null sp = Nothing
      | otherwise = Just (S.findMin sp, S.findMax sp)

-- | Recursive tree traversal to add complement span and frontier property
process2 :: Show l => T.Tree (TreeNode l) -> T.Tree (TreeNode2 l)
process2 = {-mytrace .-} p S.empty [] []
  where
    p :: S.Set Int    -- parent complement span
      -> [T.Tree (TreeNode l)] -- left siblings
      -> [T.Tree (TreeNode l)] -- right siblings
      -> T.Tree (TreeNode l)   -- tree
      -> T.Tree (TreeNode2 l)
    p par ls rs (T.Node tn ts)
      = T.Node
          (TreeNode2 tn cs (S.null (span tn `S.intersection` cs)))
          (q cs ts [])
        where
          cs = par `un` ls `un` rs
          un = foldl' (\ sp t -> sp `S.union` span (T.rootLabel t))
          -- (foldl' S.union par ls) rs
    q :: S.Set Int
      -> [T.Tree (TreeNode l)]
      -> [T.Tree (TreeNode l)]
      -> [T.Tree (TreeNode2 l)]
    q _ [] _ = []
    q par (t:ts) ts' = p par ts ts' t : q par ts (t:ts')


type Sync l = (T.Tree (Either Int l), [Either Int l])
newtype Rule v l = Rule { unRule :: Hyperedge v (Sync l) Int }
  deriving NFData

-- instance (NFData v, NFData l) => NFData (Rule v l) where
--   rnf = rnf . unRule


process3
  :: Show l
  => Alignment l
  -> Bool                      -- are we at the root?
  -> T.Tree (TreeNode2 l)
  -> State Int
      ( T.Tree (Either Int l)  -- left-hand side
      , [(Int, Maybe (Int, Int))]  -- rhs variables with span closure
      , IM.IntMap l            -- state assignment
      , [T.Tree (TreeNode2 l)] -- remaining trees
      )
process3 al atroot t@(T.Node tn2 ts)
  | not atroot && frontier tn2 = do
      i <- get
      put (i+1)
      return ( T.Node (Left i) []
             , [(i, clos (treeNode tn2))]
             , IM.singleton i (label (treeNode tn2))
             , [t]
             )
  | otherwise = do
      (lhss, rhss, stmaps, rs) <- fmap unzip4 $ mapM (process3 al False) ts
      return ( T.Node (Right (label (treeNode tn2))) lhss 
             , concat rhss
             , foldl' IM.union IM.empty stmaps
             , concat rs
             )

process3'
  :: Alignment l
  -> T.Tree (TreeNode2 l)
  -> [(Int, Maybe (Int, Int))]
  -> [Either Int l]
process3' al (T.Node tn2 _)
  = case clos (treeNode tn2) of
      Nothing -> map (Left . fst)
      Just (lspan, rspan)
        -> go lspan
           . (++ [(-1, Just (rspan+1, undefined))]) -- add sentinel element
  where
    sp = span (treeNode tn2)
    go lspan ((i, Nothing) : xs)
      = Left i : go lspan xs
    go lspan ((-1, Just (rspan, _)) : xs)
      = [ Right (snt al V.! j) | j <- [lspan..rspan-1], j `S.member` sp ]
    go lspan ((i, Just (rspan, rspan')) : xs)
      = [ Right (snt al V.! j) | j <- [lspan..rspan-1], j `S.member` sp ]
        -- ++ if i /= -1 then [ Left i ] else []
        ++ [ Left i ]
        ++ go (rspan'+1) xs

newtype MyPair a b = MyPair { unMyPair :: (a, b) }

instance Eq b => Eq (MyPair a b) where
  MyPair (_, b1) == MyPair (_, b2) = b1 == b2

instance Ord b => Ord (MyPair a b) where
  MyPair (_, b1) `compare` MyPair (_, b2) = b1 `compare` b2

seqEither :: Strategy a -> Strategy b -> Strategy (Either a b)
seqEither sa _ (Left a)  = sa a
seqEither _ sb (Right b) = sb b

{-process4
  :: (NFData l, Show l)
  => [(Alignment l, T.Tree (TreeNode2 l))] -> State Int [Rule l l]
process4 [] = return []
process4 ((al, t) : ats)
  = case runState (process3 al True t) 0 of
      ((lhs, rhslist, stmap, rs'), n) -> do
        let rhs = (process3' al t $ sortBy (compare `on` snd) rhslist)
                  -- `using` seqList (seqEither rseq rseq)
        -- let rhs = fst . unzip . map unMyPair
        --        . S.toAscList . S.fromList . map MyPair $ rhs'
          -- = fst $ unzip $ sortBy (compare `on` snd) rhs'
        i <- get
        put (i+1)
        let r = id $!! Rule $ mkHyperedge
                  (label (treeNode (T.rootLabel t)))
                  (map (stmap IM.!) [(0::Int) .. n-1])
                  (lhs, rhs)
                  i
        rs <- r `seq` process4 $ zip (repeat al) rs'
        rs2 <- process4 ats
        return (r : rs ++ rs2)-}

process4
  :: (NFData l, Show l)
  => [(Alignment l, T.Tree (TreeNode2 l))] -> [Rule l l]
process4 [] = []
process4 ((al, t) : ats)
  = case runState (process3 al True t) 0 of
      ((lhs, rhslist, stmap, rs'), n) ->
        let rhs = (process3' al t $ sortBy (compare `on` snd) rhslist)
                  -- `using` seqList (seqEither rseq rseq)
            r = id $!! Rule $ mkHyperedge
                  (label (treeNode (T.rootLabel t)))
                  (map (stmap IM.!) [(0::Int) .. n-1])
                  (lhs, rhs)
                  0
            rs = process4 $ zip (repeat al) rs'
            rs2 = process4 ats
        in r `seq` (r : rs ++ rs2)

{-

p_mapv
  :: (u -> String -> (u, l))
  -> String
  -> GenParser (Int, u) l
p_mapv mapper !s = do
  u <- fmap snd getState
  let (u', v) = mapper u s
  updateState $ second (const u')
  return v


treeParser
  :: (u -> String -> (u, l))
  -> Alignment l
  -> GenParser (Int, u) (T.Tree (TreeNode l))
treeParser mapper al@Alignment{}
  = do
    { _ <- char '('
    -- ; spaces
    ; ide <- p_mapv mapper =<< many (noneOf " )")  -- was: many1
    ; spaces
    ; trees <- many (treeParser mapper al)
    ; _ <- char ')'
    -- ; spaces
    ; let span = foldl S.union S.empty $ map (span . T.rootLabel) trees
      in return $ T.Node (TreeNode ide span (closure span) (-1)) trees
    } <|>
    do
    { ide <- p_mapv mapper =<< many1 (noneOf " )")
    ; spaces
    ; pos <- fmap fst getState
    ; updateState $ first (+1)
    ; let span = M.findWithDefault S.empty pos (almap al)
      in return $ T.Node (TreeNode ide span (closure span) pos) []
    }
    where
      closure span
        | S.null span = Nothing
        | otherwise = Just (S.findMin span, S.findMax span)

mkTree :: Alignment l -> TIO.Text -> Maybe (T.Tree (TreeNode l))
mkTree al@Alignment{}
  = either (const Nothing) Just . runParser (treeParser mapper al) 1 ""
-}

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
  => (u -> String -> (u, l)) -> u -> TIO.Text -> (u, [Alignment l])
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
      x' <- alignParser mapper -- (p_tag f)
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
  = "(" ++ getString ta l ++ (if null ts then "" else " ")
    ++ unwords (map (printTreeTA ta) ts) ++ ")"
printTreeTA _ (T.Node (Left l) _)
  = "x" ++ show l

printStringTA :: TokenArray -> [Either Int Token] -> String
printStringTA ta = unwords . map p
  where
    p (Left i) = "x" ++ show i
    p (Right l) = getString ta l

printRuleTA :: TokenArray -> Rule Token Token -> String
printRuleTA ta (Rule e)
  = getString ta (to e)
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
          let parseTrees = parsePenn parseContents :: [T.Tree Int32]
          let rules = S.toList . S.fromList
                    $ -- evalState
                        ( process4
                          ( map
                            ( second process2
                            . uncurry process
                            )
                            ( zip
                              alignments
                              parseTrees
                            )
                          )
                        )
                        -- 0
                    {-$ evalState
                        ( process4
                          ( zip
                            alignments
                          $!! ( map
                              ( process2
                              . uncurry process
                              )
                              ( zip
                                alignments
                                parseTrees
                              )
                            )
                          )
                        )
                        0-}
          let rul = [ mkHyperedge (to e) (from e) (Hypergraph.label e) i 
                    | (r, i) <- zip rules [0..]
                    , let e = unRule r
                    ]
          TIO.writeFile outFile (TIO.unlines (map (TIO.pack . printRule) rules))
          TIO.writeFile (outFile ++ ".string") (TIO.unlines (map (TIO.pack . printRuleTA (toArray tmap')) rules))
          -- let hyp = mkHypergraph rul 
          --       :: EdgeList Token (T.Tree (Either Int Token), [Either Int Token]) Int
          -- B.writeFile (outFile ++ ".bhg.gz") (compress $ B.encode hyp)
          -- TIO.writeFile (mapFile ++ ".new") (toText tmap')
          -- TIO.writeFile (outFile ++ ".bhg.gz") 
              -- (TIO.unlines $ map (TIO.pack. show . unRule) rules)
          -- print $ tmap 
          -- print $ (TIO.unpack parseContents) 
          -- print $ (TIO.unpack alignContents) 
    _ -> putStrLn ("Usage: " ++ arg0 ++ " -m mapfile -p parses -a align -o output")
    -- putStrLn $ show $ length parseTrees
