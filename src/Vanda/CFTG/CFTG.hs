{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CFTG.CFTG
-- Copyright   :  (c) Technische Universität Dresden 2015
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module Vanda.CFTG.CFTG
( SF(..)
, variables
, Rule(..)
, isLinear
, isNondeleting
, substitute
, applyAt
, language
, Derivation
, language'
, mainDerive
, -- * Conversion
  toTree
, treeToSF
, -- * Miscellaneous
  drawTreeCompact
) where


import qualified Control.Error
import           Vanda.Util.Tree


import           Control.Arrow
import           Data.Char (isUpper)
import           Data.Either
import           Data.List ((\\), findIndices, intercalate, mapAccumL)
import qualified Data.Map.Lazy as M
import           Data.Tree
import           Text.Read (readMaybe)


errorHere :: String -> String -> a
errorHere = Control.Error.errorHere "Vanda.CFTG.CFTG"


data SF v n t = V v
              | N n [SF v n t]
              | T t [SF v n t]
              deriving (Eq, Show)


children :: SF v n t -> [SF v n t]
children (V _   ) = []
children (N _ cs) = cs
children (T _ cs) = cs


variables :: SF v n t -> [v]
variables (V v   ) = [v]
variables (N _ cs) = concatMap variables cs
variables (T _ cs) = concatMap variables cs


toTree :: (v -> a) -> (n -> a) -> (t -> a) -> SF v n t -> Tree a
toTree fV fN fT = go
  where
    go (V v   ) = Node (fV v) []
    go (N n ts) = Node (fN n) $ map go ts
    go (T t ts) = Node (fT t) $ map go ts


-- ^ Convert a 'Tree' over 'String' into 'SF'.
-- 'Node'’s 'rootLabel's beginning with capital letter are interpreted as
-- non-terminal symbols, an underscore at the beginning marks a variable.
-- Everything else is interpreted as terminal symbol.
treeToSF :: Tree String -> SF String String String
treeToSF (Node cs@('_' : _) []) = V cs
treeToSF (Node cs@('_' : _) _ ) = errorHere "treeToSF" (cs ++ " is no leaf")
treeToSF (Node cs@(c   : _) ts) | isUpper c = N cs $ map treeToSF ts
treeToSF (Node cs           ts) = T cs $ map treeToSF ts


data Rule v n t = Rule
  { lhs  :: n
  , vars :: [v]
  , rhs  :: SF v n t
  }


isLinear :: Eq a => Rule a t t1 -> Bool
isLinear (Rule _ vs sf) = null (variables sf \\ vs)


isNondeleting :: Eq a => Rule a t t1 -> Bool
isNondeleting (Rule _ vs sf) = null (vs \\ variables sf)


apply :: (Ord v, Eq n) => Rule v n t -> SF v' n t -> SF v' n t
apply (Rule l vs r) sf@(N root _)
  |  root == l
  && equalLength vs (children sf)
  = substitute varM r
  where
    varM = M.fromListWith (error "Non-linear lhs.")
         $ zip vs (children sf)
apply _ _ = error "Rule not applicable."


applyAt :: (Ord v, Eq n) => [Int] -> Rule v n t -> SF v' n t -> SF v' n t
applyAt [] r sf = apply r sf
applyAt (_ : _ ) _ (V _) = errorHere "applyAt" "invalid position"
applyAt (i : is) r (N n ts) = N n $ mapAt i (applyAt is r) ts
applyAt (i : is) r (T t ts) = T t $ mapAt i (applyAt is r) ts


mapAt :: Int -> (a -> a) -> [a] -> [a]
mapAt n f (x : xs)
  = case compare n 0 of
      GT -> x : mapAt (n - 1) f xs
      EQ -> f x : xs
      LT -> errorHere "mapAt" "negative index"
mapAt _ _ []
  = errorHere "mapAt" "index to large"


substitute :: Ord v => M.Map v (SF v' n t) -> SF v n t -> SF v' n t
substitute varM = go
  where
    go (V v   ) = M.findWithDefault (error "Unassigned var.") v varM
    go (N n ts) = N n $ map go ts
    go (T t ts) = T t $ map go ts


equalLength :: [a] -> [b] -> Bool
equalLength (_ : xs) (_ : ys) = equalLength xs ys
equalLength []       []       = True
equalLength _        _        = False


language :: (Ord v, Eq n) => [Rule v n t] -> SF v'' n t -> [SF v' n' t]
language = (map snd .) . language'


type Derivation = [([Int], Int)]


language'
  :: forall v v' v'' n n' t
   . (Ord v, Eq n)
  => [Rule v n t] -> SF v'' n t -> [(Derivation, SF v' n' t)]
language' rs start
  = go [([], start)]
  where
    go :: [(Derivation, SF v'' n t)] -> [(Derivation, SF v' n' t)]
    go []  = []
    go sfs = let complete'' (d, sf) = either (Left . (,) d) (Right . (,) d) (complete' sf)
                 (todo, done) = partitionEithers $ map complete'' sfs
             in done ++ go (todo >>= (\ (d, sf) -> fmap (first (d ++)) (dive sf)))

    dive :: SF v'' n t -> [(Derivation, SF v'' n t)]
    dive (V _   ) = error "Variable in sentential form."
    dive (T t ts) = topConcat t `fmap` sequence (fmap dive ts)
    dive (N n ts)
      = [ ([([], i)], substitute varM r)
        | (i, Rule l vs r) <- zip [0 ..] rs
        , l == n
        , equalLength vs ts
        , let varM = M.fromListWith (error "Non-linear lhs.") $ zip vs ts
        ]

    topConcat :: t -> [(Derivation, SF v'' n t)] -> (Derivation, SF v'' n t)
    topConcat t
      = ((concat . zipWith (map . first . (:)) [0 ..]) *** T t)
      . unzip


complete' :: SF v n t -> Either (SF v n t) (SF v' n' t)
complete' sf = maybe (Left sf) Right (complete sf)


complete :: SF v n t -> Maybe (SF v' n' t)
complete (T t ts) = T t `fmap` mapM complete ts
complete _        = Nothing


drawTreeCompact :: Tree String -> String
drawTreeCompact = drawTree' (drawstyleCompact1 "──╴")


colorTTY :: [Int] -> String -> String
colorTTY cols str
  = "\ESC[" ++ intercalate ";" (map show cols) ++ "m" ++ str ++ "\ESC[m"


mainDerive
  :: (Ord v)
    => [Rule v String String] -> SF v' String String -> IO (SF v' String String)
mainDerive rs sf = do
  let (nM, labeledSf) = mapAccumN f M.empty sf
      f a pos n = let i  = succ (M.size a)
                      a' = M.insert i (n, pos) a
                  in (a', (i, n))
  putStr
    $ drawTreeCompact
    $ toTree undefined
             (\ (i, n) -> colorTTY [96] n ++ " (" ++ show i ++ ")")
             (colorTTY [93])
    $ labeledSf
  if M.null nM
    then return sf
    else do
      i <- askOption "Which position shall be expanded?" (M.keys nM)
      let (n, pos) = nM M.! i
      r <- fmap ((rs !!) . pred)
         $ askOption "Which rule shall be applied?"
         $ map succ
         $ findIndices ((n ==) . lhs) rs
      let sf' = applyAt pos r sf
      mainDerive rs sf'


askOption :: (Eq a, Read a, Show a) => String -> [a] -> IO a
askOption question answers@[answer] = do
  printOptions question answers
  print answer
  return answer
askOption question answers = do
  printOptions question answers
  answerLine <- getLine
  case readMaybe answerLine of
    Just answer | answer `elem` answers -> return answer
    _ -> askOption question answers

printOptions :: Show a => String -> a -> IO ()
printOptions question answers
  = putStr $ question ++ " " ++ show answers ++ " "


mapAccumN :: (a -> [Int] -> n -> (a, n')) -> a -> SF v n t -> (a, SF v n' t)
mapAccumN f = go []
  where
    go _  acc (V v   ) = (acc, V v)
    go ps acc (T t ts) = second (T t ) $ mapAccumL (step ps) acc  (zip ts [0 ..])
    go ps acc (N n ts) = second (N n') $ mapAccumL (step ps) acc' (zip ts [0 ..])
      where (acc', n') = f acc ps n

    step ps a (t, p) = go (ps ++ [p]) a t
