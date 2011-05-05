-- (c) 2010 Daniel Geisler
-- (c) 2010 Matthias Büchse <Matthias.Buechse@tu-dresden.de>
-- (c) 2011 Toni Dietze <Toni.Dietze@tu-dresden.de>
--
-- Technische Universität Dresden / Faculty of Computer Science / Institute
-- of Theoretical Computer Science / Chair of Foundations of Programming
--
-- Redistribution and use in source and binary forms, with or without
-- modification, is ONLY permitted for teaching purposes at Technische
-- Universität Dresden AND IN COORDINATION with the Chair of Foundations
-- of Programming.
-- ---------------------------------------------------------------------------

module Algorithms.NBest
( best, worst
, best', worst'
) where

import Tools.Miscellaneous (mapSnd)

import Data.List
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.Tree as T
import Maybe
-- import Array
-- import System( getArgs )
-- import Debug.Trace
import qualified Data.Heap as Heap
import Data.Heap ( MinHeap )
-- import Parser.HGraphLoader
-- import Control.Parallel.Strategies




type HWeights hEdge hWeight = hEdge -> [hWeight] -> hWeight
type HBack hEdge hNode = hNode -> [(hEdge, [hNode])]
type HGraph hNode hEdge hWeight
      = ([hNode], HBack hEdge hNode, HWeights hEdge hWeight)
data Pair a b = P a b deriving Show

{-
showHGraph
  :: (Show hNode, Show hEdge, Show hWeight)
  => HGraph hNode hEdge hWeight -> String
showHGraph (hNodes, hBack, hWeights)
  = show hNodes ++ "\n" ++ f' ++ "\n" ++ "kanten: " ++ show cnt ++ "\n"
  where
    -- b = map (\x -> (show x) ++ "-->" ++ show (hBack x)) hNodes
    cnt = sum $ map (\x -> length (hBack x)) hNodes
    f = map
          ( \ x -> map
            (\ y@(sym, _) ->
                  show x
              ++  "-->"
              ++  show y
              ++  "   weight:"
              ++  show (hWeights sym [])
            )
            (hBack x)
          )
          hNodes
    f' = concat $ intersperse "\n" (concat f)
-}
--instance (Show hNode,Show hEdge,Show hWeight) => Show (HGraph hNode hEdge hWeight) where
--    show (hNodes,hBack,hWeights) = "(" ++ show hNodes ++ ")"

instance (Eq a, Eq b) => Eq (Pair a b) where
    (==) (P a b) (P a' b') = (b == b') && (a == a')

instance (Eq a,Ord b) => Ord (Pair a b) where
    compare (P _ b) (P _ b') = compare b b'


-- our merge data structure for the heap
data M a = E | M a [M a]

instance Ord a => Eq (M a) where
  E == E = True
  M a1 _ == M a2 _ = a1 == a2
  _ == _ = False

instance Ord a => Ord (M a) where
  compare E E = EQ
  compare E _ = LT
  compare _ E = GT
  compare (M a1 _) (M a2 _) = compare a1 a2


lft
  :: HGraph hNode hEdge hWeight
  -> HGraph hNode hEdge (Pair (T.Tree hEdge) hWeight)
lft  (hNodes, hBack, hWeights) = (hNodes, hBack, hWeights')
  where
    hWeights' hEdge pairs = P (T.Node hEdge l1) (hWeights hEdge l2)
      where
        (l1, l2) =  foldr (\ (P a b) (as, bs) -> (a:as, b:bs)) ([], []) pairs


--------------------------------------------------------------------
--      Calculation of best hyperpath                             --
--------------------------------------------------------------------

knuth
  :: (Ord hWeight, Ord hNode, Ord hEdge)
  => HGraph hNode hEdge hWeight -> hNode -> Maybe (hWeight, hEdge)
knuth h@(hNodes, _, _)
  = (Map.!)
  $ knuth' h
      ( Map.fromList [(v, Nothing) | v <- hNodes]
      , Set.fromList hNodes
      , Set.empty
      )


knuth'
  :: (Ord hWeight, Ord hNode, Ord hEdge)
  => HGraph hNode hEdge hWeight
  -> (Map hNode (Maybe (hWeight, hEdge)), Set hNode, Set hNode)
  -> Map hNode (Maybe (hWeight, hEdge))
knuth' h@(_, hBack, hWeights) (oldMap, unvis, vis)
  | Set.null unvis
    = oldMap
  | otherwise
    = knuth' h
        ( Map.insert v m oldMap
        , Set.delete v unvis
        , Set.insert v vis
        )
  where
    f sym weights = do
      b <- sequence weights
      let (ws, _) = unzip b
      return (hWeights sym ws, sym)
    list
      = [ (v', f sym (map ((Map.!) oldMap) srcs))
        | v' <- Set.toList unvis
        , (sym, srcs) <- hBack v'
        , Set.isSubsetOf (Set.fromList srcs) vis
        ]
    list' = filter (\ (_, m') -> isJust m') list
    (v, m)
      = minimumBy (\ (_, Just (x, _)) (_, Just (x', _)) -> compare x x') list'

{-
kknuth
  :: (Ord hWeight, Ord hNode, Ord hEdge)
  => HGraph hNode hEdge hWeight -> hNode -> Maybe (hWeight, hEdge)
kknuth h@(hNodes, _, _)
  = (Map.!) m
  where (m, _, _) = kknuth' h hNodes


kknuth'
  ::  (Ord hWeight, Ord hNode, Ord hEdge)
  => HGraph hNode hEdge hWeight
  -> [hNode]
  -> (Map hNode (Maybe (hWeight,hEdge)), Set hNode, Set hNode)
kknuth' (hNodes, _, _) []
  = ( Map.fromList [(v, Nothing) | v <- hNodes]
    , Set.fromList hNodes
    , Set.empty
    )
kknuth' h@(_, hBack, hWeights) (_:xs)
  | null list'
    = (oldMap, unvis, vis)
  | otherwise
    = ( Map.insert v m oldMap
      , Set.delete v unvis
      , Set.insert v vis
      )
  where
    (oldMap, unvis, vis) = kknuth' h xs
    f sym weights = do
      b <- sequence weights
      let (ws, _) = unzip b
      return (hWeights sym ws, sym)
    list
      = [ (v', f sym (map ((Map.!) oldMap) srcs))
        | v' <- Set.toList unvis
        , (sym, srcs) <- hBack v'
        , Set.isSubsetOf (Set.fromList srcs) vis
        ]
    list' = filter (\ (_, m') -> isJust m') list
    (v, m)
      = minimumBy (\ (_, Just (x, _)) (_, Just (x', _)) -> compare x x') list'
-}

--------------------------------------------------------------------
--      Calculation of n-best hyperpaths                          --
--------------------------------------------------------------------

q :: (Ord hWeight, Ord hNode, Ord hEdge)
  => HGraph hNode hEdge hWeight
  -> (hNode -> Maybe (hWeight, hEdge))
  -> hNode
  -> [hWeight]
q h@(hNodes, hBack, hWeights) kn
  = (Map.!) m
  where
    m = Map.fromList [(hNode, q' hNode) | hNode <- hNodes]
    q' v
      = case kn v of
          Nothing -> []
          Just (w, edge) ->
              w
            : p (Heap.fromList (
                    [ topconcat (hWeights sym) (map (q h kn) srcs)
                    | (sym, srcs) <- hBack v
                    , sym /= edge
                    ]
                ++  ( mytail
                    $ head
                      [ topconcat (hWeights sym) (map (q h kn) srcs)
                      | (sym, srcs) <- hBack v
                      , sym == edge
                      ]
                    )
                ))


p :: (Ord hWeight) => MinHeap (M hWeight) -> [hWeight]
p heap
  = case Heap.view heap of
      Nothing -> []
      Just (E, heap') -> p heap'
      Just (M a l, heap') -> a : p (Heap.union heap' (Heap.fromList l))


topconcat :: ([hWeight] -> hWeight) -> [[hWeight]] -> M hWeight
topconcat f lists
  | any null lists
    = E
  | otherwise
    = M (f (map head lists)) (map (topconcat f) (tail (combine lists)))
  where
    combine :: [[a]] -> [[[a]]]
    combine [] = [[]]
    combine (x:xs) = map (x:) c ++ [tail x : map (\ x' -> [head x']) xs]
      where c = combine xs


mytail :: M a -> [M a]
mytail E = []
mytail (M _ as) = as


-- | @best hypergraph vertex n@ computes the weights of the @n@ best
-- executions of @hypergraph@ beginning at @vertex@, where best means the
-- executions with the lowest weights with respect to 'Ord'.
best
  :: (Ord hWeight, Ord hNode, Ord hEdge)
  => HGraph hNode hEdge hWeight
  -> hNode
  -> Int
  -> [hWeight]
best h v n = take n (q h (knuth h) v)


-- | @best' hypergraph vertex n@ computes the @n@ best executions and their
-- weights of @hypergraph@ beginning at @vertex@, where best means the
-- executions with the lowest weights with respect to 'Ord'.
best'
  :: (Ord hEdge, Ord hWeight, Ord hNode)
  => HGraph hNode hEdge hWeight
  -> hNode
  -> Int
  -> [(T.Tree hEdge, hWeight)]
best' h v n
  = map (\ (P t w) -> (t, w))
  $ take n (q h' (knuth h') v)
  where
    h' = lft h


newtype FlipOrd a = FlipOrd { unflipOrd :: a } deriving (Eq)

instance (Ord a) => Ord (FlipOrd a) where
  compare (FlipOrd x) (FlipOrd y) = compare y x


-- | @worst hypergraph vertex n@ computes the weights of the @n@ worst
-- executions of @hypergraph@ beginning at @vertex@, where worst means the
-- executions with the highest weights with respect to 'Ord'.
worst
  :: (Ord hWeight, Ord hNode, Ord hEdge)
  => HGraph hNode hEdge hWeight
  -> hNode
  -> Int
  -> [hWeight]
worst (hNodes, hBack, hWeights) v n
  = map unflipOrd
  $ best (hNodes, hBack, \ e -> FlipOrd . hWeights e . map unflipOrd) v n


-- | @worst' hypergraph vertex n@ computes the @n@ worst executions and their
-- weights of @hypergraph@ beginning at @vertex@, where worst means the
-- executions with the highest weights with respect to 'Ord'.
worst'
  :: (Ord hEdge, Ord hWeight, Ord hNode)
  => HGraph hNode hEdge hWeight
  -> hNode
  -> Int
  -> [(T.Tree hEdge, hWeight)]
worst' (hNodes, hBack, hWeights) v n
  = map (mapSnd unflipOrd)
  $ best' (hNodes, hBack, \ e -> FlipOrd . hWeights e . map unflipOrd) v n

{-
main :: IO ()
main = do
  args <- getArgs
  let (file, n) = (head args, (read (head (tail args))) :: Int)
  -- m <- loadHGraph file
  m <- loadHGraph file
  case m of
    Right (node, hGraph) -> print (best' hGraph node n)
    -- Right (node,hGraph) -> print (knuth hGraph node)
    -- Right (node,hGraph) -> putStr $ showHGraph hGraph
    Left err -> print err
-}
{-
-# OPTIONS_GHC -fglasgow-exts -XBangPatterns -XGADTs #-

knuth :: (Ord hWeight, Ord hNode, Ord hEdge) => HGraph hNode hEdge hWeight -> hNode -> (Maybe (hWeight,hEdge))
knuth h@(hNodes,hBack,hWeights)= (Map.!) $ knuth' ((Map.fromList [(v, Nothing) | v <- hNodes]), (Set.fromList hNodes), Set.empty)
  where
            blaa = concatMap (\x -> map  (\(s,ls) -> (x,s,ls,length ls)) $ hBack x  ) hNodes
            counter1 = Map.fromList $ map (\(_,s,_,l)-> (s,l)) blaa
            counter2 = Map.fromListWith (++) $ map (\(_,s,_,l)-> (l,s)) blaa
            locked =  foldl (\set (n,s,ns,r) -> Map.insertWith (++) n [s] set ) (Map.empty) blaa
            aux = Map.fromListWith (++) (concatMap (\(n,s,ns,r)-> map (\n'->(n',[s])) ns) blaa)
            unlocked = Map.fromList $ zip hNodes (repeat Set.empty)
            step (lck,unlck,cntr1,cntr2,ax) =
                where

            knuth'  (oldMap,unvis,vis)
                | Set.null unvis = oldMap
                | otherwise  = knuth' ((Map.insert v m oldMap), (Set.delete v unvis), (Set.insert v vis))
              where
              f sym weights = do b <- (sequence weights)
                                 let (ws,_) = unzip b
                                 return ((hWeights sym ws),sym)
              list = [(v', (f sym ((map ((Map.!) oldMap) srcs)))) | v' <- Set.toList unvis, (sym,srcs) <- (hBack v'), (Set.isSubsetOf (Set.fromList srcs) vis)]
              list'= filter (\(_,m) -> isJust m) list
              (v,m) = minimumBy (\(_,(Just (x,_))) (_,(Just (x',_)))-> compare x x') list'

      --        locked :: Map hNode -> [hEdge]
       --       aux :: Map hNode -> [hEdge]
        --      unlocked :: Map hNode -> Set hEdge













test = do
            l <- loadHGraph "/home/daniel/weighted_trainable.rtg"
            case l of
           --     (Right (a,b)) -> putStr $ showHGraph b
                  (Left err) -> print err
                  (Right err) -> putStrLn (showHGraph $ snd err)


p :: Vertex -> [BufferedHyperpath]
p vertex = a!vertex
  where
  {
      a = array (minimum vertices,maximum vertices) [(v,p' v) | v <- vertices];
      p' v = if (knuth!v) == Nothing then [] else ((fromJust (knuth!v)):(merge ((first v) ++ (second v))));
      first v= [tail (topconcat symbol' (map p source) ) | (symbol',source,destination) <- hyperedges, symbol' == (topsymbol (knuth!v)), v == destination  ];
      second v = [topconcat symbol' (map p source) | (symbol',source,destination) <- hyperedges, symbol' /= (topsymbol (knuth!v)), v == destination  ];
      topsymbol (Just (Branch symbol _,_)) = symbol
  }



-- p :: Vertex -> [BufferedHyperpath]
-- p vertex
--   | isNothing mininmalHyperpath = []
--   | otherwise       = (fromJust mininmalHyperpath):(merge (first ++ second))
--   where {
--   first = [tail (topconcat symbol' (map p source) ) | (symbol',source,destination) <- hyperedges, symbol' == symbol, vertex == destination  ];
--   second = [topconcat symbol' (map p source) | (symbol',source,destination) <- hyperedges, symbol' /= symbol, vertex == destination  ];
--         mininmalHyperpath   = knuth!vertex;
--         (Branch symbol _,_)  = fromJust mininmalHyperpath;
--   }
-- merges a orderd lists of hyperpath to an ordered list
-- merge :: [[BufferedHyperpath]] -> [BufferedHyperpath]
-- merge list
--  | sortedList == [] = []
--  | otherwise  = (head (head sortedList)):(merge ((tail (head sortedList)):(tail sortedList)))
--     where   {
--          sortedList = (sortBy (\(x:xs) (y:ys)-> bufferedHyperpathCompare x y) list); -- (filter (\x-> let b = (not.null) x in trace (show b) b) list));
--      }


merge :: [[BufferedHyperpath]] -> [BufferedHyperpath]
merge l = merge' $ sortBy (\(x:xs) (y:ys)-> bufferedHyperpathCompare x y) $ filter (not.null) l
  where
    merge' :: [[BufferedHyperpath]] -> [BufferedHyperpath]
    merge' [] = []
    merge' (x:xs) = head x:binarymerge (tail x) (merge' xs) -- FAST!!!
    -- merge' (x:xs) = head x:merge ((tail x):xs) -- SLOW!!!

-- binary merge (merge two sorted lists into one)
binarymerge :: [BufferedHyperpath] -> [BufferedHyperpath] -> [BufferedHyperpath]
binarymerge [] l = l
binarymerge l [] = l
binarymerge l1@(x:xs) l2@(y:ys)
  | cmp == GT = y:binarymerge l1 ys
  | otherwise = x:binarymerge xs l2
  where cmp = bufferedHyperpathCompare x y

--
topconcat :: Symbol -> [[BufferedHyperpath]] -> [BufferedHyperpath]
topconcat symbol lists
  | elem [] lists = []
  | otherwise = (f' symbol (map head lists)):(merge (map (topconcat symbol) (tail (combine lists))))


combine :: [[a]] -> [[[a]]]
combine [] = [[]]
combine (x:xs) = (map (x:) c) ++ [((tail x):(map (\x-> [head x]) xs))]
  where c = combine xs

-- combine [] = [[]]
-- combine (x:xs) = (map (([head x]):) c ) ++ (map ((tail x):) c )
--   where c = combine xs

--------------------------------------------------------------------
--      Testing                                                   --
--------------------------------------------------------------------
main = do
  args <- getArgs
  let (n:_) = (map read args)::[Int]
 -- putStr (show ((p 0)!!(n-1)))
 -- putStr (show ( take n (p 0)))
  print $ take n $p 0
 -- BS.putStr (myshowlist $ fst $ unzip $ take n $p 0)


--------------------------------------------------------------------
--      EOF                                                       --
--------------------------------------------------------------------
-- merge :: [[BufferedHyperpath]] -> [BufferedHyperpath]
-- merge list
--   | filteredlist == [] = []
--    | otherwise  = m:(merge (first  ++ second))
--      where   {
--         filteredlist   = filter (/= []) list; -- remove all empty list
--         m        = minimumBy bufferedHyperpathCompare [head l | l<- filteredlist]; -- select a smallest head of the lists
--         first     = {-# SCC "first" #-}[tail l | l<- filteredlist, (head l) == m]; -- removed m from its corresponding list
--         second     = {-# SCC "second" #-}[l | l<- filteredlist, (head l) /= m];
--     }




--  knuth :: ? -> Array Vertex (Maybe BufferedHyperpath)
-- knuth u q              =  array (minimum vertices,maximum vertices) [(v, knuth' v) | v <- vertices]

-- minimalhyperedge = minimumBy bufferedHyperpathCompare [ f' symbol [] | (symbol, sources, destination) <- hyperedges, sources == [] ]

-- knuth' :: Vertex -> Maybe BufferedHyperpath
-- knuth' v = minimumBy bufferedHyperpathCompare [f' symbol (fromJust (result sources))  | (symbol, sources, destination) <- hyperedges, destination == v, isJust (result sources) ]
--     result sources = sequence (map ((!) knuth) sources)



--   [(symbol, sources, destination) <- hyperedges]
--   minimumBy bufferedHyperpathCompare [f' symbol (fromJust (result sources))  | (symbol, sources, destination) <- hyperedges, destination == v, isJust (result sources) ]
--     result sources = sequence (map ((!) knuth) sources)


--   (array (minimum vertices,maximum vertices) [(v, Nothing) | v <- vertices],[])

-- bei zugriff auf array an position v: erstmal gucken ob in v die kleinste nullstellige kante reingeht


-- instance Eq BufferedHyperpath where
--  (hyperpath,value) == (hyperpath',value') = (value == value') && (hyperpath == hyperpath')

-- instance Ord BufferedHyperpath where
--   (hyperpath,value) <= (hyperpath',value') = (value <= value')


-- ghc -O2 -fglasgow-exts -threaded -prof -auto-all -caf-all -fforce-recomp neu.hs
-- ./a.out +RTS -p && cat a.out.prof
-- time ./a.out


main = print 0 -}
