{-# LANGUAGE BangPatterns #-}
module Main where

import Codec.Compression.GZip ( compress, decompress )

-- import Control.Applicative
import Control.DeepSeq ( ($!!) , deepseq , force)
import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import Data.Int ( Int8, Int16, Int32 )
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Environment ( getArgs )


import Control.Arrow ( (***), (&&&) )
import Control.DeepSeq ( NFData (..) )
import qualified Data.Heap as H
import Data.Either
import qualified Data.IntMap as IM
import qualified Data.Ix as Ix
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Tree as T



import Vanda.Features
import Vanda.Hypergraph hiding ( knuth )
import Vanda.Hypergraph.Binary
import Vanda.Hypergraph.NFData
-- import Vanda.Hypergraph.EdgeList as EL (toBackwardStar)

instance (Show v, Show l, Show i) => Show (Hyperedge v l i) where
  show e
    = show (to e)
      ++ " -> "
      ++ show (label e)
      ++ " "
      ++ unwords (map show $ from e)
      ++ " # "
      ++ show (i e)

-- "~lindal/Downloads/SHK/Berkeley/t_eng_sm6.txt.grammar.hg" 
-- "~lindal/Downloads/SHK/Berkeley/t_small.hg" 
-- "/home/student/lindal/Downloads/SHK/Berkeley/berkeley.bhg"


type CandidateHeap = H.MinPrioHeap Double (Derivation Int32 Int32 Int32, Double)
type BestMapping = Int32 -> Candidate Int32 Int32 Int32 Double

knuth
  :: EdgeList Int32 Int32 Int32
  -> Feature Int32 Int32 Double
  -> V.Vector Double
  -> BestArray Int32 Int32 Int32 Double
knuth (EdgeList vs es) feat wV
  = knuthLoop
      (H.fromList $!! map (flip (topCC feat wV) []) $ nullE)
      (A.array vs [ (v, []) | v <- Ix.range vs ])
      adjIM
  where
    -- auxiliary data structures:
    nullE :: [Hyperedge Int32 Int32 Int32] -- ^ nullary edges
    forwA :: A.Array Int32 [Hyperedge Int32 Int32 Int32] -- ^ forward star w/edge ids
    adjIM :: IM.IntMap Int -- ^ # ingoing adjacencies by edge id
    (nullE, (forwA, adjIM))
      = lefts
        &&& ( (A.accumArray (flip (:)) [] vs *** IM.fromList)
            . (concat *** concat)
            . unzip . rights
            )
      $ [ if null frome
          then Left e
          else Right
            $!! (,)
              [ (v, e) | v <- S.toList ingoing ]
              [ (fromIntegral $ i e, S.size ingoing) ]
        | e <- es
        , let frome = from e
        , let ingoing = S.fromList $ frome
        ]
    -- 
    knuthLoop
      :: CandidateHeap
      -> BestArray Int32 Int32 Int32 Double
      -> IM.IntMap Int
      -> BestArray Int32 Int32 Int32 Double
    knuthLoop candH bestA adjIM = case H.view candH of
      Nothing -> bestA -- < no candidates, so we are done
      Just (it@(w, (d@(T.Node e ds), x)), candH') ->
        case bestA A.! v of
          -- candidate for an as yet unvisited node
          [] -> knuthLoop
            (H.union candH' $ H.fromList newCand)
            bestA'
            (IM.fromList adjChange `IM.union` adjIM)
              -- union: left argument preferred
          -- candidate for a visited node, just throw it away
          _ -> knuthLoop candH' bestA adjIM
        where
          bestA' = bestA A.// [(v, [it])]
          v = to e
          newCand :: [Candidate Int32 Int32 Int32 Double] -- < new candidates from v
          adjChange :: [(Int, Int)] -- < changes to adjacency map
          (newCand, adjChange)
            = (catMaybes *** id) . unzip . map work . (forwA A.!) $ v
          -- compute change for a given edge information
          work
            :: (Hyperedge Int32 Int32 Int32)
            -> (Maybe (Candidate Int32 Int32 Int32 Double), (Int, Int))
          work e
            = let k = fromIntegral $ i e
                  unvis' = (adjIM IM.! k) - 1
                  cand =
                    if (==0) unvis'
                    then Just $ topCC feat wV e $ map (head . (bestA' A.!))
                         $ from e
                    else Nothing
              in (cand, (k, unvis'))



main :: IO ()
main = mainKnuth

mainKnuth :: IO ()
mainKnuth = do 
  args <- getArgs
  case args of
    ["-z", zhgfile] -> do
      weights :: VU.Vector Double
        <- fmap
             (VU.fromList . B.decode . decompress)
           $ B.readFile
           $ zhgfile ++ ".weights.gz"
      el :: EdgeList Int32 Int32 Int32
        <- fmap
             (force . B.decode . decompress)
           $ B.readFile zhgfile
      let pN (l,i) xs = (weights VU.! fromIntegral i) + (sum xs)
      --el `seq` print "ok"
      weights `seq` el `seq` print
        $ (A.! 1132)
        $ knuth el (Feature pN V.singleton) (V.singleton 1)

-- heap: ~350MB
-- time: ~102s
      
{-mainNbest :: IO()
mainNbest = do                                       
  args <- getArgs
  case args of
    ["-z", zhgfile] -> do
      wghts <- B.decodeFile $ zhgfile ++ ".weights" :: IO([Double])
      hg' <- B.readFile zhgfile
      let !hg = B.decode . decompress $! hg'  :: [Hyperedge Int32 Int32 Int32]
          !weights = array (0,fromIntegral $ (length wghts) - 1) $ zip [0..] wghts 
          !el = ($!!) EdgeList (bounds weights) hg
          processNode = (\(l,i) xs -> (weights ! i) + (sum xs))
          feat = ($!!) Feature processNode V.singleton
          vector = V.singleton 1
      deepseq (wghts)
              print $ bests 
              (EL.toBackwardStar el)
              feat
              (V.singleton 1)
  -}            
-- heap: ~700MB
-- time: ~104s


