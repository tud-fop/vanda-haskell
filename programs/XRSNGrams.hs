module Main where

import qualified Data.Array as A
import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.WTA as WTA
import qualified Vanda.Algorithms.Earley.WSA as WSA
import qualified Vanda.Algorithms.IntersectWithNGram as IS
import qualified Vanda.Algorithms.IntersectWithNGramUtil as ISU
import qualified Vanda.Grammar.LM as LM
import qualified Vanda.Grammar.NGrams.Functions as LM
import qualified Vanda.Grammar.NGrams.VandaNGrams as VN
import qualified Vanda.Grammar.XRS.Functions as IF
import qualified Vanda.Grammar.XRS.IRTG as I
import qualified Vanda.Hypergraph.IntHypergraph as HI
import qualified Vanda.Token as TK

import Codec.Compression.GZip ( compress, decompress )
import Vanda.Algorithms.IntEarley
import System.Environment ( getArgs )

import System.Console.GetOpt

import Debug.Trace

data Modus = Product String
           | Translate String

data Flag' = Help
           | Beam String
           | Zhg String
           | EFile String
           | FFile String
           | LM String deriving Eq

type Flag = Either Flag' Modus

type Configuration = ( Maybe Modus, -- modus
                       Maybe Int,   -- beam
                       Maybe String -- path
                     )

type DataSet = ( Maybe I.XRS,                        -- grammar
                 Maybe (TK.TokenMap, TK.TokenArray), -- node names
                 Maybe (TK.TokenMap, TK.TokenArray), -- english token names
                 Maybe (TK.TokenMap, TK.TokenArray), -- french token names
                 Maybe (VN.NGrams T.Text)            -- language model
               )

options :: [OptDescr Flag] 
options
  = [ Option ['h'] ["help"]      (NoArg  (Left Help))                   "shows help",
      Option ['p'] ["product"]   (ReqArg (Right . Product) "<style>")   "product with <style>",
      Option ['t'] ["translate"] (ReqArg (Right . Translate) "<style>") "translate with <style>",
      Option ['b'] ["beam"]      (ReqArg (Left  . Beam) "<beam>")       "beam width",
      Option ['z'] ["zhg"]       (ReqArg (Left  . Zhg) "<zhg>")         "zhg file prefix to be used",
      Option ['e'] ["eMapFile"]  (ReqArg (Left  . EFile) "<map.e>")     "english map file to be used",
      Option ['f'] ["fMapFile"]  (ReqArg (Left  . FFile) "<map.f>")     "french file to be used",
      Option ['l'] ["lm"]        (ReqArg (Left  . LM) "<lm>")           "language model file to be used"
    ]

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (flags, _, []) -> do ds <- getDataSet flags
                         let cfg = getConfiguration flags
                         doWork cfg ds
    _              -> error $ usageInfo "" options

doWork :: Configuration -> DataSet -> IO ()
doWork (Just (Product x), b, Just zhgFile) (Just xrs, Just (nm, na), _, Just (fm, fa), Just lm)
  = case (x, b) of ("BHPS", _)            -> go IS.intersectBHPS
                   ("NoBackoff", _)       -> go IS.intersectUnsmoothed
                   ("Backoff", _)         -> go IS.intersectSmoothed
                   ("Pruning", Just beam) -> go (IS.intersectPruning beam)
    where
      go function = do
       let (xrs1, states)
                   = ISU.intersect function (LM.indexOf lm . T.fromStrict . TK.getString fa) lm
                   $ xrs
       let xrs'  = ISU.relabel (TK.getToken fm . T.toStrict . LM.getText lm) xrs1
       let states'
                 = V.map (ISU.mapState id (TK.getToken fm . T.toStrict . LM.getText lm)) states
       B.writeFile (zhgFile ++ ".new.bhg.gz") . compress
                                              . B.encode
                                              $ I.irtg xrs'
       B.writeFile (zhgFile ++ ".new.weights.gz") . compress
                                                  . B.encode
                                                  . VU.toList
                                                  $ I.weights xrs'
       TIO.writeFile (zhgFile ++ ".new.nodes") . TK.toText
                                               . TK.TokenArray
                                               . (\x -> A.listArray (0, length x - 1) x)
                                               . map (TS.pack . show)
                                               . V.toList
                                               . flip V.map states'
                                               $ ISU.mapState (TK.getString na) (TK.getString fa)
doWork (Just (Translate x), b, _) (Just xrs, Just (nm, na), Just (em, ea), Just (fm, fa), Just lm)
  = do case (x, b) of ("BHPS", _)            -> go IS.intersectBHPS
                      ("NoBackoff", _)       -> go IS.intersectUnsmoothed
                      ("Backoff", _)         -> go IS.intersectSmoothed
                      ("Pruning", Just beam) -> go (IS.intersectPruning beam)
    where
      go function = do
       inp <- TIO.getContents
       let translate input = output
             where
             wsa = IF.toWSAmap fm input
             comp = V.toList . (I.h2 (I.irtg xrs) V.!) . I._snd
             (mm, ip, _) = earley (toBackwardStar (I.rtg $ I.irtg xrs) comp) comp wsa fst . I.initial $ I.irtg xrs
             initial' = mm M.! (0, I.initial $ I.irtg xrs, fst . head . WSA.finalWeights $ wsa)
             xrs1 = I.XRS (I.IRTG ip initial' (I.h1 $ I.irtg xrs) (I.h2 $ I.irtg xrs)) (VU.generate (VU.length $ I.weights xrs) (I.weights xrs VU.!))
             (xrs', sts)
               = ISU.intersect function (LM.indexOf lm . T.fromStrict . TK.getString ea) lm
               $ xrs1
--              xrs' = xrs1
             feat _ i xs = (if i < 0 then 1 else I.weights xrs' VU.! i) * product xs
             ba = flip HI.knuth feat . I.rtg $ I.irtg xrs'
             best = ba A.! 0 -- target symbol is always 0
             otree = map (IF.getTree' ((I.h1 (I.irtg xrs') V.!) . I._fst) . HI.deriv) best
             output = IF.toString ea otree
       TIO.putStr . T.unlines . map translate $ T.lines inp
doWork _ _ = error $ usageInfo "" options

getConfiguration :: [Flag] -> Configuration
getConfiguration = go (Nothing, Nothing, Nothing) where
  go x []            = x
  go (a, b, c) (x : xs) = case x of
    Right (Product y)   -> go (Just (Product y), b, c) xs
    Right (Translate y) -> go (Just (Translate y), b, c) xs
    Left (Beam y)       -> go (a, Just (read y), c) xs
    Left (Zhg y)        -> go (a, b, Just y) xs
    _                   -> go (a, b, c) xs

getDataSet :: [Flag] -> IO DataSet
getDataSet = go (Nothing, Nothing, Nothing, Nothing, Nothing) where
  go x [] = return x
  go (a, b, c, d, e) (x : xs) = case x of
    Left (Zhg file)   -> do irtg1 <- IF.loadIRTG (file ++ ".bhg.gz")
                            ws    <- IF.loadWeights (file ++ ".weights.gz")
                            nm    <- IF.loadTokenMap (file ++ ".nodes")
                            na    <- IF.loadTokenArray (file ++ ".nodes")
                            let xrs = I.XRS irtg1 (VU.generate (V.length ws) (ws V.!))
                            go (Just xrs, Just (nm, na), c, d, e) xs
    Left (EFile file) -> do em    <- IF.loadTokenMap file
                            ea    <- IF.loadTokenArray file
                            go (a, b, Just (em, ea), d, e) xs
    Left (FFile file) -> do fm    <- IF.loadTokenMap file
                            fa    <- IF.loadTokenArray file
                            go (a, b, c, Just (fm, fa), e) xs
    Left (LM file)    -> do lm    <- LM.loadNGrams file
                            go (a, b, c, d, Just lm) xs
    _                 -> go (a, b, c, d, e) xs

-- productPostProcess
--   :: (LM a)
--   => TokenArray -> TokenMap -> TokenArray -> TokenMap -> a -> XRS -> 
-- productPostProcess na nm fa fm lm xrs1 states
--   = do let xrs'  = ISU.relabel (TK.getToken fm . T.toStrict . LM.getText lm) xrs1
--        let states'
--                  = V.map (ISU.mapState id (TK.getToken fm . T.toStrict . LM.getText lm)) states
--        B.writeFile (zhgFile ++ ".new.bhg.gz") . compress
--                                               . B.encode
--                                               . I.irtg
--                                               $ xrs'
--        B.writeFile (zhgFile ++ ".new.weights.gz") . compress
--                                                   . B.encode
--                                                   . VU.toList
--                                                   . I.weights
--                                                  $ xrs'
--        TIO.writeFile (zhgFile ++ ".new.nodes") . TK.toText
--                                                . TK.TokenArray
--                                                . (\x -> A.listArray (0, length x - 1) x)
--                                                . map (TS.pack . show)
--                                                . V.toList
--                                                . V.map (ISU.mapState (TK.getString na) (TK.getString fa))
--                                                $ states'

{-
main2
  :: IO ()
main2 = do
  args <- getArgs
  case args of
    ["--translateComplete", wtaStyle, "-e", eMapFile, "-f", fMapFile, "-z", zhgFile, "-l", lmFile] -> do
      I.IRTG{ .. }
          <- IF.loadIRTG (zhgFile ++ ".bhg.gz")
      ws  <- IF.loadWeights (zhgFile ++ ".weights.gz")
      na  <- IF.loadTokenArray (zhgFile ++ ".nodes")
      em  <- IF.loadTokenArray eMapFile
      fm  <- IF.loadTokenMap fMapFile
      fa  <- IF.loadTokenArray fMapFile
      lm  <- LM.loadNGrams lmFile
      inp <- TIO.getContents
      let translate (i, input) = do
            B.writeFile (zhgFile ++ ".new" ++ show i ++ ".bhg.gz")
              . compress . B.encode . I.irtg $ xrs'
            B.writeFile (zhgFile ++ ".new" ++ show i ++ ".weights.gz")
              . compress . B.encode . VU.toList . I.weights $ xrs'
            TIO.writeFile (zhgFile ++ ".new" ++ show i ++ ".nodes")
              . TK.toText
              . TK.TokenArray
              . (\x -> A.listArray (0, length x - 1) x)
              . map (TS.pack . show)
              . V.toList
--               . V.map (ISU.mapState (TK.getString na) (TK.getString fa))
              $ states'
            where
              wsa  = IF.toWSAmap fm input
              comp = V.toList . (h2 V.!) . I._snd
              (mm, ip, _)
                   = earley (toBackwardStar rtg comp) comp wsa fst initial
              initial'
                   = mm M.! (0, initial, fst . head . WSA.finalWeights $ wsa)
              xrs  = I.XRS (I.IRTG ip initial' h1 h2) (VU.generate (V.length ws) (ws V.!))
              (xrs1, sts)
                   = case wtaStyle of
                       "--unsmoothed" -> IS.intersectUnsmoothed `ISU.intersect` lm
                       "--smoothed"   -> IS.intersectSmoothed `ISU.intersect` lm
                   . ISU.relabel (LM.indexOf lm . T.fromStrict . TK.getString fa)
                   $ xrs
              xrs' = ISU.relabel (TK.getToken fm . T.toStrict . LM.getText lm) xrs1
              states'
                   = V.map (ISU.mapState id (TK.getToken fm . T.toStrict . LM.getText lm)) sts
      mapM_ translate . zip [1 ..] $ T.lines inp
    ["--translateComplete", "--pruning", c, "-e", eMapFile, "-f", fMapFile, "-z", zhgFile, "-l", lmFile] -> do
      I.IRTG{ .. }
          <- IF.loadIRTG (zhgFile ++ ".bhg.gz")
      ws  <- IF.loadWeights (zhgFile ++ ".weights.gz")
      na  <- IF.loadTokenArray (zhgFile ++ ".nodes")
      em  <- IF.loadTokenArray eMapFile
      fm  <- IF.loadTokenMap fMapFile
      fa  <- IF.loadTokenArray fMapFile
      lm  <- LM.loadNGrams lmFile
      inp <- TIO.getContents
      let translate (i, input) = do
            B.writeFile (zhgFile ++ ".new" ++ show i ++ ".bhg.gz")
              . compress . B.encode . I.irtg $ xrs'
            B.writeFile (zhgFile ++ ".new" ++ show i ++ ".weights.gz")
              . compress . B.encode . VU.toList . I.weights $ xrs'
            TIO.writeFile (zhgFile ++ ".new" ++ show i ++ ".nodes")
              . TK.toText
              . TK.TokenArray
              . (\x -> A.listArray (0, length x - 1) x)
              . map (TS.pack . show)
              . V.toList
--               . V.map (ISU.mapState (TK.getString na) (TK.getString fa))
              $ states'
            where
              wsa  = IF.toWSAmap fm input
              comp = V.toList . (h2 V.!) . I._snd
              (mm, ip, _)
                   = earley (toBackwardStar rtg comp) comp wsa fst initial
              initial'
                   = mm M.! (0, initial, fst . head . WSA.finalWeights $ wsa)
              xrs  = I.XRS (I.IRTG ip initial' h1 h2) (VU.generate (V.length ws) (ws V.!))
              (xrs1, sts)
                   = ISU.intersect (IS.intersectPruning (read c)) lm
                   . ISU.relabel (LM.indexOf lm . T.fromStrict . TK.getString fa)
                   $ xrs
              xrs' = ISU.relabel (TK.getToken fm . T.toStrict . LM.getText lm) xrs1
              states'
                   = V.map (ISU.mapState id (TK.getToken fm . T.toStrict . LM.getText lm)) sts
      mapM_ translate . zip [1 ..] $ T.lines inp
    ["--best", "-e", eMapFile, "-z", zhgFile, "-c", c] -> do
      irtg <- IF.loadIRTG (zhgFile ++ ".bhg.gz")
      ws   <- IF.loadWeights (zhgFile ++ ".weights.gz")
      em   <- IF.loadTokenArray eMapFile
      let xrs = I.XRS irtg (VU.generate (V.length ws) (ws V.!))
          feat _ i xs = (if i < 0 then 1 else I.weights xrs VU.! i) * product xs
          ba = flip HI.knuth feat . I.rtg $ I.irtg xrs
          best = ba A.! 0 -- target symbol is always 0
          otree = map (IF.getTree' ((I.h1 irtg V.!) . I._fst) . HI.deriv) best
          output = IF.toString em otree
      TIO.putStrLn output
    _ -> putStr "usage: XRSNGrams [--translate] [WTA_STYLE] [-e EMAP] -f FMAP -z ZHG -l LM\n"
-}
