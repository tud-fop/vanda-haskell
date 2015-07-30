#!/usr/bin/env runhaskell

{-|
Module:      Main
Description: functions to work with /Dyck languages/ and /congruence multiple Dyck languages/
Copyright:   Ⓒ Tobias Denkinger and Toni Dietze (clipList, clipTree), 2015
Maintainer:  Tobias.Denkinger@tu-dresden.de
Stability:   experimental

This module contains functions to work with /Dyck languages/ and /congruence multiple Dyck languages/ [cf. <http://fsmnlp2015.phil.hhu.de/wp-content/uploads/2015/06/denkinger-chomsky-schuetzenberger.pdf Tobias Denkinger: A Chomsky-Schützenberger result for weighted multiple context-free languages, 2015>].
-}
module Main where

import System.Environment (getArgs)
import qualified Data.List as L
import qualified Data.Map as M

import Vanda.Dyck.DyckLanguages
import Vanda.Dyck.MultipleDyckLanguages

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-D", k1, k2, ws] -> do
      putStrLn . show $ isDyck k1 k2 ws
    ["-mD", k1, k2, ws] -> do
      putStrLn . show $ isMultipleDyck " ,;.:|" k1 k2 ws
    ["--intersectDyck", l] -> do
      putStr . unlines
             . map
                 (\(i, (cl, cr), l') ->
                      "i = " ++ show i ++ ": " ++ show (zip cl cr)
                             ++ "\n\tD ∩ Dᵢ ⊇ { ε"
                             ++ L.intercalate ", " (take 10 l')
                             ++ " }\n\t|D ∩ Dᵢ| ≥ " ++ show (length l')
                             ++ "\n")
             . zip3 [1 ..] lst
             $ map (take 100 . intersectDyckLangs
                                 (take (10 ^ (quot (read l :: Int) 2)))
                                 (splitIt $ take (read l) ['a' ..]))
                                 lst
       where lst = permute $ take (read l) ['a' ..]
    ["--intersectDyckWith", u, v] -> do
      putStrLn . (\x -> "i = " ++ show x ++ ": " ++ show (zip u v))
               . flip (M.!) (u, v)
               . M.fromList
               . flip zip [1 .. ]
               . permute
               . flip take ['a' ..]
               . length
               $ u ++ v
      putStr . unlines
             $ intersectDyckLangs
                 id
                 (splitIt $ take (length $ u ++ v) ['a' ..])
                 (u, v)
      putStr . unlines
             . map (\(i, (cl, cr), l') -> "i = " ++ show i ++ ": " ++ show (zip cl cr) ++ "\n\tD ∩ Dᵢ ⊇ { ε" ++ L.intercalate ", " (take 10 l') ++ " }\n\t|D ∩ Dᵢ| ≥ " ++ show (length l') ++ "\n")
             . filter (\ (_,x,_) -> x == (u, v))
             . zip3 [1 ..] lst
             $ map (intersectDyckLangs id . splitIt $ take (length $ u ++ v) ['a' ..]) lst where
              lst = permute $ take (length $ u ++ v) ['a' ..]
    _      -> do
      putStrLn "usage:  ./MultipleDyckLanguages ( -D <lefts> <rights> <words> |-mD <lefts> <rights> <words> |--intersectDyck <number> | --intersectDyckWith <lefts> <rights> )"

-- | Splits a given 'L.List' /M/ into two equally big 'L.List's /M₁/ and /M₂/
--   (i.e., /M₁ ∩ M₂ = ∅/, /M₁ ∪ M₂ = M/, and /|M₁| = |M₂|/) and returns all
--   bijections between /M₁/ and /M₂/.
permute :: Ord a => [a] -> [([a], [a])]
permute chars
  = concatMap (\l -> [(l, l') | l' <- L.permutations (chars L.\\ l)])
  . L.sort
  . filter ((== length chars) . (2 *) . length)
  $ L.subsequences chars

-- | For a given 'L.List' /l/ it returns a tuple of 'L.List's /(l₁, l₂)/ where
--   /l₁/ contains all the elements of /l/ that occur at an even position and
--   /l₂/ contains all the elements of /l/ that occur at an odd position where
--   indexing starts with 0 and the order of the elements is retained.
splitIt :: [a] -> ([a], [a])
splitIt [] = ([], [])
splitIt [x] = ([x], [])
splitIt (x:y:xs) = (x:xp, y:yp) where (xp, yp) = splitIt xs
