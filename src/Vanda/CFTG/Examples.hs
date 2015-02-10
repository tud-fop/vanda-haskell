-----------------------------------------------------------------------------
-- |
-- Module      :  Vanda.CFTG.Examples
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

module Vanda.CFTG.Examples where


import           Vanda.CFTG.CFTG
import           Vanda.Corpus.TreeTerm (parseTree)

import           Control.Arrow
import           Data.List
import           Data.Tree


drawRules :: [Rule String String String] -> String
drawRules = unlines . map drawRule


drawRule :: Rule String String String -> String
drawRule (Rule l vs r)
  = unlines
  $ (lft ++ rght) : map (replicate (length lft) ' ' ++) rghts
  where
    lft = l ++ "(" ++ intercalate ", " vs ++ ")" ++ " → "
    rght : rghts = lines $ drawTreeCompact (toTree id id id r)


languageS :: Ord v => [Rule v [Char] t] -> [Tree t]
languageS = map snd . languageS'


languageS' :: Ord v => [Rule v [Char] t] -> [(Derivation, Tree t)]
languageS' rs = map (second (toTree undefined id id)) $ language' rs (N "S" [])


drawDerivation :: Derivation -> String
drawDerivation
  = unlines
  . map (\ (d, r) -> "rule " ++ show r ++ " at " ++ if null d then "ε" else intercalate "-" (map show d))
  . map (map succ *** succ)

putLanguage :: [Tree String] -> IO ()
putLanguage = putStr . unlines . map drawTreeCompact


putLanguage' :: [(Derivation, Tree String)] -> IO ()
putLanguage' = putStr . unlines . concatMap f
  where f (d, t) = [drawDerivation d, drawTreeCompact t]


rule :: String -> Int -> String -> Rule String String String
rule l v
  = Rule l ['_' : show i | i <- [1 .. v]]
  . treeToSF
  . parseTree


g1 :: [Rule String String String]
g1 =
  [ rule "S"  0 "δ₁{(α, B{(α, α, B}(α, α, δ₂(α, α))))"
  , rule "B{" 3 "B{(α, α, B{(γ(_1), γ(_2), B}(α, α, _3)))"
  , rule "B{" 3 "δ₂(_1, δ₁{(_2, _3))"
  , rule "B}" 3 "B{(α, α, B}(γ(_1), γ(_2), B}(α, α, _3)))"
  , rule "B}" 3 "δ₂(_1, δ₁}(_2, _3))"
  ]


g1trans :: [Rule String String String]
g1trans =
  [ rule "S"  0 "B{(α, α, R}(α))"
  , rule "B{" 3 "L{(B{(γ(_1), γ(_2), R}(_3)))"
  , rule "B{" 3 "δ₁{(α, δ₂(_1, δ₁{(_2, δ₂(α, _3))))"
  , rule "B}" 3 "L{(B}(γ(_1), γ(_2), R}(_3)))"
  , rule "B}" 3 "δ₁{(α, δ₂(_1, δ₁}(_2, δ₂(α, _3))))"
  , rule "L{" 1 "L{(B{(γ(α), γ(α), _1))"
  , rule "L{" 1 "δ₁{(α, δ₂(α, _1))"
  , rule "R}" 1 "B}(γ(α), γ(α), R}(_1))"
  , rule "R}" 1 "δ₁}(α, δ₂(α, _1))"
  ]


g1trans2 :: [Rule String String String]
g1trans2 =
  {-  1 -} [ rule "S"  0 "δ₁{(α, δ₂(α, δ₁{(α, δ₂(α, B}(α, α, α, α)))))"
  {-  2 -} , rule "S"  0 "δ₁{(α, δ₂(α, δ₁{(α, δ₂(α, δ₁}(α, δ₂(α, α))))))"
  {-  3 -} , rule "S"  0 "δ₁{(α, δ₂(α, B{(α, α, α, B}(α, α, α, α))))"
  {-  4 -} , rule "S"  0 "δ₁{(α, δ₂(α, B{(α, α, α, δ₁}(α, δ₂(α, α)))))"

  {-  5 -} , rule "B{" 4 "δ₁{(α, δ₂(γ(_1), δ₁{(γ(_2), δ₂(α, B}(α, α, _3, _4)))))"
  {-  6 -} , rule "B{" 4 "δ₁{(α, δ₂(γ(_1), δ₁{(γ(_2), δ₂(α, δ₁}(α, δ₂(_3, _4))))))"
  {-  7 -} , rule "B{" 4 "δ₁{(α, δ₂(α, B{(γ(_1), γ(_2), α, B}(α, α, _3, _4))))"
  {-  8 -} , rule "B{" 4 "δ₁{(α, δ₂(α, B{(γ(_1), γ(_2), α, δ₁}(α, δ₂(_3, _4)))))"
  {-  9 -} , rule "B{" 4 "B{(α, α, γ(_1), δ₁{(γ(_2), δ₂(α, B}(α, α, _3, _4))))"
  {- 10 -} , rule "B{" 4 "B{(α, α, α, B{(γ(_1), γ(_2), α, B}(α, α, _3, _4)))"
  {- 11 -} , rule "B{" 4 "B{(α, α, α, B{(γ(_1), γ(_2), α, δ₁}(α, δ₂(_3, _4))))"
  {- 12 -} , rule "B{" 4 "B{(α, α, γ(_1), δ₁{(γ(_2), δ₂(α, δ₁}(α, δ₂(_3, _4)))))"

  {- 13 -} , rule "B}" 4 "δ₁{(α, δ₂(γ(_1), δ₁}(γ(_2), δ₂(α, B}(α, α, _3, _4)))))"
  {- 14 -} , rule "B}" 4 "δ₁{(α, δ₂(γ(_1), δ₁}(γ(_2), δ₂(α, δ₁}(α, δ₂(_3, _4))))))"
  {- 15 -} , rule "B}" 4 "δ₁{(α, δ₂(α, B}(γ(_1), γ(_2), α, B}(α, α, _3, _4))))"
  {- 16 -} , rule "B}" 4 "δ₁{(α, δ₂(α, B}(γ(_1), γ(_2), α, δ₁}(α, δ₂(_3, _4)))))"
  {- 17 -} , rule "B}" 4 "B{(α, α, γ(_1), δ₁}(γ(_2), δ₂(α, B}(α, α, _3, _4))))"
  {- 18 -} , rule "B}" 4 "B{(α, α, α, B}(γ(_1), γ(_2), α, B}(α, α, _3, _4)))"
  {- 19 -} , rule "B}" 4 "B{(α, α, α, B}(γ(_1), γ(_2), α, δ₁}(α, δ₂(_3, _4))))"
  {- 20 -} , rule "B}" 4 "B{(α, α, γ(_1), δ₁}(γ(_2), δ₂(α, δ₁}(α, δ₂(_3, _4)))))"
  ]


g2 :: [Rule String String String]
g2 =
  [ rule "S" 0 "A(α, α)"
  , rule "A" 2 "A(A(_1, α), A(_2, α))"
  , rule "A" 2 "σ(_1, _2)"
  ]
