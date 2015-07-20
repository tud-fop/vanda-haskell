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

import           Data.Tree


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

ts1 :: Int -> [Tree String]
ts1 n = take n $ map (toTree undefined id id) $ language g1 (N "S" [])


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

ts1trans :: Int -> [Tree String]
ts1trans n = take n $ map (toTree undefined id id) $ language g1trans (N "S" [])
