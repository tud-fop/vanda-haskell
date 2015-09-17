-- {-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

module Vanda.Grammar.PCFG.PCFG where





-- Treebank Extraktion

extractFromForest :: Forest String -> PCFG
extractFromForest = undefined


-- Schnitt Grammatik + String

intersect :: PCFG -> String -> PCFG
intersect = undefined


-- EM Algorithmus

train :: PCFG -> [String] -> String
train = undefined


-- n best derivations

bests :: PCFG -> Int -> Forest String
bests = undefined