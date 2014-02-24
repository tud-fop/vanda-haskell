module Data.WTA where

data WTA w v
  = WTA { delta :: [v] -> [w] -> [(v, Double)],
          nu    ::  v         -> Double
        }

class Functor s => State s where
  mapState :: (v -> v') -> s v -> s v'
