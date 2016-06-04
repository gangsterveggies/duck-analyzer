module Duck.Hypothesis where

import Duck.Types

hypothesis :: String -> (Double -> Double) -> (String, (Double -> Double))
hypothesis name hp = (name, hp)

hp, hpc, hpln, hp2, hp3, hp4 :: Double -> Double
hpc n = 1
hp n = n
hpln n = n * (log n) / (log 2)
hp2 n = n^2
hp3 n = n^3
hp4 n = n^4
