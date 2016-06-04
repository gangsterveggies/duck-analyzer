module Duck.Hypothesis where

import Duck.Types

type Hypothesis = (Double -> Double)

-- |Builds a named hypothesis.
hypothesis :: String -> Hypothesis -> (String, Hypothesis)
hypothesis name hp = (name, hp)

-- |Default hypothesis function.
hp, hpc, hpln, hp2, hp3, hp4 :: Hypothesis
hpc n = 1
hp n = n
hpln n = n * (log n) / (log 2)
hp2 n = n^2
hp3 n = n^3
hp4 n = n^4
