module Main where

import Duck
--import Duck.Types
import Data.List

--hpc n = 1
hp n = n
hpln n = n * (log n) / (log 2)
hp2 n = n^2
hp3 n = n^3

slowSort :: [Int] -> [Int]
slowSort [] = []
slowSort xs = mn : (slowSort $ delete mn xs)
  where mn = (minimum xs)

fastSort :: [String] -> [String]
fastSort = (map reverse) . sort

main = do  
  let t = defaultParam {range = (100, 3000),
                        iterations = 10,
                        verbosity = Full,
                        timePerTest = 1.0}
  r <- runSingleTest t (fastSort)

--  putStrLn $ show $ testHypothesis hpc r
  putStrLn $ show $ testHypothesis hp r
  putStrLn $ show $ testHypothesis hpln r
  putStrLn $ show $ testHypothesis hp2 r
  putStrLn $ show $ testHypothesis hp3 r

  putStrLn $ show $ testGroup r [
    hypothesis "linear" hp,
    hypothesis "linearitmic" hpln,
    hypothesis "quadratic" hp2]
  
  plotHypothesis r hp (testHypothesis hp r)
