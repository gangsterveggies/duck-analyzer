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

fastSort :: [Int] -> [Int]
fastSort = sort

main = do
  let t = defaultParam {range = (100, 10000),
                        iterations = 15,
                        verbosity = Full}
  r <- runSingleTest' t (fastSort)
  
--  putStrLn $ show $ testHypothesis hpc r
  putStrLn $ show $ testHypothesis hp r
  putStrLn $ show $ testHypothesis hpln r
  putStrLn $ show $ testHypothesis hp2 r
  putStrLn $ show $ testHypothesis hp3 r
  
  plotReport "Sum" r

-- TODO
-- Change instance gen
-- Plot analytical
-- Add groups
