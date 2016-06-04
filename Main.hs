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

fastSum :: [Int] -> [Int]
fastSum = map (+1)

main = do  
  let t = defaultParam {range = (100, 500),
                        iterations = 15}

--  rsum <- runSingleTest t (fastSum)
--  rfast <- runSingleTest t (fastSort)
  rslow <- runSingleTest t (slowSort)

--  putStrLn $ show $ testHypothesis hpc r
--  putStrLn $ show $ testHypothesis hp r
--  putStrLn $ show $ testHypothesis hpln r
--  putStrLn $ show $ testHypothesis hp2 r
--  putStrLn $ show $ testHypothesis hp3 r

  let hypList = [
        hypothesis "linear" hp,
        hypothesis "linearitmic" hpln,
        hypothesis "quadratic" hp2,
        hypothesis "cubic" hp3]

--  putStrLn $ show $ map fst $ testGroup rsum hypList
--  putStrLn $ show $ map fst $ testGroup rfast hypList
  putStrLn $ show $ map fst $ testGroup rslow hypList
  putStrLn $ show $ outlierEffect rslow

--  putStrLn $ show (cconst hp rsum, cconst hp rfast)
  
--  plotHypothesis rsum hp2 (testHypothesis hp2 rsum)
  where cconst hyp fr = (propConstant $ testHypothesis hyp fr)
