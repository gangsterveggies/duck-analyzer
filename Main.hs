import Duck
import Duck.Types
import Data.List

--hp n = n * (log n) / (log 2)
hp n = n
hp2 n = n^2
hp3 n = 1

slowSort :: [Int] -> [Int]
slowSort [] = []
slowSort xs = mn : (slowSort $ delete mn xs)
  where mn = (minimum xs)

main = do
  let t = Test {
        range = (10, 300),
        iterations = 15,
        source = slowSort
        }
  r <- runSingleTest t
  putStrLn $ show $ testHypothesis hp2 r
  plotReport "Sum" r


-- let res = runAndAnalyseOne 100 "o" (nf (sum) [1..1000])
-- Analysed rep <- withConfig (defaultConfig { verbosity = Quiet }) res
-- anOutlierVar $ reportAnalysis rep
-- B.estPoint $ anMean  $ reportAnalysis rep

--let t = Test {range = (10, 100000), iterations = 2, source = sort, types = [0::Int]}
