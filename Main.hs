import Duck
import Duck.Types
import Data.List

--hp n = n * (log n) / (log 2)
hp n = n
hp2 n = n^2
hp3 n = 1

slowSort [] = []
slowSort xs = mn : (slowSort $ delete mn xs)
  where mn = (minimum xs)

main = do
  let t = Test {
        range = (10, 100000),
        iterations = 15,
        source = slowSort,
        types = [0::Int]
        }
  r <- runSingleTest t
--  putStrLn $ show $ generateReport r
  runAndPlot "Sum" t


-- let res = runAndAnalyseOne 100 "o" (nf (sum) [1..1000])
-- Analysed rep <- withConfig (defaultConfig { verbosity = Quiet }) res
-- anOutlierVar $ reportAnalysis rep
-- B.estPoint $ anMean  $ reportAnalysis rep

--let t = Test {range = (10, 100000), iterations = 2, source = sort, types = [0::Int]}
