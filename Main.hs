import Duck
import Data.List

hp x = n -- * (log n) / (log 2)
  where n = fromIntegral $ (length x)

main = do
  let t = Test {
        range = (500000, 1000000),
        iterations = 20,
        precision = 20,
        hypothesis = hp,
        source = sort
        }
  r <- runTest t
  putStrLn $ show r
