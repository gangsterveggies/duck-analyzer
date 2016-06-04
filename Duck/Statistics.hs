module Duck.Statistics where

import Duck.Types

import Data.Ord (comparing)
import Data.List (maximumBy, sortBy)
import qualified Data.Vector.Unboxed as UVec
import Statistics.Regression
import Test.QuickCheck (generate)

genSizeList :: (Int, Int) -> Int -> [Int]
genSizeList (lo, hi) iter = take iter [lo, lo + step .. hi]
  where step = (hi - lo) `div` iter

genInstances :: (Cased a) => [Int] -> IO [a]
genInstances [] = return []
genInstances (x:xs) = (:) <$>
                      (generate $ genCase x) <*>
                      (genInstances xs)

regress :: [Double] -> [Double] -> (Double, Double)
regress ana exp = (const UVec.! 0, conf)
  where (const, conf) = olsRegress [UVec.fromList (ana)] (UVec.fromList exp)

relevant :: [(Double, a)] -> [(Double, a)]
relevant = filter ((>= 0.98) . fst)

maxConf :: [(Double, a)] -> [(Double, a)]
maxConf ls = [maximumBy (comparing fst) ls]

topConf :: Int -> [(Double, a)] -> [(Double, a)]
topConf sz ls = take sz $ reverse $ sortBy (comparing fst) ls

outlierReport :: Double -> String
outlierReport ot
  | ot > 0.5  = "critically random"
  | ot > 0.25 = "very random"
  | ot > 0.1  = "moderately random"
  | ot > 0.05 = "slightly random"
  | otherwise = "not random"
