module Duck.Statistics where

import Duck.Types

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

relevant :: Report -> Bool
relevant r = (confidence r) >= 0.98
