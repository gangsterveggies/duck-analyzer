module Duck.Statistics where

import Duck.Types

import Data.Ord (comparing)
import Data.List (maximumBy, sortBy)
import qualified Data.Vector.Unboxed as UVec
import Statistics.Regression
import Test.QuickCheck (generate)

-- |Generates a list of instance sizes to be benchmarked.
genSizeList :: (Int, Int) -> Int -> [Int]
genSizeList (lo, hi) iter = take iter [lo, lo + step .. hi]
  where step = (hi - lo) `div` iter

-- |Generates multiple instance of given sizes to be benchmarked.
genInstances :: (Cased a) => [Int] -> IO [a]
genInstances [] = return []
genInstances (x:xs) = (:) <$>
                      (generate $ genCase x) <*>
                      (genInstances xs)

-- |Packs and unpacks lists to perform a linear regression
-- on data and return a error value and a proportionality constant. 
regress :: [Double] -> [Double] -> (Double, Double)
regress ana exp = (const UVec.! 0, conf)
  where (const, conf) = olsRegress [UVec.fromList (ana)] (UVec.fromList exp)

-- |A grouping function to be used with 'testGroup'. It
-- filters out the statistically unrelevant results.
relevant :: [(Double, a)] -> [(Double, a)]
relevant = filter ((>= 0.98) . fst)

-- |A grouping function to be used with 'testGroup'. It
-- keeps only the maximum confidence result.
maxConf :: [(Double, a)] -> [(Double, a)]
maxConf ls = [maximumBy (comparing fst) ls]

-- |A grouping function to be used with 'testGroup'. It
-- keeps only the top 'sz' maximum confidence results.
topConf :: Int -> [(Double, a)] -> [(Double, a)]
topConf sz ls = take sz $ reverse $ sortBy (comparing fst) ls
