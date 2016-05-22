module Duck where

import qualified Data.Vector.Unboxed as UVec
import System.Random

import qualified Test.QuickCheck as QC
import Statistics.Regression

import Criterion
import Criterion.Measurement
import Criterion.Types (measTime)

type Testable = [Int]
type Range = (Int, Int)
data Report = Report {confidence :: Double,
                      propConstant :: Double}
              deriving Show
data Test a = Test {range :: Range,
                    iterations :: Int,
                    precision :: Int,
                    hypothesis :: (Testable -> Double),
                    source :: (Testable -> a)}

runTest :: Test a -> IO Report
runTest t = do
  instList <- genIList (range t) (iterations t)
  testInstances <- genInstances instList
  expTimes <- runSource (source t) (precision t) testInstances
  let anaTimes = [(hypothesis t) vl | vl <- testInstances]
      (const, conf) = olsRegress ([UVec.fromList expTimes]) (UVec.fromList anaTimes)
  return (Report {confidence = conf,
                  propConstant = (const UVec.! 1) / 1000})

genIList :: (Int, Int) -> Int -> IO [Int]
genIList _ 0 = return []
genIList (lo, hi) iter = (:) <$>
                         QC.generate (QC.choose (lo, hi)) <*>
                         genIList (lo, hi) (iter - 1)

genInstances :: [Int] -> IO [Testable]
genInstances [] = return []
genInstances (x:xs) = (:) <$>
                      (QC.generate $ QC.resize x QC.arbitrary) <*>
                      (genInstances xs)

runSource :: (Testable -> a) -> Int -> [Testable] -> IO [Double]
runSource _ _ [] = return []
runSource src prec (x:xs) = do
  (a, _) <- measure (whnf src x) (fromIntegral prec)
  let b = (measTime a) / (fromIntegral prec)
  putStrLn $ show (b, length x)
  rs <- runSource src prec xs
  return (b:rs)
