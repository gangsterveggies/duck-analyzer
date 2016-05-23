{-# LANGUAGE BangPatterns #-}

module Duck where

import Duck.Types
import Duck.Plot

import qualified Data.Vector.Unboxed as UVec
import System.Random

import qualified Test.QuickCheck as QC

import Statistics.Regression
import qualified Statistics.Resampling.Bootstrap as B

import Criterion
import Criterion.Main.Options (defaultConfig)
import Criterion.Internal (runAndAnalyseOne)
import Criterion.Monad (withConfig)
import qualified Criterion.Types as CT
--(DataRecord(..), Verbosity(..), Report(..), SampleAnalysis(..))

runAndPlot :: (Testable a) => String -> Test a b -> IO ()
runAndPlot title t = do
  fr <- runSingleTest t
  plotReport title fr

runSingleTest :: (Testable a) => Test a b -> IO FullReport
runSingleTest t = do
  instList <- genIList (range t) (iterations t)
  testInstances <- genInstances instList
  expTimes <- runSource (source t) testInstances
  return (FullReport {experiment = expTimes,
                      sizes = [fromIntegral $ dimSize vl | vl <- testInstances]
                     })

genIList :: (Int, Int) -> Int -> IO [Int]
genIList _ 0 = return []
genIList (lo, hi) iter = (:) <$>
                         QC.generate (QC.choose (lo, hi)) <*>
                         genIList (lo, hi) (iter - 1)

genInstances :: (Testable a) => [Int] -> IO [a]
genInstances [] = return []
genInstances (x:xs) = (:) <$>
                      (QC.generate $ QC.resize x QC.arbitrary) <*>
                      (genInstances xs)

runSource :: (Testable a) => (a -> b) -> [a] -> IO [Double]
runSource _ [] = return []
runSource src (x:xs) = do
  let res = runAndAnalyseOne 1 "" (whnf src x)
  CT.Analysed rep <- withConfig (defaultConfig { CT.verbosity = CT.Quiet}) res
  let !sampPoint = CT.anMean $ CT.reportAnalysis rep
      !sampVar = CT.anOutlierVar $ CT.reportAnalysis rep
  putStrLn $ show (sampPoint, dimSize x, sampVar)
  rs <- runSource src xs
  return ((B.estPoint sampPoint):rs)

generateReport :: [Double] -> FullReport -> Report
generateReport ana fr = Report {confidence = conf,
                            propConstant = (const UVec.! 0) / 100000}
  where (const, conf) = olsRegress [UVec.fromList (experiment fr)] (UVec.fromList (ana))

testHypothesis :: (Double -> Double) -> FullReport -> Report
testHypothesis hp fr = generateReport ana fr
  where ana = [hp (fromIntegral vl) | vl <- (sizes fr)]

-- Debug
runSource' _ [] = return []
runSource' src (x:xs) = do
  let res = runAndAnalyseOne 1 "" (whnf src x)
  CT.Analysed rep <- withConfig (defaultConfig { CT.verbosity = CT.Quiet}) res
  let !sampPoint = CT.anMean $ CT.reportAnalysis rep
      !sampVar = CT.anOutlierVar $ CT.reportAnalysis rep
  putStrLn $ show (sampPoint, dimSize x, sampVar)
  rs <- runSource' src xs
  return ((rep):rs)
