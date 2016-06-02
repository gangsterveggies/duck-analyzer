module Duck.Analysis where

import Duck.Types
import Duck.Plot
import Duck.Utils

import Debug.Trace

import qualified Data.Vector.Unboxed as UVec
import System.Random

import Test.QuickCheck (Arbitrary(..), generate,
                        choose, resize, arbitrary)

import Statistics.Distribution
import Statistics.Distribution.ChiSquared
import Statistics.Regression
import qualified Statistics.Resampling.Bootstrap as B

import Criterion
import Criterion.Main.Options (defaultConfig)
import Criterion.Internal (runAndAnalyseOne)
import Criterion.Monad (withConfig)
import Control.DeepSeq (NFData)
import qualified Criterion.Types as CT

runSingleTest :: (Cased a, Sized a, NFData b) => Parameters -> (a -> b) -> IO FullReport
runSingleTest pr src = do
  testInstances <- genInstances $ genIList (range pr) (iterations pr)
  let sizes = [fromIntegral $ dimSize vl | vl <- testInstances]
  (expTimes, expStds) <- runSource pr (src) testInstances sizes
  return (FullReport {experiment = expTimes,
                      expStd = expStds,
                      sizes = sizes
                     })

plotHypothesis :: FullReport -> (Double -> Double) -> Report -> IO ()
plotHypothesis fr hp r = trace (show (exp, ana)) $ plotReportHypothesis sz exp ana
  where sz = (sizes fr)
        exp = (experiment fr)
        ana = [(propConstant r) * hp (fromIntegral vl) | vl <- (sizes fr)]

testGroup :: FullReport -> [(String, Double -> Double)] -> GroupReport
testGroup fr xs = GroupReport {relevantHypothesis = ls}
  where ls = (filter (relevant . snd)) $ map (\(name, f) -> (name, testHypothesis f fr)) xs

testHypothesis :: (Double -> Double) -> FullReport -> Report
testHypothesis hp fr = generateReport ana fr
  where ana = [hp (fromIntegral vl) | vl <- (sizes fr)]

genIList :: (Int, Int) -> Int -> [Int]
genIList (lo, hi) iter = take iter [lo, lo + step .. hi]
  where step = (hi - lo) `div` iter

genInstances :: (Cased a) => [Int] -> IO [a]
genInstances [] = return []
genInstances (x:xs) = (:) <$>
                      (generate $ genCase x) <*>
                      (genInstances xs)

-- Returns ([Mean], [Standard Deviation])
runSource :: (NFData b) => Parameters -> (a -> b) -> [a] -> [Int] -> IO ([Double], [Double])
runSource _ _ [] [] = return ([], [])
runSource pr src (inst:is) (sz:ss) = do
  let res = runAndAnalyseOne 1 "" (nf src inst)
  CT.Analysed rep <- withConfig (defaultConfig { CT.verbosity = CT.Quiet,
                                                 CT.timeLimit = (timePerTest pr) }) res
  let sampPoint = CT.anMean $ CT.reportAnalysis rep
      sampVar = CT.anStdDev $ CT.reportAnalysis rep
      sampOutlier = CT.anOutlierVar $ CT.reportAnalysis rep
      
  putStrIf ((verbosity pr) == Full) $ show sampPoint
  putStrIf ((verbosity pr) == Full) $ show sz
  putStrIf ((verbosity pr) == Full) $ show sampOutlier
  putStrIf ((verbosity pr) == Full) $ show sampVar
  putStrIf ((verbosity pr) == Full) ""
  
  (avgs, stds) <- runSource pr src is ss
  return ((B.estPoint sampPoint):avgs, (B.estPoint sampVar):stds)
runSource _ _ _ _ = error "Invalid instance/size"

generateReport :: [Double] -> FullReport -> Report
generateReport ana fr = trace (show (const, conf)) $
                        Report {confidence = conf,
                                propConstant = const UVec.! 0}
  where (const, conf) = olsRegress [UVec.fromList (ana)] (UVec.fromList (experiment fr))

relevant :: Report -> Bool
relevant r = (confidence r) >= 0.98
