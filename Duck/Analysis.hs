module Duck.Analysis where

import Duck.Types
import Duck.Plot
import Duck.Utils

import qualified Data.Vector.Unboxed as UVec
import System.Random

import Test.QuickCheck (Arbitrary(..), generate,
                        choose, resize, arbitrary)

import Statistics.Regression
import qualified Statistics.Resampling.Bootstrap as B

import Criterion
import Criterion.Main.Options (defaultConfig)
import Criterion.Internal (runAndAnalyseOne)
import Criterion.Monad (withConfig)
import Control.DeepSeq (NFData)
import qualified Criterion.Types as CT


runSingleTest' :: (Arbitrary a, Sized a, NFData b) => Parameters -> (a -> b) -> IO FullReport
runSingleTest' t src = runSingleTest dimSize t src

runSingleTest :: (Arbitrary a, NFData b) => (a -> Int) -> Parameters -> (a -> b) -> IO FullReport
runSingleTest dim pr src = do
  instList <- genIList (range pr) (iterations pr)
  testInstances <- genInstances instList
  let sizes = [fromIntegral $ dim vl | vl <- testInstances]
  expTimes <- runSource pr (src) testInstances sizes
  return (FullReport {experiment = expTimes,
                      sizes = sizes
                     })

testHypothesis :: (Double -> Double) -> FullReport -> Report
testHypothesis hp fr = generateReport ana fr
  where ana = [hp (fromIntegral vl) | vl <- (sizes fr)]

genIList :: (Int, Int) -> Int -> IO [Int]
genIList _ 0 = return []
genIList (lo, hi) iter = (:) <$>
                         generate (choose (lo, hi)) <*>
                         genIList (lo, hi) (iter - 1)

genInstances :: (Arbitrary a) => [Int] -> IO [a]
genInstances [] = return []
genInstances (x:xs) = (:) <$>
                      (generate $ resize x arbitrary) <*>
                      (genInstances xs)

runSource :: (NFData b) => Parameters -> (a -> b) -> [a] -> [Int] -> IO [Double]
runSource _ _ [] [] = return []
runSource pr src (inst:is) (sz:ss) = do
  let res = runAndAnalyseOne 1 "" (nf src inst)
  CT.Analysed rep <- withConfig (defaultConfig { CT.verbosity = CT.Quiet,
                                                 CT.timeLimit = (timePerTest pr) }) res
  let sampPoint = CT.anMean $ CT.reportAnalysis rep
      sampVar = CT.anOutlierVar $ CT.reportAnalysis rep
  putStrIf ((verbosity pr) == Full) $ show (sampPoint, sz, sampVar)
  rs <- runSource pr src is ss
  return ((B.estPoint sampPoint):rs)
runSource _ _ _ _ = error "Invalid instance/size"

generateReport :: [Double] -> FullReport -> Report
generateReport ana fr = Report {confidence = conf,
                                propConstant = (const UVec.! 0) / 100000}
  where (const, conf) = olsRegress [UVec.fromList (experiment fr)] (UVec.fromList (ana))
