module Duck.Analysis where

import Duck.Types
import Duck.Plot
import Duck.Utils
import Duck.Statistics
import Duck.Hypothesis

import qualified Statistics.Resampling.Bootstrap as B

import Criterion
import Criterion.Main.Options (defaultConfig)
import Criterion.Internal (runAndAnalyseOne)
import Criterion.Monad (withConfig)
import Control.DeepSeq (NFData)
import qualified Criterion.Types as CT

-- |The worker function. Takes a set of parameters and
-- a function to evaluate and produces a full report of
-- results. It is required that the function to be evaluated
-- is of type (a -> b), where 'a' is sizable and willing to
-- have test cases generated, and 'b' needs to have a normal
-- form.
runSingleTest :: (Cased a, Sized a, NFData b) => Parameters -> (a -> b) -> IO FullReport
runSingleTest pr src = do
  putStrIf ((verbosity pr) >= Moderate) "Starting benchmarks..."
  testInstances <- genInstances $ genSizeList (range pr) (iterations pr)
  let sizes = [fromIntegral $ dimSize vl | vl <- testInstances]
  (expTimes, expStds, otlr) <- runSource pr (src) testInstances sizes
  putStrIf ((verbosity pr) >= Moderate) "Crunching results..."
  return (FullReport {experiment = expTimes,
                      expStd = expStds,
                      sizes = sizes,
                      outlierEffect = otlr / (fromIntegral $ length sizes)
                     })

-- |Takes an hypothesis complexity function and a full
-- report and fits the experimental data to the analytical
-- data to obtain a confidence value and a proportionality
-- constant (in seconds).
testHypothesis :: Hypothesis -> FullReport -> Report
testHypothesis hp fr = generateReport ana fr
  where ana = [hp (fromIntegral vl) | vl <- (sizes fr)]

-- |Performs a grouped hypothesis test, takes a full report
-- and a set of named hypothesis, as well as a grouping function
-- (some examples defined in Duck.Statistics) and produces a list
-- of reports.
testGroup :: FullReport -> [(String, Hypothesis)] ->
             ([(Double, (String, Report))] -> [(Double, (String, Report))])
             -> GroupReport
testGroup fr xs flt = map snd fltrd
  where ls = map (\(name, f) -> (name, testHypothesis f fr)) xs
        ind = zip (map (confidence . snd) ls) ls
        fltrd = flt ind

-- |Takes experimental data and an hypothesis and plots both.
plotHypothesis :: FullReport -> Hypothesis -> Report -> String -> IO ()
plotHypothesis fr hp r ttype = plotReportHypothesis sz exp ana ttype
  where sz = (sizes fr)
        exp = (experiment fr)
        ana = [(propConstant r) * hp (fromIntegral vl) | vl <- (sizes fr)]

-- |Runs a source function in a set of inputs with a given size
-- and produces a tuple of: ([Mean], [Standard deviation], Outlier effect),
-- wrapped in the IO monad.
runSource :: (NFData b) => Parameters -> (a -> b) -> [a] -> [Int] -> IO ([Double], [Double], Double)
runSource _ _ [] [] = return ([], [], 0)
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
  putStrIf ((verbosity pr) == Moderate && (1 + length ss) `mod` 5 == 0) $
    (show $ 1 + length ss) ++ " iterations left (about " ++
    (show $ (round $ timePerTest pr) * 4 * (1 + length ss) `div` 3) ++ " seconds left)"
  
  (avgs, stds, otlr) <- runSource pr src is ss
  return ((B.estPoint sampPoint):avgs,
          (B.estPoint sampVar):stds,
          otlr + CT.ovFraction sampOutlier)

-- |Generates a report from a full report and a list
-- of analytical results.
generateReport :: [Double] -> FullReport -> Report
generateReport ana fr = Report {confidence = conf,
                                propConstant = const}
  where (const, conf) = regress ana (experiment fr)
