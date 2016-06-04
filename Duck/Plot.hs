module Duck.Plot where

import Duck.Types
import Graphics.EasyPlot
import Data.List

-- |Takes a label and a full report and plots it.
plotReport :: String -> FullReport -> IO ()
plotReport title fr = do
  plot X11 [Data2D [Title title, Color Red, Style Lines]
            [Range (fromIntegral (minimum sz - 2)) (fromIntegral (maximum sz + 2))]
            [(fromIntegral x, y) | (x, y) <- comb]]
  return ()
    where sz = (sizes fr)
          exp = (experiment fr)
          comb = sort $ zip sz exp

-- |Takes a list of sizes, a list of experimental times,
-- a list of analytical times and a label and plots them.
plotReportHypothesis :: [Int] -> [Double] -> [Double] -> String -> IO ()
plotReportHypothesis sz exp ana ttype = do
  plot X11 [Data2D [Title "Experimental Time", Color Red, Style Lines]
            opts
            [(fromIntegral x, y) | (x, y) <- combExp],
            Data2D [Title ("Analytic Time (" ++ ttype ++ ")"), Color Blue, Style Lines]
            opts
            [(fromIntegral x, y) | (x, y) <- combAna]]
  return ()
    where combExp = sort $ zip sz exp
          combAna = sort $ zip sz ana
          opts = [Range (fromIntegral (minimum sz - 2)) (fromIntegral (maximum sz + 2))]
