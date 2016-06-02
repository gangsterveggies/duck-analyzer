module Duck.Plot where

import Duck.Types
import Graphics.EasyPlot
import Data.List

plotReport :: String -> FullReport -> IO ()
plotReport title fr = do
  plot X11 [Data2D [Title title, Color Red, Style Lines]
            [Range (fromIntegral (minimum sz - 2)) (fromIntegral (maximum sz + 2))]
            [(fromIntegral x, y) | (x, y) <- comb]]
  return ()
    where sz = (sizes fr)
          exp = (experiment fr)
          comb = sort $ zip sz exp

plotReportHypothesis :: [Int] -> [Double] -> [Double] -> IO ()
plotReportHypothesis sz exp ana = do
  plot X11 [Data2D [Title "exp", Color Red, Style Lines]
            opts
            [(fromIntegral x, y) | (x, y) <- combExp],
            Data2D [Title "ana", Color Blue, Style Lines]
            opts
            [(fromIntegral x, y) | (x, y) <- combAna]]
  return ()
    where combExp = sort $ zip sz exp
          combAna = sort $ zip sz ana
          opts = [Range (fromIntegral (minimum sz - 2)) (fromIntegral (maximum sz + 2))]
