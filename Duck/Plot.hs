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
