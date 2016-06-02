module Duck.Utils where

import Debug.Trace

traceMonad :: (Show a, Monad m) => String -> a -> m a
traceMonad v x = trace ("Debug: " ++ v ++ " " ++ show x) (return x)

putStrIf :: Bool -> String -> IO()
putStrIf False _ = return()
putStrIf True str = putStrLn str

chi2test :: [Double] -> [Double] -> [Double] -> Double
chi2test exp std ana = sum $ map (\(x, sg, mu) -> ((x - mu) / sg)^2) (zip3 exp std ana)
