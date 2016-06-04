{-# LANGUAGE CPP #-}

module Duck.Utils where

import Debug.Trace
import Control.Monad (liftM2)
import qualified Test.QuickCheck as QC

-- |An helpful debug function. 
traceMonad :: (Show a, Monad m) => String -> a -> m a
traceMonad v x = trace ("Debug: " ++ v ++ " " ++ show x) (return x)

-- |A function that prints under a condition.
putStrIf :: Bool -> String -> IO()
putStrIf False _ = return()
putStrIf True str = putStrLn str

-- |Combines two generators in a pair.
two :: QC.Gen a -> QC.Gen b -> QC.Gen (a, b)
two = liftM2 (,)

-- |Provides an string explanation for outlier effects.
outlierReport :: Double -> String
outlierReport ot
  | ot > 0.5  = "critically random"
  | ot > 0.25 = "very random"
  | ot > 0.1  = "moderately random"
  | ot > 0.05 = "slightly random"
  | otherwise = "not random"
