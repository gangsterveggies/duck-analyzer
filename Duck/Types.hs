module Duck.Types where

import qualified Test.QuickCheck as QC

class Sized t where
   dimSize :: t -> Int

instance (Sized a) => Sized [a] where
  dimSize arr = sum $ map (dimSize) arr

instance (Sized a, Sized b) => Sized (a, b) where
  dimSize (a, b) = (dimSize a) + (dimSize b)

instance Sized Int where
  dimSize _ = 1

instance Sized Char where
  dimSize _ = 1

type Range = (Int, Int)

data Verbosity = Quiet
               | Full
               deriving Eq

data Report = Report {confidence :: Double,
                      propConstant :: Double}
              deriving Show

data FullReport = FullReport {experiment :: [Double],
                              sizes :: [Int]}

data Parameters = Parameters {range :: Range,
                              iterations :: Int,
                              timePerTest :: Double,
                              verbosity :: Verbosity}

defaultParam = Parameters {range = (10, 100),
                           iterations = 15,
                           timePerTest = 1.0,
                           verbosity = Quiet}
