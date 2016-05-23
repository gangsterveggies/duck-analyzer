module Duck.Types where

import qualified Test.QuickCheck as QC

class (QC.Arbitrary t) => Testable t where
   dimSize :: t -> Int

instance (Testable a) => Testable [a] where
  dimSize arr = sum $ map (dimSize) arr

instance Testable Int where
  dimSize _ = 1

instance Testable Char where
  dimSize _ = 1

type Range = (Int, Int)
data Report = Report {confidence :: Double,
                      propConstant :: Double}
              deriving Show
data FullReport = FullReport {experiment :: [Double],
                              sizes :: [Int]}
data Test a b = Test {range :: Range,
                      iterations :: Int,
                      source :: (a -> b),
                      types :: a}
