{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances  #-}
#endif

module Duck.Types where

import Text.Printf (printf)
import qualified Test.QuickCheck as QC
import Data.Graph (Graph)
import qualified Data.Graph as Graph
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix

-- |Class for objects with size.
class Sized t where
   dimSize :: t -> Int -- ^ This function calculates the size of an object.

instance (Sized a) => Sized [a] where
  dimSize arr = sum $ map (dimSize) arr

instance (Sized a, Sized b) => Sized (a, b) where
  dimSize (a, b) = (dimSize a) + (dimSize b)

instance Sized Int where
  dimSize _ = 1

instance Sized Bool where
  dimSize _ = 1

instance Sized Char where
  dimSize _ = 1

instance Sized Graph where
  dimSize g = max (length $ Graph.vertices g) (length $ Graph.edges g)

instance (Num a, Sized a) => Sized (Matrix a) where
  dimSize m = avg (Matrix.nrows m) (Matrix.ncols m)
    where avg a b = (a + b) `div` 2

-- |Class for objects that can be generated with a given size.
class Cased a where
  genCase :: Int -> QC.Gen a  -- ^ This function produces a quickcheck
                              -- generator of an object.

instance Cased () where
  genCase _ = return ()

instance Cased Bool where
  genCase _ = QC.choose (False, True)

instance Cased Int where
  genCase _ = QC.arbitrary

instance Cased Char where
  genCase _ = QC.arbitrary

instance (Cased a, Cased b) => Cased (a, b) where
  genCase sz = two (genCase sz) (genCase sz)

instance (Cased a) => Cased [a] where
  genCase sz = QC.vectorOf (sz) (genCase 1)

instance {-# OVERLAPPING #-} (Cased a, Cased b) => Cased ([a], [b]) where
  genCase sz = two (genCase nvec) (genCase nvec)
    where nsingle = 2
          nvec = sz `div` nsingle

instance {-# OVERLAPPING #-} Cased [String] where
  genCase sz = QC.vectorOf (nvec) (genCase nsingle)
    where nsingle = (floor . sqrt . fromIntegral) sz
          nvec = sz `div` nsingle

instance Cased Graph where
  genCase sz = do
    nvert <- QC.choose (nvavg `div` 2, nvavg * 2)
    let bds = (1, nvert)
    egds <- QC.vectorOf sz (two (QC.choose (1, nvert)) (QC.choose (1, nvert)))
    return (Graph.buildG bds egds)
      where nvavg = max (sz `div` 10) 1

instance (Num a, Cased a) => Cased (Matrix a) where
  genCase sz = do
    ls <- QC.vectorOf nsz (genCase sz)
    return (Matrix.fromList nrow ncol ls)
      where ncol = (floor . sqrt . fromIntegral) sz
            nrow = sz `div` ncol
            nsz = ncol * nrow

-- Other types

-- |A range of integers.
type Range = (Int, Int)

-- |A verbosity function to control the output of functions.
data Verbosity = Quiet
               | Moderate
               | Full
               deriving (Eq, Ord)

-- |The report of a test.
data Report = Report {confidence :: Double,
                      propConstant :: Double}
              deriving Show

-- |The raw report of a test.
data FullReport = FullReport {experiment :: [Double],
                              expStd :: [Double],
                              sizes :: [Int],
                              outlierEffect :: Double}

-- |The raw report of a group of tests.
type GroupReport = [(String, Report)]

-- |The parameters for a test.
data Parameters = Parameters {range :: Range,
                              iterations :: Int,
                              timePerTest :: Double,
                              verbosity :: Verbosity}

-- |The default parameters. 
defaultParam = Parameters {range = (10, 100),
                           iterations = 15,
                           timePerTest = 1.0,
                           verbosity = Moderate}
