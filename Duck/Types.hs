{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances  #-}
#endif

module Duck.Types where

import Text.Printf (printf)
import Control.Monad (liftM2)
import qualified Test.QuickCheck as QC
import Data.Graph (Graph)
import qualified Data.Graph as Graph
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix

-- Sized data

class Sized t where
   dimSize :: t -> Int

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

-- Cased data

two :: QC.Gen a -> QC.Gen b -> QC.Gen (a, b)
two = liftM2 (,)

class Cased a where
  genCase :: Int -> QC.Gen a

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

type Range = (Int, Int)

data Verbosity = Quiet
               | Moderate
               | Full
               deriving (Eq, Ord)

data Report = Report {confidence :: Double,
                      propConstant :: Double}
              deriving Show

type GroupReport = [(String, Report)]

data FullReport = FullReport {experiment :: [Double],
                              expStd :: [Double],
                              sizes :: [Int],
                              outlierEffect :: Double}

data Parameters = Parameters {range :: Range,
                              iterations :: Int,
                              timePerTest :: Double,
                              verbosity :: Verbosity}

defaultParam = Parameters {range = (10, 100),
                           iterations = 15,
                           timePerTest = 1.0,
                           verbosity = Moderate}
