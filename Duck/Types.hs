{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances  #-}
#endif

module Duck.Types where

import Control.Monad (liftM2)
import qualified Test.QuickCheck as QC

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

-- Cased data

class Cased a where
  genCase :: Int -> QC.Gen a
  chooseRange :: a -> Int -> Int
  chooseRange _ _ = 1

instance Cased () where
  genCase _ = return ()

instance Cased Bool where
  genCase _ = QC.choose (False, True)

instance Cased Int where
  genCase _ = QC.arbitrary

instance Cased Char where
  genCase _ = QC.arbitrary

instance (Cased a, Cased b) => Cased (a, b) where
  genCase sz = liftM2 (,) (genCase sz) (genCase sz)

instance (Cased a) => Cased [a] where
  genCase sz = QC.vectorOf (sz) (genCase 1)

instance {-# OVERLAPPING #-} Cased [String] where
  genCase sz = QC.vectorOf (nvec) (genCase nsingle)
    where nsingle = chooseRange (undefined::[String]) sz
          nvec = sz `div` nsingle
  chooseRange _ sz = (floor . sqrt . fromIntegral) sz

-- Other types

type Range = (Int, Int)

data Verbosity = Quiet
               | Full
               deriving Eq

data Report = Report {confidence :: Double,
                      propConstant :: Double}
              deriving Show

data GroupReport = GroupReport {relevantHypothesis :: [(String, Report)]}
              deriving Show

data FullReport = FullReport {experiment :: [Double],
                              expStd :: [Double],
                              sizes :: [Int]}

data Parameters = Parameters {range :: Range,
                              iterations :: Int,
                              timePerTest :: Double,
                              verbosity :: Verbosity}

defaultParam = Parameters {range = (10, 100),
                           iterations = 15,
                           timePerTest = 1.0,
                           verbosity = Quiet}

hypothesis :: String -> (Double -> Double) -> (String, (Double -> Double))
hypothesis name hp = (name, hp)
