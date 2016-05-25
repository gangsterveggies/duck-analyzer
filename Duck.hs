{-|
Module      : Duck
Description : An experimental complexity tester
Copyright   : (c) Pedro Paredes, 2016
Maintainer  : gangsterveggies@gmail.com
Stability   : experimental

Duck is a library to test the complexity of programs.

The programmer provides one program and one or many complexity
functions that will be hypothesis for the theoretical behavior of the
program. Duck then runs the program multiple times for multiple
instances and compares the theoretical hypothesis with the
experimental data, allowing to determing the complexity closest to the
actual one.
-}

module Duck
       (
         -- * Typeclasses and Types for testing
         Sized,
         Parameters(..),
         Verbosity(..),
         Report(..),
         FullReport(..),
         -- * Running functions
         runSingleTest,
         runSingleTest',
         plotReport,
         testHypothesis,
         -- * Defaults
         defaultParam
       ) where

import Duck.Types
import Duck.Plot
import Duck.Analysis
